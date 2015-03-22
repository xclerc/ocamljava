(*
 * This file is part of OCaml-Java compiler.
 * Copyright (C) 2007-2015 Xavier Clerc.
 * Original file (asmcomp/closure.ml in the OCaml source
 * distribution) is Copyright (C) INRIA.
 *
 * OCaml-Java compiler is free software; you can redistribute it and/or modify
 * it under the terms of the Q Public License as published by
 * Trolltech (with a change to choice of law).
 *
 * OCaml-Java compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Q Public License for more details.
 *
 * You should have received a copy of the Q Public License
 * along with this program.  If not, see
 * <http://opensource.org/licenses/QPL-1.0>.
 *)

open Asttypes
open Lambda
open Jlambda
open BaristaLibrary
open Jclosure_arrays
open Jclosure_exceptions

type error =
    Special_primitive_first_argument

exception Error of error

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> Misc.fatal_error "Jclosure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec build_closure_env env_param pos = function
    [] -> Tbl.empty
  | id :: rem ->
      Tbl.add id (Jprim(Pfield pos, [Jvar env_param], Debuginfo.none))
              (build_closure_env env_param (pos+1) rem)

(* Auxiliary for accessing globals.  We change the name of the global
   to the name of the corresponding asm symbol.  This is done here so
   that approximations stored in .cmj files contain the right names if
   the -for-pack option is active. *)

let getglobal id =
  Jprim(Pgetglobal (Ident.create_persistent (Jcompilenv.symbol_for_global id)),
        [], Debuginfo.none)

(* Check if a variable occurs in a [jlambda] term. *)

let occurs_var var j =
  let rec occurs = function
      Jvar v -> Ident.same v var
    | Jconst _cst -> false
    | Jdirect_apply(_lbl, args, _, _, _) -> List.exists occurs args
    | Jgeneric_apply(funct, args, _) -> occurs funct || List.exists occurs args
    | Jclosure(_fundecls, clos) -> List.exists occurs clos
    | Joffset(j, _) -> occurs j
    | Jlet(_id, def, body) -> occurs def || occurs body
    | Jletrec(decls, body) ->
        List.exists (fun (_id, j) -> occurs j) decls || occurs body
    | Jprim(_p, args, _) -> List.exists occurs args
    | Jjavaprim(_p, args, _) -> List.exists occurs args
    | Jswitch(arg, s) ->
        occurs arg ||
        occurs_array s.js_actions
    | Jstaticfail (_, args) -> List.exists occurs args
    | Jstaticcatch(_, _, body, hdlr) -> occurs body || occurs hdlr
    | Jtrywith(body, _exn, hdlr) -> occurs body || occurs hdlr
    | Jifthenelse(cond, ifso, ifnot) ->
        occurs cond || occurs ifso || occurs ifnot
    | Jsequence(j1, j2) -> occurs j1 || occurs j2
    | Jwhile(cond, body, _) -> occurs cond || occurs body
    | Jfor(_id, lo, hi, _dir, body, _) -> occurs lo || occurs hi || occurs body
    | Jassign(id, j) -> Ident.same id var || occurs j
    | Jsend(_, met, obj, args, _) ->
        occurs met || occurs obj || List.exists occurs args
    | Jnop -> false
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs j

(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
    Pidentity -> 0
  | Pgetglobal _id -> 1
  | Psetglobal _id -> 1
  | Pmakeblock(_tag, _mut) -> 5 + List.length args
  | Pfield _f -> 1
  | Psetfield (_f, isptr) -> if isptr then 4 else 1
  | Pfloatfield _f -> 1
  | Psetfloatfield _f -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.Primitive.prim_alloc then 10 else 4) + List.length args
  | Praise -> 4
  | Pstringlength -> 5
  | Pstringrefs | Pstringsets -> 6
  | Pmakearray _kind -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbittest -> 3
  | Pbigarrayref(_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset(_, ndims, _, _) -> 4 + ndims * 6
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of switch cost *)

let lambda_size_threshold lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Jvar _ -> ()
    | Jconst (Lambda_const(
        (Const_base(Asttypes.Const_int _ | Const_char _ | Const_float _ |
                        Const_int32 _ | Const_int64 _ | Const_nativeint _) |
             Const_pointer _))) -> incr size
    | Jconst (Const_targetint _ | Const_null _ | Const_javastring _) ->
        incr size
    | Jconst _ ->
        raise Exit (* avoid duplication of structured constants *)
    | Jdirect_apply(_fn, args, _, _, _) ->
        size := !size + 4; lambda_list_size args
    | Jgeneric_apply(fn, args, _) ->
        size := !size + 6; lambda_size fn; lambda_list_size args
    | Jclosure(_defs, _vars) ->
        raise Exit (* inlining would duplicate function definitions *)
    | Joffset(lam, _) ->
        incr size; lambda_size lam
    | Jlet(_id, lam, body) ->
        lambda_size lam; lambda_size body
    | Jletrec(_bindings, _body) ->
        raise Exit (* usually too large *)
    | Jprim(prim, args, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Jjavaprim(_prim, args, _) ->
        size := !size + 4;
        lambda_list_size args
    | Jswitch(lam, cases) ->
        if Array.length cases.js_actions > 1 then size := !size + 5 ;
        lambda_size lam;
        lambda_array_size cases.js_actions
    | Jstaticfail (_, args) -> lambda_list_size args
    | Jstaticcatch(_, _, body, handler) ->
        incr size; lambda_size body; lambda_size handler
    | Jtrywith (body, _id, handler) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Jifthenelse(cond, ifso, ifnot) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Jsequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | Jwhile(cond, body, Some times) ->
        size := !size + 2;
        for _i = 1 to times do
          lambda_size cond; lambda_size body
        done
    | Jwhile(cond, body, None) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Jfor(_id, low, high, _dir, body, Some times) ->
        size := !size + 4; lambda_size low; lambda_size high;
        for _i = 1 to times do
          lambda_size body
        done
    | Jfor(_id, low, high, _dir, body, None) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Jassign(_id, lam) ->
        incr size;  lambda_size lam
    | Jsend(_, met, obj , args, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
    | Jnop -> ()
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter lambda_size a in
  lambda_size lam;
  !size

let lambda_smaller lam threshold =
  try
    lambda_size_threshold lam threshold <= threshold
  with Exit ->
    false

let lambda_size lam =
  try
    lambda_size_threshold lam 1_000_000
  with Exit ->
    1_000_000

(* Check if a clambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure_jlambda = function
    Jvar _ -> true
  | Jconst _ -> true
  | Jprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _, _) -> false
  | Jprim(_p, args, _) -> List.for_all is_pure_jlambda args
  | Jnop -> true
  | _ -> false

(* Simplify primitive operations on integers *)

let make_const_int n = (Jconst(Const_targetint n)), (Value_integer n)
let make_const_int32 n = (Jconst(Lambda_const(Const_base(Const_int32 n)))), (Value_integer32 n)
let make_const_int64 n = (Jconst(Lambda_const(Const_base(Const_int64 n)))), (Value_integer64 n)
let make_const_intnat n = (Jconst(Lambda_const(Const_base(Const_nativeint n)))), (Value_integernat n)
let make_const_float f = (Jconst(Lambda_const(Const_base(Const_float (string_of_float f))))), (Value_float f)
let make_const_ptr n = (Jconst(Lambda_const(Const_pointer n))), (Value_constptr n)
let make_const_bool b = make_const_ptr(if b then 1 else 0)
let make_const_null jt = (Jconst(Const_null jt)), (Value_java_null jt)
let make_const_string s = (Jconst(Const_javastring s)), (Value_java_string s)
let make_comparison cmp (x: int) (y: int) =
  make_const_bool
    (match cmp with
       Ceq -> x = y
     | Cneq -> x <> y
     | Clt -> x < y
     | Cgt -> x > y
     | Cle -> x <= y
     | Cge -> x >= y)

let return_repr_of_primitive = function
  | Pccall prim_desc ->
      let prim_name = prim_desc.Primitive.prim_name in
      let prim_desc = Runtimeprimitives.get_description prim_name in
      Some prim_desc.Runtimeprimitives.primdesc_return
  | _ ->  None

external bswap_int32 : int32 -> int32 = "%bswap_int32"

external bswap_int64 : int64 -> int64 = "%bswap_int64"

external bswap_intnat : nativeint -> nativeint = "%bswap_native"

let simplif_prim_pure p (args, approxs) dbg =
  let unknown =
    Jprim (p, args, dbg),
    Value_unknown (return_repr_of_primitive p) in
  let comp x y op =
    let res =
      match op with
      | Ceq -> x = y
      | Cneq -> x <> y
      | Clt -> x < y
      | Cgt -> x > y
      | Cle -> x <= y
      | Cge -> x >= y in
    make_const_bool res in
  match approxs with
    [Value_integer x] ->
      begin match p with
        Pidentity -> make_const_int x
      | Pnegint -> make_const_int (Targetint.neg x)
      | Pbswap16 -> make_const_int (Targetint.bswap16 x)
      | Poffsetint y -> make_const_int (Targetint.add x (Targetint.of_int y))
      | Pbintofint Pint32 -> make_const_int32 (Targetint.to_int32 x)
      | Pbintofint Pint64 -> make_const_int64 (Targetint.to_int64 x)
      | Pbintofint Pnativeint -> make_const_intnat (Targetint.to_nativeint x)
      | Pfloatofint when !Jclflags.opt_floats -> make_const_float (Targetint.to_float x)
      | _ -> unknown
      end
  | [Value_integer32 x] ->
      begin match p with
        Pidentity -> make_const_int32 x
      | Pnegbint Pint32 -> make_const_int32 (Int32.neg x)
      | Pbbswap Pint32 ->  make_const_int32 (bswap_int32 x)
      | Pintofbint Pint32 -> make_const_int (Targetint.of_int32 x)
      | Pcvtbint (Pint32, Pint64) -> make_const_int64 (Int64.of_int32 x)
      | Pcvtbint (Pint32, Pnativeint) -> make_const_intnat (Nativeint.of_int32 x)
      | _ -> unknown
      end
  | [Value_integer64 x] ->
      begin match p with
        Pidentity -> make_const_int64 x
      | Pnegbint Pint64 -> make_const_int64 (Int64.neg x)
      | Pbbswap Pint64 ->  make_const_int64 (bswap_int64 x)
      | Pintofbint Pint64 -> make_const_int (Targetint.of_int64 x)
      | Pcvtbint (Pint64, Pint32) -> make_const_int32 (Int64.to_int32 x)
      | Pcvtbint (Pint64, Pnativeint) -> make_const_intnat (Int64.to_nativeint x)
      | _ -> unknown
      end
  | [Value_integernat x] ->
      begin match p with
        Pidentity -> make_const_intnat x
      | Pnegbint Pnativeint -> make_const_intnat (Nativeint.neg x)
      | Pbbswap Pint64 ->  make_const_intnat (bswap_intnat x)
      | Pintofbint Pnativeint -> make_const_int (Targetint.of_nativeint x)
      | Pcvtbint (Pnativeint, Pint32) -> make_const_int32 (Nativeint.to_int32 x)
      | Pcvtbint (Pnativeint, Pint64) -> make_const_int64 (Int64.of_nativeint x)
      | _ -> unknown
      end
  | [Value_float x] when !Jclflags.opt_floats ->
      begin match p with
        Pidentity -> make_const_float x
      | Pintoffloat -> make_const_int (Targetint.of_float x)
      | Pnegfloat -> make_const_float (~-. x)
      | Pabsfloat -> make_const_float (abs_float x)
      | _ -> unknown
      end
  | [Value_integer x; Value_integer y] ->
      begin match p with
        Paddint -> make_const_int(Targetint.add x y)
      | Psubint -> make_const_int(Targetint.sub x y)
      | Pmulint -> make_const_int(Targetint.mul x y)
      | Pdivint when not Targetint.(equal zero y) -> make_const_int(Targetint.div x y)
      | Pmodint when not Targetint.(equal zero y) -> make_const_int(Targetint.rem x y)
      | Pandint -> make_const_int(Targetint.logand x y)
      | Porint -> make_const_int(Targetint.logor x y)
      | Pxorint -> make_const_int(Targetint.logxor x y)
      | Plslint -> make_const_int(Targetint.shift_left x y)
      | Plsrint -> make_const_int(Targetint.shift_right_logical x y)
      | Pasrint -> make_const_int(Targetint.shift_right x y)
      | Pintcomp cmp -> comp x y cmp
      | _ -> unknown
      end
  | [Value_integer32 x; Value_integer32 y] ->
      begin match p with
      | Paddbint Pint32 -> make_const_int32(Int32.add x y)
      | Psubbint Pint32 -> make_const_int32(Int32.sub x y)
      | Pmulbint Pint32 -> make_const_int32(Int32.mul x y)
      | Pdivbint Pint32 when y <> 0l -> make_const_int32(Int32.div x y)
      | Pmodbint Pint32 when y <> 0l -> make_const_int32(Int32.rem x y)
      | Pandbint Pint32 -> make_const_int32(Int32.logand x y)
      | Porbint Pint32 -> make_const_int32(Int32.logor x y)
      | Pxorbint Pint32 -> make_const_int32(Int32.logxor x y)
      | Pbintcomp (Pint32, cmp) -> comp x y cmp
      | _ -> unknown
      end
  | [Value_integer32 x; Value_integer y] ->
      begin match p with
      | Plslbint Pint32 -> make_const_int32(Int32.shift_left x (Targetint.to_int y))
      | Plsrbint Pint32 -> make_const_int32(Int32.shift_right_logical x (Targetint.to_int y))
      | Pasrbint Pint32 -> make_const_int32(Int32.shift_right x (Targetint.to_int y))
      | _ -> unknown
      end
  | [Value_integer64 x; Value_integer64 y] ->
      begin match p with
      | Paddbint Pint64 -> make_const_int64(Int64.add x y)
      | Psubbint Pint64 -> make_const_int64(Int64.sub x y)
      | Pmulbint Pint64 -> make_const_int64(Int64.mul x y)
      | Pdivbint Pint64 when y <> 0L -> make_const_int64(Int64.div x y)
      | Pmodbint Pint64 when y <> 0L -> make_const_int64(Int64.rem x y)
      | Pandbint Pint64 -> make_const_int64(Int64.logand x y)
      | Porbint Pint64 -> make_const_int64(Int64.logor x y)
      | Pxorbint Pint64 -> make_const_int64(Int64.logxor x y)
      | Pbintcomp (Pint64, cmp) -> comp x y cmp
      | _ -> unknown
      end
  | [Value_integer64 x; Value_integer y] ->
      begin match p with
      | Plslbint Pint64 -> make_const_int64(Int64.shift_left x (Targetint.to_int y))
      | Plsrbint Pint64 -> make_const_int64(Int64.shift_right_logical x (Targetint.to_int y))
      | Pasrbint Pint64 -> make_const_int64(Int64.shift_right x (Targetint.to_int y))
      | _ -> unknown
      end
  | [Value_integernat x; Value_integernat y] ->
      begin match p with
      | Paddbint Pnativeint -> make_const_intnat(Nativeint.add x y)
      | Psubbint Pnativeint -> make_const_intnat(Nativeint.sub x y)
      | Pmulbint Pnativeint -> make_const_intnat(Nativeint.mul x y)
      | Pdivbint Pnativeint when y <> 0n -> make_const_intnat(Nativeint.div x y)
      | Pmodbint Pnativeint when y <> 0n -> make_const_intnat(Nativeint.rem x y)
      | Pandbint Pnativeint -> make_const_intnat(Nativeint.logand x y)
      | Porbint Pnativeint -> make_const_intnat(Nativeint.logor x y)
      | Pxorbint Pnativeint -> make_const_intnat(Nativeint.logxor x y)
      | Pbintcomp (Pnativeint, cmp) -> comp x y cmp
      | _ -> unknown
      end
  | [Value_integernat x; Value_integer y] ->
      begin match p with
      | Plslbint Pnativeint -> make_const_intnat(Nativeint.shift_left x (Targetint.to_int y))
      | Plsrbint Pnativeint -> make_const_intnat(Nativeint.shift_right_logical x (Targetint.to_int y))
      | Pasrbint Pnativeint -> make_const_intnat(Nativeint.shift_right x (Targetint.to_int y))
      | _ -> unknown
      end
  | [Value_float x; Value_float y] when !Jclflags.opt_floats ->
      begin match p with
        Paddfloat -> make_const_float(x +. y)
      | Psubfloat -> make_const_float(x -. y)
      | Pmulfloat -> make_const_float(x *. y)
      | Pdivfloat -> make_const_float(x /. y)
      | Pfloatcomp cmp -> comp x y cmp
      | _ -> unknown
      end
  | [Value_constptr x] ->
      begin match p with
        Pidentity -> make_const_ptr x
      | Pnot -> make_const_bool(x = 0)
      | Pisint -> make_const_bool true
      | Pctconst c ->
          begin
            match c with
            | Big_endian -> make_const_bool true
            | Word_size -> make_const_int (Targetint.of_int 64)
            | Ostype_unix -> make_const_bool (Sys.os_type = "Unix")
            | Ostype_win32 -> make_const_bool (Sys.os_type = "Win32")
            | Ostype_cygwin -> make_const_bool (Sys.os_type = "Cygwin")
          end
      | _ -> unknown
      end
  | [Value_constptr x; Value_constptr y] ->
      begin match p with
        Psequand -> make_const_bool(x <> 0 && y <> 0)
      | Psequor  -> make_const_bool(x <> 0 || y <> 0)
      | Pintcomp cmp -> make_comparison cmp x y
      | _ -> unknown
      end
  | [Value_constptr x; Value_integer y] ->
      begin match p with
      | Pintcomp cmp -> comp (Targetint.of_int x) y cmp
      | _ -> unknown
      end
  | [Value_integer x; Value_constptr y] ->
      begin match p with
      | Pintcomp cmp -> comp x (Targetint.of_int y) cmp
      | _ -> unknown
      end
  | _ ->
      unknown

let simplif_prim p (args, _ as args_approxs) dbg =
  if List.for_all is_pure_jlambda args
  then simplif_prim_pure p args_approxs dbg
  else (Jprim (p, args, dbg)), Value_unknown (return_repr_of_primitive p)

(* Substitute variables in a [ulambda] term (a body of an inlined function)
   and perform some more simplifications on integer primitives.
   Also perform alpha-conversion on let-bound identifiers to avoid
   clashes with locally-generated identifiers.
   The variables must not be assigned in the term.
   This is used to substitute "trivial" arguments for parameters
   during inline expansion, and also for the translation of let rec
   over functions. *)

let approx_jlam = function
    Jconst (Lambda_const (Const_base (Const_int n))) -> Value_integer (Targetint.of_int n)
  | Jconst (Lambda_const (Const_base (Const_char c))) -> Value_integer (Targetint.of_int (Char.code c))
  | Jconst (Lambda_const (Const_base (Const_float s))) when !Jclflags.opt_floats -> Value_float (float_of_string s)
  | Jconst (Lambda_const (Const_base (Const_int32 n))) -> Value_integer32 n
  | Jconst (Lambda_const (Const_base (Const_int64 n))) -> Value_integer64 n
  | Jconst (Lambda_const (Const_base (Const_nativeint n))) -> Value_integernat n
  | Jconst (Lambda_const (Const_pointer n)) -> Value_constptr n
  | _ -> Value_unknown None

let rec substitute sb jlam =
  match jlam with
  | Jvar v ->
      begin try Tbl.find v sb with Not_found -> jlam end
  | Jconst _ -> jlam
  | Jdirect_apply(lbl, args, params, return, dbg) ->
      Jdirect_apply(lbl, List.map (substitute sb) args, params, return, dbg)
  | Jgeneric_apply(fn, args, dbg) ->
      Jgeneric_apply(substitute sb fn, List.map (substitute sb) args, dbg)
  | Jclosure(defs, env) ->
      Jclosure(defs, List.map (substitute sb) env)
  | Joffset(j, ofs) -> Joffset(substitute sb j, ofs)
  | Jlet(id, j1, j2) ->
      let id' = Ident.rename id in
      Jlet(id', substitute sb j1, substitute (Tbl.add id (Jvar id') sb) j2)
  | Jletrec(bindings, body) ->
      let bindings1 =
        List.map (fun (id, rhs) -> (id, Ident.rename id, rhs)) bindings in
      let sb' =
        List.fold_right
          (fun (id, id', _) s -> Tbl.add id (Jvar id') s)
          bindings1 sb in
      Jletrec(
        List.map (fun (_id, id', rhs) -> (id', substitute sb' rhs)) bindings1,
        substitute sb' body)
  | Jprim(p, args, dbg) ->
      let sargs = List.map (substitute sb) args in
      let (res, _) = simplif_prim p (sargs, List.map approx_jlam sargs) dbg in
      res
  | Jjavaprim(jp, args, dbg) ->
      Jjavaprim (jp, List.map (substitute sb) args, dbg)
  | Jswitch(arg, sw) ->
      Jswitch(substitute sb arg,
              { sw with
                js_actions =
                  Array.map (substitute sb) sw.js_actions;
               })
  | Jstaticfail (nfail, args) ->
      Jstaticfail (nfail, List.map (substitute sb) args)
  | Jstaticcatch(nfail, ids, j1, j2) ->
      Jstaticcatch(nfail, ids, substitute sb j1, substitute sb j2)
  | Jtrywith(j1, id, j2) ->
      let id' = Ident.rename id in
      Jtrywith(substitute sb j1, id', substitute (Tbl.add id (Jvar id') sb) j2)
  | Jifthenelse(j1, j2, j3) ->
      begin match substitute sb j1 with
        Jconst(Lambda_const (Const_pointer n)) ->
          if n <> 0 then substitute sb j2 else substitute sb j3
      | sj1 ->
          Jifthenelse(sj1, substitute sb j2, substitute sb j3)
      end
  | Jsequence(j1, j2) -> Jsequence(substitute sb j1, substitute sb j2)
  | Jwhile(j1, j2, lii) -> Jwhile(substitute sb j1, substitute sb j2, lii)
  | Jfor(id, j1, j2, dir, j3, lii) ->
      let id' = Ident.rename id in
      Jfor(id', substitute sb j1, substitute sb j2, dir,
           substitute (Tbl.add id (Jvar id') sb) j3, lii)
  | Jassign(id, j) ->
      let id' =
        try
          match Tbl.find id sb with Jvar i -> i | _ -> assert false
        with Not_found ->
          id in
      Jassign(id', substitute sb j)
  | Jsend(k, j1, j2, jl, dbg) ->
      Jsend(k, substitute sb j1, substitute sb j2, List.map (substitute sb) jl,
            dbg)
  | Jnop -> Jnop

(* Perform an inline expansion *)

let is_simple_argument = function
    Jvar _ -> true
  | Jconst(Lambda_const (Const_base (Const_int _ | Const_char _ | Const_float _ |
                                     Const_int32 _ | Const_int64 _ | Const_nativeint _))) ->
      true
  | Jconst(Lambda_const (Const_pointer _)) -> true
  | Jconst(Const_null _) -> true
  | Jconst(Const_javastring _) -> true
  | _ -> false

let no_effects = function
    Jclosure _ -> true
  | Jconst(Lambda_const (Const_base (Const_string _))) -> true
  | j -> is_simple_argument j

let rec bind_params_rec subst params args body =
  match (params, args) with
    ([], []) -> substitute subst body
  | (p1 :: pl, a1 :: al) ->
      if is_simple_argument a1 then
        bind_params_rec (Tbl.add p1 a1 subst) pl al body
      else begin
        let p1' = Ident.rename p1 in
        let body' =
          bind_params_rec (Tbl.add p1 (Jvar p1') subst) pl al body in
        if occurs_var p1 body then Jlet(p1', a1, body')
        else if no_effects a1 then body'
        else Jsequence(a1, body')
      end
  | _ -> assert false

let bind_params params args body =
  (* Reverse parameters and arguments to preserve right-to-left
     evaluation order (PR#2910). *)
  bind_params_rec Tbl.empty (List.rev params) (List.rev args) body

(* Check if a lambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure = function
    Lvar _ -> true
  | Lconst _ -> true
  | Lprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _) -> false
  | Lprim(_p, args) -> List.for_all (fun (x, _) -> is_pure x) args
  | Levent(lam, _ev) -> is_pure lam
  | _ -> false

(* Generate a direct application *)

let direct_apply fundesc funct jfunct jargs types =
  let fundesc =
    if fundesc.fun_closed then optimize_intarray_call fundesc types else fundesc in
  let app_args =
    if fundesc.fun_closed then jargs else jargs @ [jfunct] in
  let app =
    match fundesc.fun_inline with
    | None ->
        let params =
          if fundesc.fun_closed then
            fundesc.fun_repr_parameters
          else
            fundesc.fun_repr_parameters @ [LR_value] in
        let return = fundesc.fun_repr_return in
        Jdirect_apply(fundesc.fun_label, app_args, params, return, Debuginfo.none)
    | Some(params, body) -> bind_params params app_args body in
  (* If ufunct can contain side-effects or function definitions,
     we must make sure that it is evaluated exactly once.
     If the function is not closed, we evaluate ufunct as part of the
     arguments.
     If the function is closed, we force the evaluation of ufunct first. *)
  if not fundesc.fun_closed || is_pure funct
  then app
  else Jsequence(jfunct, app)

(* Add [Value_xyz] (where xyz designates a scalar constant) info to the approximation
   of an application *)

let strengthen_approx appl approx =
  match approx_jlam appl with
    (Value_integer _ | Value_integer32 _ | Value_integer64 _
    | Value_integernat _ | Value_constptr _
    | Value_java_null _ | Value_java_string _) as appr -> appr
  | (Value_float _) as appr when !Jclflags.opt_floats -> appr
  | _ -> approx

(* If a term has approximation Value_integer or Value_constptr and is pure,
   replace it by an integer constant *)

let check_constant_result lam jlam approx =
  match approx with
    Value_integer n when is_pure lam -> make_const_int n
  | Value_integer32 n when is_pure lam -> make_const_int32 n
  | Value_integer64 n when is_pure lam -> make_const_int64 n
  | Value_integernat n when is_pure lam -> make_const_intnat n
  | Value_constptr n when is_pure lam -> make_const_ptr n
  | Value_float s when !Jclflags.opt_floats && is_pure lam -> make_const_float s
  | Value_java_null jt when is_pure lam -> make_const_null jt
  | Value_java_string s when is_pure lam -> make_const_string s
  | _ -> (jlam, approx)

(* Evaluate an expression with known value for its side effects only,
   or discard it if it's pure *)

let sequence_constant_expr lam jlam1 (jlam2, approx2 as res2) =
  if is_pure lam then res2 else (Jsequence(jlam1, jlam2), approx2)

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Maintain the nesting depth for functions *)

let function_nesting_depth = ref 0
let excessive_function_nesting_depth = 5

(* Decorate clambda term with debug information *)

let rec add_debug_info ev j =
  match ev.lev_kind with
  | Lev_after _ ->
      begin match j with
      | Jdirect_apply(lbl, args, params, return, _dinfo) ->
          Jdirect_apply(lbl, args, params, return, Debuginfo.from_call ev)
      | Jgeneric_apply(Jdirect_apply(lbl, args1, params, return, _dinfo1),
                       args2, _dinfo2) ->
          Jgeneric_apply(Jdirect_apply(lbl, args1, params, return, Debuginfo.from_call ev),
                          args2, Debuginfo.from_call ev)
      | Jgeneric_apply(fn, args, _dinfo) ->
          Jgeneric_apply(fn, args, Debuginfo.from_call ev)
      | Jprim(Praise, args, _dinfo) ->
          Jprim(Praise, args, Debuginfo.from_call ev)
      | Jprim(p, args, _dinfo) ->
          Jprim(p, args, Debuginfo.from_call ev)
      | Jsend(kind, j1, j2, args, _dinfo) ->
          Jsend(kind, j1, j2, args, Debuginfo.from_call ev)
      | Jsequence(j1, j2) ->
          Jsequence(j1, add_debug_info ev j2)
      | _ -> j
      end
  | _ -> j

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

let close_approx_var fenv cenv id =
  let approx = try Tbl.find id fenv with Not_found -> Value_unknown None in
  match approx with
    Value_integer n ->
      make_const_int n
  | Value_integer32 n ->
      make_const_int32 n
  | Value_integer64 n ->
      make_const_int64 n
  | Value_integernat n ->
      make_const_intnat n
  | Value_constptr n ->
      make_const_ptr n
  | Value_float s when !Jclflags.opt_floats ->
      make_const_float s
  | Value_java_null jt ->
      make_const_null jt
  | Value_java_string s ->
      make_const_string s
  | approx ->
      let subst = try Tbl.find id cenv with Not_found -> Jvar id in
      (subst, approx)

let close_var fenv cenv id =
  let (jlam, _app) = close_approx_var fenv cenv id in jlam

let rec close fenv cenv = function
    Lvar id ->
      close_approx_var fenv cenv id
  | Lconst cst ->
      begin match cst with
        Const_base(Const_int n) -> (Jconst(Lambda_const cst), Value_integer(Targetint.of_int n))
      | Const_base(Const_char c) -> (Jconst(Lambda_const cst), Value_integer(Targetint.of_int (Char.code c)))
      | Const_base(Const_float s) when !Jclflags.opt_floats -> (Jconst(Lambda_const cst), Value_float (float_of_string s))
      | Const_base(Const_int32 n) -> (Jconst(Lambda_const cst), Value_integer32 n)
      | Const_base(Const_int64 n) -> (Jconst(Lambda_const cst), Value_integer64 n)
      | Const_base(Const_nativeint n) -> (Jconst(Lambda_const cst), Value_integernat n)
      | Const_pointer n -> (Jconst(Lambda_const cst), Value_constptr n)
      | _ -> (Jconst (Lambda_const cst), Value_unknown None)
      end
  | Lfunction(_kind, _params, _return, _body) as funct ->
      close_one_function fenv cenv (Ident.create "fun") funct

    (* We convert [f a] to [let a' = a in fun b c -> f a' b c]
       when fun_arity > nargs *)
  | Lapply(funct, args, loc) ->
      let nargs = List.length args in
      begin match (close fenv cenv funct, close_list fenv cenv args) with
        ((jfunct, Value_closure (fundesc, approx_res)),
         [Jprim(Pmakeblock(_, _), jargs, _)])
        when List.length jargs = - fundesc.fun_arity ->
          let app = direct_apply fundesc funct jfunct jargs (List.map snd args) in
          (app, strengthen_approx app approx_res)
      | ((jfunct, Value_closure (fundesc, approx_res)), jargs)
        when nargs = fundesc.fun_arity ->
          let app = direct_apply fundesc funct jfunct jargs (List.map snd args) in
          (app, strengthen_approx app approx_res)

      | ((_jfunct, Value_closure(fundesc, _approx_res)), jargs)
          when nargs < fundesc.fun_arity ->
        let first_args = List.map (fun arg ->
          (Ident.create "arg", arg)) jargs in
        let final_args =
          Array.to_list (Array.init (fundesc.fun_arity - nargs)
                                    (fun _ -> Ident.create "arg")) in
        let rec iter args body =
          match args with
              [] -> body
            | (arg1, arg2) :: args ->
              iter args
                (Jlet ( arg1, arg2, body))
        in
        let internal_args =
          (List.map (fun (arg1, _arg) -> Lvar arg1) first_args)
          @ (List.map (fun arg -> Lvar arg ) final_args)
        in
        let (new_fun, approx) = close fenv cenv
          (Lfunction(
           Curried,
           List.map (fun x -> x, LR_value) final_args,
           LR_value,
           Lapply(funct, List.map (fun x -> x, None) internal_args, loc)))
        in
        let new_fun = iter first_args new_fun in
        (new_fun, approx)

      | ((jfunct, Value_closure(fundesc, _approx_res)), jargs)
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity ->
          let (first_args, rem_args) = split_list fundesc.fun_arity jargs in
          (Jgeneric_apply(direct_apply fundesc funct jfunct first_args (List.map snd args),
                          rem_args, Debuginfo.none),
           Value_unknown None)
      | ((jfunct, _), jargs) ->
          (Jgeneric_apply(jfunct, jargs, Debuginfo.none), Value_unknown None)
      end
  | Lsend(kind, met, obj, args, _) ->
      let (jmet, _) = close fenv cenv met in
      let (jobj, _) = close fenv cenv obj in
      let args = List.map (fun x -> x, None) args in
      (Jsend(kind, jmet, jobj, close_list fenv cenv args, Debuginfo.none),
       Value_unknown None)
  | Llet(_, id,
         Lconst (Const_base (Const_string s) | Const_immstring s),
         Lprim(Pccall { Primitive.prim_name = "ocamljava_javastring_of_string"; _ },
               [Lvar id', _])) when Ident.same id id' ->
      make_const_string s
  | Llet(str, id, lam, body) ->
      let (jlam, alam) = close_named fenv cenv id lam in
      begin match (str, alam) with
        (Variable, _) ->
          let (jbody, abody) = close fenv cenv body in
          (Jlet(id, jlam, jbody), abody)
      | (_, (Value_integer _ | Value_integer32 _ | Value_integer64 _ | Value_integernat _ | Value_constptr _ | Value_java_null _ | Value_java_string _))
        when str = Alias || is_pure lam ->
          close (Tbl.add id alam fenv) cenv body
      | (_, Value_float _)
        when !Jclflags.opt_floats && (str = Alias || is_pure lam) ->
          close (Tbl.add id alam fenv) cenv body
      | (_, _) ->
          let (jbody, abody) = close (Tbl.add id alam fenv) cenv body in
          (Jlet(id, jlam, jbody), abody)
      end
  | Lletrec(defs, body) ->
      if List.for_all
           (function (_id, Lfunction (_, _, _, _)) -> true | _ -> false)
           defs
      then begin
        (* Simple case: only function definitions *)
        let (clos, infos) = close_functions fenv cenv defs in
        let clos_ident = Ident.create "clos" in
        let fenv_body =
          List.fold_right
            (fun (id, _pos, approx) fenv -> Tbl.add id approx fenv)
            infos fenv in
        let (jbody, approx) = close fenv_body cenv body in
        let sb =
          List.fold_right
            (fun (id, pos, _approx) sb ->
              Tbl.add id (Joffset(Jvar clos_ident, pos)) sb)
            infos Tbl.empty in
        (Jlet(clos_ident, clos, substitute sb jbody),
         approx)
      end else begin
        (* General case: recursive definition of values *)
        let rec clos_defs = function
          [] -> ([], fenv)
        | (id, lam) :: rem ->
            let (jdefs, fenv_body) = clos_defs rem in
            let (jlam, approx) = close fenv cenv lam in
            ((id, jlam) :: jdefs, Tbl.add id approx fenv_body) in
        let (jdefs, fenv_body) = clos_defs defs in
        let (jbody, approx) = close fenv_body cenv body in
        (Jletrec(jdefs, jbody), approx)
      end
  | Lprim(Pidentity, [arg,_]) ->
      close fenv cenv arg
  | Lprim(Pdirapply loc,[funct,_;arg])
  | Lprim(Prevapply loc,[arg;funct,_]) ->
      close fenv cenv (Lapply(funct, [arg], loc))
  | Lprim(Pgetglobal id, []) as lam ->
      check_constant_result lam
                            (getglobal id)
                            (Jcompilenv.global_approx id)
  | Lprim(Pmakeblock(_tag, mut) as prim, lams) ->
      let (lams, _types) = List.split lams in
      let (jlams, approxs) = List.split (List.map (close fenv cenv) lams) in
      (Jprim(prim, jlams, Debuginfo.none),
       begin match mut with
           Immutable -> Value_tuple(Array.of_list approxs)
         | Mutable -> Value_unknown None
       end)
  | Lprim(Pfield n, [lam, _type]) ->
      let (jlam, approx) = close fenv cenv lam in
      let fieldapprox =
        match approx with
          Value_tuple a when n < Array.length a -> a.(n)
        | _ -> Value_unknown None in
      check_constant_result lam (Jprim(Pfield n, [jlam], Debuginfo.none))
                            fieldapprox
  | Lprim(Psetfield (n, _), [Lprim (Pgetglobal id, []), _type1; lam, _type2]) ->
      let (jlam, approx) = close fenv cenv lam in
      (!global_approx).(n) <- approx;
      (Jprim(Psetfield (n, false), [getglobal id; jlam], Debuginfo.none),
       Value_unknown None)
  | Lprim(Praise, [Levent (arg, ev), _type]) ->
      let (jlam, _approx) = close fenv cenv arg in
      (Jprim(Praise, [jlam], Debuginfo.from_raise ev),
       Value_unknown None)
  | Lprim (Pccall { Primitive.prim_name = pname; _ }, args)
    when is_java_primitive pname ->
      close_java_primitive fenv cenv pname args
  | Lprim (Pccall { Primitive.prim_name = pname; _ }, args)
    when is_java_array_primitive pname ->
      let (args, _approx) = close_list_approx fenv cenv args in
      let (javaprim, repr) = java_array_primitive_infos pname in
      (Jjavaprim(javaprim, args, Debuginfo.none), Value_unknown(Some repr))
  | Lprim((Pccall { Primitive.prim_name = "ocamljava_javastring_of_string"; _ }) as p, args) ->
      let (args, _approx) = close_list_approx fenv cenv args in
      begin match args with
        [ Jconst (Lambda_const (Const_base (Const_string s))) ]
      | [ Jconst (Lambda_const (Const_immstring s)) ] ->
          make_const_string s
      | _ ->
          (Jprim(p, args, Debuginfo.none),
           Value_unknown(Some(LR_java_instance "java.lang.String")))
      end
  | Lprim((Pccall { Primitive.prim_name = "ocamljava_javastring_to_string"; _ }) as p, args) ->
      let (args, approx) = close_list_approx fenv cenv args in
      begin match approx with
        [ Value_java_string s ] ->
          (Jconst(Lambda_const(Const_immstring s)), Value_unknown None)
      | _ ->
          (Jprim(p, args, Debuginfo.none), Value_unknown None)
      end
  | Lprim(p, args) ->
      let p = optimize_intarray_primitive p args in
      simplif_prim p (close_list_approx fenv cenv args) Debuginfo.none
  | Lswitch(arg, sw) ->
(* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let (jarg, _) = close fenv cenv arg in
      let const_index, block_index, actions =
        close_switch fenv cenv
          sw.sw_consts sw.sw_numconsts
          sw.sw_blocks sw.sw_numblocks
          sw.sw_failaction in
      (Jswitch(jarg,
               { js_index_consts = const_index;
                 js_index_blocks = block_index;
                 js_actions = actions; }),
       Value_unknown None)
  | Lstaticraise (i, args) ->
      let args = List.map (fun x -> x, None) args in
      (Jstaticfail (i, (close_list fenv cenv args)), Value_unknown None)
  | Lstaticcatch(body, (i, vars), handler) ->
      let (jbody, _) = close fenv cenv body in
      let (jhandler, _) = close fenv cenv handler in
      (Jstaticcatch(i, vars, jbody, jhandler), Value_unknown None)
  | Ltrywith(body, id, handler) ->
      close_trywith fenv cenv body id handler
  | Lifthenelse(arg, ifso, ifnot) ->
      begin match close fenv cenv arg with
        (jarg, Value_constptr n) ->
          sequence_constant_expr arg jarg
            (close fenv cenv (if n = 0 then ifnot else ifso))
      | (jarg, _ ) ->
          let (jifso, _) = close fenv cenv ifso in
          let (jifnot, _) = close fenv cenv ifnot in
          (Jifthenelse(jarg, jifso, jifnot), Value_unknown None)
      end
  | Lsequence(lam1, lam2) ->
      let (jlam1, _) = close fenv cenv lam1 in
      let (jlam2, approx) = close fenv cenv lam2 in
      (Jsequence(jlam1, jlam2), approx)
  | Lwhile(cond, body) ->
      close_while fenv cenv cond body
  | Lfor(id, lo, hi, dir, body) ->
      close_for fenv cenv id lo hi dir body
  | Lassign(id, lam) ->
      let (jlam, _) = close fenv cenv lam in
      (Jassign(id, jlam), Value_unknown None)
  | Levent(lam, ev) ->
      let (jlam, approx) = close fenv cenv lam in
      (add_debug_info ev jlam, approx)
  | Lifused _ ->
      assert false

and loop_times known_bounds body =
  let body_size = lambda_size body in
  let maximum = if known_bounds then 8 else 4 in
  let times = max 1 (!Clflags.inline_threshold / body_size) in
  let times = min (if !Jclflags.opt_unroll_loops then maximum else 1) times in
  times

and close_while fenv cenv cond body =
  let (jcond, approx) = close fenv cenv cond in
  begin match approx with
    Value_constptr 0 ->
      Jnop, Value_unknown None
  | _ ->
      let (jbody, _) = close fenv cenv body in
      let times = loop_times false jbody in
      if (times > 1) || ((times = 1) && (lambda_size jcond < !Clflags.inline_threshold)) then
        (Jwhile (jcond, jbody, Some times), Value_unknown None)
      else
        (Jwhile (jcond, jbody, None), Value_unknown None)
  end

and close_for_unknown_bounds id jlo jhi dir jbody =
  let times = loop_times false jbody in
  if times > 1 then
    (Jfor (id, jlo, jhi, dir, jbody, Some times), Value_unknown None)
  else
    (Jfor (id, jlo, jhi, dir, jbody, None), Value_unknown None)

and close_for fenv cenv id lo hi dir body =
  let jlo, approx_lo = close fenv cenv lo in
  let jhi, approx_hi = close fenv cenv hi in
  let jbody, _ = close fenv cenv body in
  begin match approx_lo, approx_hi with
    Value_integer x, Value_integer y
      when ((dir = Asttypes.Upto) && (y < x)
         || (dir = Downto) && (y > x)) ->
       (* no iteration *)
       Jnop, Value_unknown None
  | Value_integer x, Value_integer y ->
      let delta =
        if dir = Asttypes.Upto then
          Targetint.(sub y x)
        else
          Targetint.(sub x y) in
      if (Targetint.compare delta Targetint.zero < 0)
      || (Targetint.equal delta Targetint.max_int) then begin
        (* bounds are known but range is far too big *)
        close_for_unknown_bounds id jlo jhi dir jbody
      end else begin
        let all_times = Targetint.succ delta in
        let times = loop_times true jbody in
        if (Targetint.(equal all_times one))
        || (Targetint.compare (Targetint.of_int times) all_times >= 0) then begin
          (* the loop will be totally unrolled *)
          let res = ref [] in
          let i = ref x in
          while (if dir = Asttypes.Upto then !i <= y else !i >= y) do
            let (idx, _) = make_const_int !i in
            let subst = Tbl.add id idx Tbl.empty in
            res := (substitute subst jbody) :: !res;
            if dir = Asttypes.Upto then Targetint.incr i else Targetint.decr i
          done;
          let res =
            match !res with
            | hd :: tl ->
                List.fold_left
                  (fun acc elem -> Jsequence (elem, acc))
                  hd
                  tl
            | _ ->
                (* the loop has at least one iteration,
                   otherwise it would have been reduced to 'Jnop' *)
                assert false in
          (res, Value_unknown None)
        end else
          (* the loop will be partially unrolled *)
          let n =
            times
            |> Targetint.of_int
            |> Targetint.min all_times (* note: 'times' is known to be "small" *)
            |> Targetint.to_int in
          (Jfor (id, jlo, jhi, dir, jbody, Some n), Value_unknown None)
      end
  | _ ->
      close_for_unknown_bounds id jlo jhi dir jbody
  end

and is_java_primitive = function
  | "java constructor"
  | "java make array"
  | "java make array dims"
  | "java method call"
  | "java method exec"
  | "java method chain"
  | "java field get"
  | "java field set"
  | "java iter"
  | "java fold"
  | "java instanceof"
  | "java cast"
  | "java class"
  | "java proxy loader"
  | "java proxy system"
  | "java proxy runtime"
  | "java null"
  | "java is_null"
  | "java is_not_null"
  | "java =="
  | "java !="
  | "java throw"
  | "java synchronized" -> true
  | _ -> false

and split_args = function
  | (Jconst (Lambda_const (Const_base (Const_int id))) :: args), (_ :: approx) ->
      id, args, approx
  | _ ->
      raise (Error Special_primitive_first_argument)

and close_java_primitive fenv cenv pname args =
  match pname with
  | "java constructor" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, _approx) = split_args args in
      begin match Jtypes.get_constructor_info id with
        { Jtypes.constructor_class; constructor_method; constructor_ellipsis } ->
          let class_name = convert_class_name constructor_class.ClassDefinition.name in
          let types = convert_java_types constructor_method.Method.cstr_descriptor in
          (Jjavaprim(Java_constructor(class_name, types, constructor_ellipsis), args, Debuginfo.none),
           Value_unknown(Some(LR_java_instance class_name)))
      end
  | "java make array" | "java make array dims" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, _approx) = split_args args in
      begin match Jtypes.get_array_info id with
        { Jtypes.array_type; array_total_dimensions; array_init_dimensions } ->
          let typ = convert_array_type array_type in
          let dims = { jpad_total = array_total_dimensions;
                       jpad_init = array_init_dimensions } in
          (Jjavaprim(Java_array(typ, dims), args, Debuginfo.none),
           Value_unknown(Some(repr_of_java_type (typ :> Jlambda.java_type))))
      end
  | "java method call" | "java method exec" | "java method chain" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, _approx) = split_args args in
      begin match Jtypes.get_method_info id with
        { Jtypes.method_class; method_method; method_call; method_ellipsis } ->
          let class_name = convert_class_name method_class.ClassDefinition.name in
          let meth_name = convert_method_name method_method.Method.name in
          let kind =
            if AccessFlag.mem_class `Interface method_class.ClassDefinition.access_flags then
              Interface_call
            else if AccessFlag.mem_method `Static method_method.Method.flags then
              Static_call
            else
              Virtual_call in
          let (params, return) = method_method.Method.descriptor in
          let params = convert_java_types params in
          let return = convert_java_type return in
          (Jjavaprim(Java_method(class_name, meth_name, method_call, kind, params, method_ellipsis, return), args, Debuginfo.none),
           Value_unknown(Some(repr_of_java_type return)))
      end
  | "java field get" | "java field set" ->
      let get = pname = "java field get" in
      let args = close_list_approx fenv cenv args in
      let (id, args, _approx) = split_args args in
      begin match Jtypes.get_field_get_info id with
        { Jtypes.field_class; field_field } ->
          let class_name = convert_class_name field_class.ClassDefinition.name in
          let field_name = convert_field_name field_field.Field.name in
          let kind =
            if AccessFlag.mem_field `Static field_field.Field.flags then
              Static_field
            else
              Instance_field in
          let typ = convert_java_type_no_void field_field.Field.descriptor in
          if get then
            (Jjavaprim(Java_get(class_name, field_name, kind, typ), args, Debuginfo.none),
             Value_unknown(Some(repr_of_java_type (typ :> Jlambda.java_type))))
          else
            (Jjavaprim(Java_set(class_name, field_name, kind, typ), args, Debuginfo.none),
             Value_unknown None)
      end
  | "java iter" ->
      begin match args with
        [ id, _; funct, _; iterator, _ ] ->
          let (id, _approx) = close fenv cenv id in
          let (iterator, _approx) = close fenv cenv iterator in
          let typ =
            match id with
            | Jconst (Lambda_const (Const_base (Const_int id))) ->
                let tc = Jtypes.get_reference_type_info id in
                convert_java_type_no_void tc.Jtypes.type_class
            | _ ->
                raise (Error Special_primitive_first_argument) in
          let id_iterator = Ident.create "iterator" in
          let id_element = Ident.create "element" in
          let has_next = Java_method("java.util.Iterator", "hasNext", Jtypes.Bare_call,
                                     Interface_call, [], false, `Boolean) in
          let cond = Jjavaprim(has_next, [Jvar id_iterator], Debuginfo.none) in
          let next = Java_method("java.util.Iterator", "next", Jtypes.Bare_call,
                                 Interface_call, [], false, `Class "java.lang.Object") in
          let body = Jlet(id_element,
                          Jjavaprim(Java_cast(typ),
                                    [Jjavaprim(next, [Jvar id_iterator], Debuginfo.none)],
                                    Debuginfo.none),
                          match funct with
                          | Lfunction(_kind, [param, _], _return, body) ->
                              let (body, _approx) = close fenv cenv body in
                              let subst = Tbl.add param (Jvar id_element) Tbl.empty in
                              let body = substitute subst body in
                              body
                          | _ ->
                              let element = Lvar id_element, None in
                              let call = Lapply(funct, [element], Location.none) in
                              let (call, _approx) = close fenv cenv call in
                              call) in
          Jlet(id_iterator, iterator, Jwhile(cond, body, None)), Value_unknown None
      | _ ->
          assert false
      end
  | "java fold" ->
      begin match args with
        [ id, _; funct, _; zero, _; iterator, _ ] ->
          let (id, _approx) = close fenv cenv id in
          let (zero, _approx) = close fenv cenv zero in
          let (iterator, _approx) = close fenv cenv iterator in
          let typ =
            match id with
            | Jconst (Lambda_const (Const_base (Const_int id))) ->
                let tc = Jtypes.get_reference_type_info id in
                convert_java_type_no_void tc.Jtypes.type_class
            | _ ->
                raise (Error Special_primitive_first_argument) in
          let id_acc = Ident.create "acc" in
          let id_iterator = Ident.create "iterator" in
          let id_element = Ident.create "element" in
          let has_next = Java_method("java.util.Iterator", "hasNext", Jtypes.Bare_call,
                                     Interface_call, [], false, `Boolean) in
          let cond = Jjavaprim(has_next, [Jvar id_iterator], Debuginfo.none) in
          let next = Java_method("java.util.Iterator", "next", Jtypes.Bare_call,
                                 Interface_call, [], false, `Class "java.lang.Object") in
          let body = Jlet(id_element,
                          Jjavaprim(Java_cast(typ),
                                    [Jjavaprim(next, [Jvar id_iterator], Debuginfo.none)],
                                    Debuginfo.none),
                          match funct with
                          | Lfunction(_kind, [param_acc, _; param_elem, _], _return, body) ->
                              let (body, _approx) = close fenv cenv (Lassign(id_acc, body)) in
                              let subst = Tbl.add param_acc (Jvar id_acc) Tbl.empty in
                              let subst = Tbl.add param_elem (Jvar id_element) subst in
                              let body = substitute subst body in
                              body
                          | _ ->
                              let acc = Lvar id_acc, None in
                              let element = Lvar id_element, None in
                              let call = Lapply(funct, [acc; element], Location.none) in
                              let assign = Lassign(id_acc, call) in
                              let (assign, _approx) = close fenv cenv assign in
                              assign) in
          Jlet(id_acc, zero,
               Jlet(id_iterator, iterator,
                    Jsequence(Jwhile(cond, body, None),
                              Jvar id_acc))), Value_unknown None
      | _ ->
          assert false
      end
  | "java instanceof" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, approx) = split_args args in
      begin match Jtypes.get_reference_type_info id, approx with
        _, [ Value_java_null _ ] ->
          make_const_bool true
      | { Jtypes.type_class }, _ ->
          let typ = convert_java_type_no_void type_class in
          (Jjavaprim(Java_instanceof typ, args, Debuginfo.none),
           Value_unknown(Some LR_bool))
      end
  | "java cast" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, approx) = split_args args in
      begin match Jtypes.get_reference_type_info id, approx with
        { Jtypes.type_class }, [ Value_java_null _ ] ->
          let typ = convert_java_type_no_void type_class in
          make_const_null (Some typ)
      | { Jtypes.type_class }, _ ->
          let typ = convert_java_type_no_void type_class in
          (Jjavaprim(Java_cast typ, args, Debuginfo.none),
           Value_unknown(Some(repr_of_java_type (typ :> Jlambda.java_type))))
      end
  | "java class" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, _) = split_args args in
      begin match Jtypes.get_any_type_info id with
        { Jtypes.any_type_desc } ->
          let typ = convert_java_type any_type_desc in
          let args = (Jconst(Lambda_const(Const_pointer 0))) :: args in
          (Jjavaprim(Java_class typ, args, Debuginfo.none),
           Value_unknown(Some(LR_java_instance "java.lang.Class")))
      end
  | "java proxy loader" | "java proxy system" | "java proxy runtime" ->
      let args = close_list_approx fenv cenv args in
      let (id, args, _approx) = split_args args in
      begin match Jtypes.get_proxy_info id with
        { Jtypes.proxy_class; proxy_classes; proxy_mapping } ->
          let jpp_kind =
            match pname with
            | "java proxy loader"  -> Custom_class_loader
            | "java proxy system"  -> System_class_loader
            | "java proxy runtime" -> Runtime_class_loader
            | _ -> assert false in
          let jpp_interface = convert_class_name proxy_class in
          let jpp_interfaces =
            List.map
              (fun cd -> convert_class_name cd.ClassDefinition.name)
              proxy_classes in
          let jpp_mapping =
            List.map
              (fun (intf, meth, (params, _return), name) ->
                { jppb_interface = intf;
                  jppb_method = meth;
                  jppb_parameters = convert_java_types params;
                  jppb_ocaml_name = name })
              proxy_mapping in
          let proxy = { jpp_kind; jpp_interface; jpp_interfaces; jpp_mapping } in
          let typ = `Class jpp_interface in
          (Jjavaprim(Java_proxy proxy, args, Debuginfo.none),
           Value_unknown(Some(repr_of_java_type typ)))
      end
  | "java null" ->
      let typ = Some (`Class "java.lang.Object") in
      begin match args with
        [ arg, _ ] when is_pure arg ->
          make_const_null typ
      | _ ->
          let (args, _approx) = close_list_approx fenv cenv args in
          ((Jsequence(Jprim(Pignore, args, Debuginfo.none), Jconst(Const_null typ))),
           Value_java_null typ)
      end
  | "java is_null" | "java is_not_null" ->
      let (args, approx) = close_list_approx fenv cenv args in
      begin match approx with
        [ Value_java_null _ ] ->
          make_const_bool (pname = "java is_null")
      | _ ->
          let test = if pname = "java is_null" then Java_is_null else Java_is_not_null in
          (Jjavaprim(test, args, Debuginfo.none),
           Value_unknown(Some LR_bool))
      end
  | "java ==" | "java !=" ->
      let (args, _approx) = close_list_approx fenv cenv args in
      let test = if pname = "java ==" then Java_equal else Java_not_equal in
      (Jjavaprim(test, args, Debuginfo.none),
       Value_unknown(Some LR_bool))
  | "java throw" ->
      let (args, _approx) = close_list_approx fenv cenv args in
      (Jjavaprim(Java_throw, args, Debuginfo.none), Value_unknown None)
  | "java synchronized" ->
      begin match args with
        [ lock, _; Lfunction(_kind, _params, _return, body), _ ] ->
          let (lock, _approx) = close fenv cenv lock in
          let (body, _approx) = close fenv cenv body in
          (Jjavaprim(Java_synchronized(Inlined_func, min_int), [lock; body], Debuginfo.none),
           Value_constptr 0)
      | [ lock, _; funct, _ ] ->
          let (lock, _approx) = close fenv cenv lock in
          let unit = Lconst(Const_pointer 0), Some Predef.type_unit in
          let expr = Lapply(funct, [unit], Location.none) in
          let (expr, _approx) = close fenv cenv expr in
          (Jjavaprim(Java_synchronized(Called_func, min_int), [lock; expr], Debuginfo.none),
           Value_constptr 0)
      | _ ->
          assert false
      end
  | _ ->
      assert false

and close_trywith fenv cenv body id handler =
  let (jbody, _) = close fenv cenv body in
  let (jhandler, _) = close fenv cenv handler in
  let handlers = extract_try_with_handlers jhandler in
  let raised = extract_raised jbody in
  let contains_unmatched_tag =
    List.exists
      (fun r ->
        List.for_all
          (fun (t, _jlam) -> not (same_exception_tag r t))
          handlers)
      raised in
  let handlers =
    handlers
    |> List.filter (* keep only the handlers that can be reached *)
        (fun (tag, _jlam) ->
          if (tag = Any_exception) then
            contains_unmatched_tag
          else
            (* check if 'tag' can actually be raised *)
            List.exists
              (fun t -> same_exception_tag tag t)
              raised)
    |> List.map (* associate static fail ids to handlers *)
        (fun (tag, jlam) ->
          (tag, jlam, occurs_var id jlam, Lambda.next_raise_count ())) in
  let jbody = replace_raise handlers jbody in
  let jbody =
    List.fold_left
      (fun acc (_tag, jlam, use_id, nfail) ->
        let params = if use_id then [id] else [] in
        Jstaticcatch (nfail, params, acc, jlam))
      jbody
      handlers in
  (Jtrywith (jbody, id, jhandler), Value_unknown None)

and close_list fenv cenv = function
    [] -> []
  | (lam, _) :: rem ->
      let (jlam, _) = close fenv cenv lam in
      jlam :: close_list fenv cenv rem

and close_list_approx fenv cenv = function
    [] -> ([], [])
  | (lam, _) :: rem ->
      let (jlam, approx) = close fenv cenv lam in
      let (jlams, approxs) = close_list_approx fenv cenv rem in
      (jlam :: jlams, approx :: approxs)

and close_named fenv cenv id = function
    Lfunction(_kind, _params, _repr, _body) as funct ->
      close_one_function fenv cenv id funct
  | lam ->
      close fenv cenv lam

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions fenv cenv fun_defs =
  (* Update and check nesting depth *)
  incr function_nesting_depth;
  let initially_closed =
    !function_nesting_depth < excessive_function_nesting_depth in
  (* Determine the free variables of the functions *)
  let fv =
    IdentSet.elements (free_variables (Lletrec(fun_defs, lambda_unit))) in
  (* Build the function descriptors for the functions.
     Initially all functions are assumed not to need their environment
     parameter. *)
  let uncurried_defs =
    List.map
      (function
          (id, Lfunction (kind, params, repr, body)) ->
            let label = Jcompilenv.make_symbol (Some (Ident.unique_name id)) in
            let arity = List.length params in
            let class_name =
              !Jclflags.java_package ^ "." ^ (Jcompilenv.current_unit_name ()) in
            let fundesc =
              {fun_label = { fl_class = class_name; fl_method = label };
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_repr_parameters = List.map snd params;
               fun_repr_return = repr;
               fun_closed = initially_closed;
               fun_inline = None } in
            (id, params, body, fundesc)
        | (_, _) -> Misc.fatal_error "Jlambda.close_functions")
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let fenv_rec =
    List.fold_right
      (fun (id, _params, _body, fundesc) fenv ->
        Tbl.add id (Value_closure(fundesc, Value_unknown None)) fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun (_id, _params, _body, fundesc) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref initially_closed in
  (* Translate each function definition *)
  let clos_fundef (id, params, body, fundesc) env_pos =
    let dbg = match body with
      | Levent (_,({lev_kind=Lev_function; _} as ev)) -> Debuginfo.from_call ev
      | _ -> Debuginfo.none in
    let env_param = Ident.create "env" in
    let cenv_fv =
      build_closure_env env_param (fv_pos - env_pos) fv in
    let cenv_body =
      List.fold_right2
        (fun (id, _params, _arity, _body) pos env ->
          Tbl.add id (Joffset(Jvar env_param, pos - env_pos)) env)
        uncurried_defs clos_offsets cenv_fv in
    let (jbody, approx) = close fenv_rec cenv_body body in
    if !useless_env && occurs_var env_param jbody then useless_env := false;
    let fun_params = if !useless_env then params else params @ [env_param, LR_value] in
    ({ label  = fundesc.fun_label;
       arity  = fundesc.fun_arity;
       params = fun_params;
       return = fundesc.fun_repr_return;
       body   = jbody;
       dbg },
     (id, env_pos, Value_closure (fundesc, approx))) in
  (* Translate all function definitions. *)
  let clos_info_list =
    if initially_closed then begin
      let cl = List.map2 clos_fundef uncurried_defs clos_offsets in
      (* If the hypothesis that the environment parameters are useless has been
         invalidated, then set [fun_closed] to false in all descriptions and
         recompile *)
      if !useless_env then cl else begin
        List.iter
          (fun (_id, _params, _body, fundesc) -> fundesc.fun_closed <- false)
          uncurried_defs;
        List.map2 clos_fundef uncurried_defs clos_offsets
      end
    end else
      (* Excessive closure nesting: assume environment parameter is used *)
        List.map2 clos_fundef uncurried_defs clos_offsets
    in
  (* Update nesting depth *)
  decr function_nesting_depth;
  (* Return the Uclosure node and the list of all identifiers defined,
     with offsets and approximations. *)
  let (clos, infos) = List.split clos_info_list in
  (Jclosure(clos, List.map (close_var fenv cenv) fv), infos)

(* Same, for one non-recursive function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
      ((Jclosure([f], _) as clos),
       [_, _, (Value_closure(fundesc, _) as approx)]) ->
        (* See if the function can be inlined *)
        if lambda_smaller f.body
          (!Clflags.inline_threshold + List.length f.params)
        then fundesc.fun_inline <- Some (List.map fst f.params, f.body);
        (clos, approx)
    | _ -> Misc.fatal_error "Jlambda.close_one_function"

(* Close a switch *)

and close_switch fenv cenv const_cases const_num block_cases block_num default =
  let const_index = Array.create const_num 0
  and block_index = Array.create block_num 0
  and store = Switch.mk_store Lambda.same in
  let store_actions index cases =
    List.iter
      (fun (key, lam) ->
        index.(key) <- store.Switch.act_store lam)
      cases in

  (* First default case *)
  begin match default with
  | Some def when (List.length const_cases < const_num) || (List.length block_cases < block_num) ->
      ignore (store.Switch.act_store def)
  | _ -> ()
  end ;
  (* Then all other cases *)
  store_actions const_index const_cases;
  store_actions block_index block_cases;
  (* Compile action *)
  let actions =
    Array.map
      (fun lam ->
        let jlam, _approx = close fenv cenv lam in
        jlam)
      (store.Switch.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |], [| |] (* May happen when default is None *)
  | _     -> const_index, block_index, actions


(* The entry point *)

let intro size lam =
  function_nesting_depth := 0;
  global_approx := Array.create size (Value_unknown None);
  Jcompilenv.set_global_approx(Value_tuple !global_approx);
  let (jlam, _approx) = close Tbl.empty Tbl.empty lam in
  global_approx := [||];
  jlam

(* Error report *)

let report_error ppf = function
  | Special_primitive_first_argument ->
      Format.fprintf ppf
        "First argument of Java primitive should be a constant string"
