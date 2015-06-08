type account = {
    name : string; (* bare value *)
    balance : int STM.ref; (* value protected by transactions *)
  }

let make_account n b =
  { name = n; balance = STM.ref b }

let print_account acc =
  STM.run_read_only (fun get ->
    Printf.printf "%s = %dn" acc.name (get acc.balance))

let transfer x y a =
  STM.run (fun get set ->
    let xv, yv = get x.balance, get y.balance in
    set x.balance (xv - a);
    set y.balance (yv + a));
