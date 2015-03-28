(*
 * This file is part of OCaml-Java library.
 * Copyright (C) 2007-2015 Xavier Clerc.
 *
 * OCaml-Java library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Java library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Class'java'util'concurrent'Phaser
open Class'java'util'concurrent'TimeUnit

type t = _'Phaser java_instance

let make ?(parent = Java.null) ?(parties = 0l) () =
  Java.make "Phaser(Phaser,int)" parent parties

let arrive ph =
  Java.call "Phaser.arrive()" ph

let arrive_and_await_advance ph =
  Java.call "Phaser.arriveAndAwaitAdvance()" ph

let arrive_and_deregister ph =
  Java.call "Phaser.arriveAndDeregister()" ph

let await_advance ph phase =
  Java.call "Phaser.awaitAdvance(int)" ph phase

let await_advance_interruptibly ph phase =
  Java.call "Phaser.awaitAdvanceInterruptibly(int)" ph phase

let await_advance_interruptibly_time ph phase time timeunit =
  Java.call "Phaser.awaitAdvanceInterruptibly(int,long,TimeUnit)"
    ph phase time timeunit

let bulk_register ph parties =
  Java.call "Phaser.bulkRegister(int)" ph parties

let force_termination ph =
  Java.call "Phaser.forceTermination()" ph

let get_arrived_parties ph =
  Java.call "Phaser.getArrivedParties()" ph

let get_parent ph =
  Java.call "Phaser.getParent()" ph

let get_phase ph =
  Java.call "Phaser.getPhase()" ph

let get_registered_parties ph =
  Java.call "Phaser.getRegisteredParties()" ph

let get_root ph =
  Java.call "Phaser.getRoot()" ph

let get_unarrived_parties ph =
  Java.call "Phaser.getUnarrivedParties()" ph

let is_terminated ph =
  Java.call "Phaser.isTerminated()" ph

let register ph =
  Java.call "Phaser.register()" ph
