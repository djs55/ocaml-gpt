(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Common
open Cmdliner
open Lwt

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let info common filename =
  try
    let filename = require "filename" filename in
    let module G = Gpt.Make(Block) in
    let t =
      Block.connect filename
      >>= function
      | `Error _ -> fail (Failure "Unable to open block device")
      | `Ok block ->
        G.unmarshal block
        >>= fun gpt ->
        let all = List.map (fun f ->
        match Gpt.get gpt f with
          | Some v -> [ f; v ]
          | None -> assert false
        ) Gpt.all in
        print_table ["field"; "value"] all;
        return () in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    exit 1

let write common filename =
  try
    let filename = require "filename" filename in
    failwith "unimplemented"
  with Failure x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    exit 1
