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
    let gpt = Cstruct.create Gpt.sizeof in
    let t =
      Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
      Gpt_lwt.really_read fd gpt >>= fun () ->
      let gpt = match Gpt.unmarshal gpt with
      | `Error reason ->
        Printf.fprintf stderr "Failed to unmarshal GPT: %s\n%!" reason;
        exit 1
      | `Ok x -> x in
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
    let t =
      let gpt = Cstruct.create Gpt.sizeof in
      Lwt_unix.LargeFile.stat filename >>= fun st ->
      let total_bytes = st.Lwt_unix.LargeFile.st_size in
      let total_sectors = Int64.(to_int32 (div total_bytes 512L)) in
      let partition_length = Int32.sub total_sectors Gpt.default_partition_start in
      Gpt.marshal gpt (Gpt.make [Gpt.Partition.make ~active:true ~ty:6 Gpt.default_partition_start partition_length]);
      Lwt_unix.openfile filename [ Lwt_unix.O_WRONLY ] 0x0 >>= fun fd ->
      Gpt_lwt.really_write fd gpt >>= fun () ->
      Lwt_unix.close fd in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    exit 1
