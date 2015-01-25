(*
 * Copyright (C) 2013 Citrix Inc
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

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]

let ( >>= ) x f = match x with
  | `Error y -> `Error y
  | `Ok z -> f z

let return x = `Ok x
let fail y = `Error y

let kib = 1024L
let mib = Int64.mul kib 1024L
let gib = Int64.mul mib 1024L

module Partition = struct
  type t = {
    ty: Uuidm.t;
    guid: Uuidm.t;
    first_lba: int64;
    last_lba: int64;
    system_partition: bool;
    efi_should_ignore: bool;
    legacy_bios_bootable: bool;
    name: string;
  }

  let microsoft_basic_data_partition =
    let uuid = "EBD0A0A2-B9E5-4433-87C0-68B6B72699C7" in
    match Uuidm.of_string uuid with
    | Some x -> x
    | None -> failwith (Printf.sprintf "Microsoft and Uuidm seem to disagree on whether %s is a uuid or not" uuid)

  let make ?(legacy_bios_bootable=false) ?(ty=microsoft_basic_data_partition) first_lba last_lba =
    let guid = Uuidm.create `V4 in
    let system_partition = false in
    let efi_should_ignore = false in
    let name = "" in
    { ty; guid; first_lba; last_lba; system_partition; efi_should_ignore; legacy_bios_bootable;
      name }

  cstruct part {
    uint8_t type_guid[16];
    uint8_t unique_guid[16];
    uint64_t first_lba;
    uint64_t last_lba;
    uint64_t flags;
    uint8_t name[72];
  } as little_endian

  let _ = assert (sizeof_part = 128)

  let sizeof = sizeof_part

  let unmarshal (buf: Cstruct.t) : (t, string) result =
    ( if Cstruct.len buf < sizeof_part
      then fail (Printf.sprintf "partition entry too small: %d < %d" (Cstruct.len buf) sizeof_part)
      else return () ) >>= fun () ->
    ( match Uuidm.of_string (copy_part_type_guid buf) with
      | None -> fail (Printf.sprintf "type is not a uuid: %s" (copy_part_type_guid buf))
      | Some x -> return x ) >>= fun ty ->
    ( match Uuidm.of_string (copy_part_unique_guid buf) with
      | None -> fail (Printf.sprintf "guid is not a uuid: %s" (copy_part_unique_guid buf))
      | Some x -> return x ) >>= fun guid ->
    let first_lba = get_part_first_lba buf in
    let last_lba = get_part_last_lba buf in
    let flags = get_part_flags buf in
    let system_partition = Int64.(logand flags 1L) <> 0L in
    let efi_should_ignore = Int64.(logand flags 2L) <> 0L in
    let legacy_bios_bootable = Int64.(logand flags 4L) <> 0L in
    let name = copy_part_name buf in
    return { ty; guid; first_lba; last_lba; system_partition;
      efi_should_ignore; legacy_bios_bootable; name }

  let marshal (buf: Cstruct.t) t =
    set_part_type_guid (Uuidm.to_string t.ty) 0 buf;
    set_part_unique_guid (Uuidm.to_string t.guid) 0 buf;
    set_part_first_lba buf t.first_lba;
    set_part_last_lba buf t.last_lba;
    let flags =
      (if t.system_partition then 1 else 0)
      lor (if t.efi_should_ignore then 2 else 0)
      lor (if t.legacy_bios_bootable then 4 else 0) in
    set_part_flags buf (Int64.of_int flags);
    set_part_name t.name 0 buf

  let _ty = "type"
  let _guid = "guid"
  let _first_lba = "first-lba"
  let _last_lba = "last-lba"
  let _system_partition = "system-partition"
  let _efi_should_ignore = "efi-should-ignore"
  let _legacy_bios_bootable = "legacy-bios-bootable"
  let _name = "name"

  let all = [ _ty; _guid; _first_lba; _last_lba;
    _system_partition; _efi_should_ignore; _legacy_bios_bootable;
    _name
  ]

  let get t key =
    if key = _ty
    then Some (Uuidm.to_string t.ty)
    else if key = _guid
    then Some (Uuidm.to_string t.guid)
    else if key = _first_lba
    then Some (Int64.to_string t.first_lba)
    else if key = _last_lba
    then Some (Int64.to_string t.last_lba)
    else if key = _system_partition
    then Some (string_of_bool t.system_partition)
    else if key = _efi_should_ignore
    then Some (string_of_bool t.efi_should_ignore)
    else if key = _legacy_bios_bootable
    then Some (string_of_bool t.legacy_bios_bootable)
    else if key = _name
    then Some t.name
    else None

end

type t = {
  current_lba: int64;
  backup_lba: int64;
  first_usable_lba: int64;
  last_usable_lba: int64;
  disk_guid: Uuidm.t;
  partitions: Partition.t list;
}

let make partitions =
  let current_lba = 0L in
  let backup_lba = 0L in
  let first_usable_lba = 0L in
  let last_usable_lba = 0L in
  let disk_guid = Uuidm.create `V4 in
  { current_lba; backup_lba; first_usable_lba; last_usable_lba;
    disk_guid; partitions }

(* GPT header format from wikipedia: *)
cstruct gpt {
  uint8_t signature[8];
  uint32_t revision;
  uint32_t header_size;
  uint32_t header_crc32;
  uint32_t reserved;
  uint64_t current_lba;
  uint64_t backup_lba;
  uint64_t first_usable_lba;
  uint64_t last_usable_lba;
  uint16_t disk_guid[16];
  uint64_t first_partition_entries_lba;
  uint32_t number_partition_entries;
  uint32_t partition_entries_crc32;
  (* zeroes for the rest of the block *)
} as little_endian

(* The datastructure is spread over the disk and of variable size;
   reading or writing therefore requires random access. *)
module Make(B: V1_LWT.BLOCK) = struct
  open Lwt

  let unmarshal (b: B.t) =
    B.get_info b
    >>= fun info ->
    return (make [])

  let marshal t (b: B.t) =
    return (`Error "unimplemented")
end

let _current_lba = "current-lba"
let _backup_lba = "backup-lba"
let _first_usable_lba = "first-usable-lba"
let _last_usable_lba = "last-usable-lba"
let _disk_guid = "disk-guid"
let _partition = "partition"
let all = [
  _current_lba;
  _backup_lba;
  _first_usable_lba;
  _last_usable_lba;
  _disk_guid;
] @ (List.concat (List.map (fun i -> List.map (fun k -> Printf.sprintf "%s/%d/%s" _partition i k) Partition.all) [0;1;2;3]))

let slash = Re_str.regexp_string "/"

let get t key =
  if key = _current_lba
  then Some (Int64.to_string t.current_lba)
  else if key = _backup_lba
  then Some (Int64.to_string t.backup_lba)
  else if key = _first_usable_lba
  then Some (Int64.to_string t.first_usable_lba)
  else if key = _last_usable_lba
  then Some (Int64.to_string t.last_usable_lba)
  else if key = _disk_guid
  then Some (Uuidm.to_string t.disk_guid)
  else begin match Re_str.split slash key with
   | [ p; i; k ] when p = _partition ->
     begin
       try
         Partition.get (List.nth t.partitions (int_of_string i)) k
       with _ -> None
     end
   | _ -> None
  end
