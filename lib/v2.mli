(*
 * Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013      Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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


module type IO_PAGE = sig

  type buffer
  (** Type of a C buffer (usually Cstruct) *)

  include V1.IO_PAGE with type buf := buffer

end

module type CLOCK = V1.CLOCK

module type DEVICE = V1.DEVICE

type console_error = V1.console_error

module type CONSOLE = sig

  include V1.CONSOLE

  val fprintf: t -> ('a, t, unit) format -> 'a
  (** [printf c fmt a1 ... an] formats the arguments [a1] to [an]
      according to the format string [format] and output the resulting
      string on the console [c]. *)

end

type block_error = V1.block_error

module type BLOCK = V1.BLOCK

module type KV_RO_0 = sig

  include V1.KV_RO_0

  val read: t -> string -> int64 -> int64 ->
    [ `Ok of page_aligned_buffer list | `Error of error ] io
  (** [read t key offset length] reads up to [length] bytes from the value
      associated with [key]. If less data is returned than requested, this
      indicates the end of the value. *)

  val read_all: t -> string ->
    [ `Ok of page_aligned_buffer list | `Error of error ] io
  (** [read_all t key] returns the full contents of the value
      associated with [key]. *)

  val mem: t -> string -> bool
  (** Check whether a key exists. *)

  val list: t -> string -> [ `Ok of string list | `Error of error ] io
  (** [list t path] returns the names of files and subdirectories
      within the directory [path] *)

end

type kv_ro_error = [
  | `Not_a_directory of string             (** Cannot create a directory entry in a file *)
  | `Is_a_directory of string              (** Cannot read or write the contents of a directory *)
  | `No_directory_entry of string * string (** Cannot find a directory entry *)
  | `Format_not_recognised of string       (** The block device appears to not be formatted *)
  | `Unknown_error of string
  | `Block_device of block_error
]
(** IO errors for kv/ro *)

module type KV_RO = KV_RO_0 with type error = kv_ro_error

type fs_error = [
  | `Directory_not_empty of string         (** Cannot remove a non-empty directory *)
  | `No_space                              (** No space left on the block device *)
  | `File_already_exists of string         (** Cannot create a file with a duplicate name *)
]
(** IO error operations for filesystems. *)

module type FS = sig

  include KV_RO_0 with type error = [ fs_error | kv_ro_error]

  val format: t -> int64 -> [ `Ok of unit | `Error of error ] io
  (** [format t size] erases the contents of [t] and creates an empty filesystem
      of size [size] bytes *)

  val create: t -> string -> [ `Ok of unit | `Error of error ] io
  (** [create t path] creates an empty file at [path] *)

  val mkdir: t -> string -> [ `Ok of unit | `Error of error ] io
  (** [mkdir t path] creates an empty directory at [path] *)

  val destroy: t -> string -> [ `Ok of unit | `Error of error ] io
  (** [destroy t path] removes a [path] (which may be a file or an empty
      directory) on filesystem [t] *)

  val write: t -> string -> int -> page_aligned_buffer -> [ `Ok of unit | `Error of error ] io
  (** [write t path offset data] writes [data] at [offset] in file [path] on
      filesystem [t] *)

  type stat = {
    basename: string; (** Filename within the enclosing directory *)
    read_only: bool;  (** True means the contents are read-only; this
                          is the case for RO filesystems but might not
                          be the case for RW filesystems that we use
                          using a RO intertace. *)
    directory: bool;  (** True means the entity is a directory; *)
    size: int64;      (** Size of the entity in bytes *)
  }
  (** Per-file/directory statistics *)

  val stat: t -> string -> [ `Ok of stat | `Error of error ] io
  (** [stat t path] returns information about file or directory at [path] *)

end

module type NETWORK = V1.NETWORK
