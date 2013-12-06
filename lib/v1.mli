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
  (** Memory allocation interface. *)

  type buf
  (** Type of a C buffer (usually Cstruct) *)

  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** Type of memory blocks. *)

  val get : int -> t
  (** [get n] allocates and returns a memory block of [n] pages. If
      there is not enough memory, the unikernel will terminate. *)

  val get_order : int -> t
  (** [get_order i] is [get (1 lsl i)]. *)

  val pages : int -> t list
  (** [pages n] allocates a memory block of [n] pages and return the the
      list of pages allocated. *)

  val pages_order : int -> t list
  (** [pages_order i] is [pages (1 lsl i)]. *)

  val length : t -> int
  (** [length t] is the size of [t], in bytes. *)

  val to_cstruct : t -> buf
  val to_string : t -> string

  val to_pages : t -> t list
  (** [to_pages t] is a list of [size] memory blocks of one page each,
      where [size] is the size of [t] in pages. *)

  val string_blit : string -> int -> t -> int -> int -> unit
  (** [string_blit src srcoff dst dstoff len] copies [len] bytes from
      string [src], starting at byte number [srcoff], to memory block
      [dst], starting at byte number dstoff. *)

  val blit : t -> t -> unit
  (** [blit t1 t2] is the same as {!Bigarray.Array1.blit}. *)

  val round_to_page_size : int -> int
  (** [round_to_page_size n] returns the number of bytes that will be
      allocated for storing [n] bytes in memory *)
end

module type CLOCK = sig
  (** Clock operations.
      Currently read-only to retrieve the time in various formats. *)

  type tm =
    { tm_sec : int;               (** Seconds 0..60 *)
      tm_min : int;               (** Minutes 0..59 *)
      tm_hour : int;              (** Hours 0..23 *)
      tm_mday : int;              (** Day of month 1..31 *)
      tm_mon : int;               (** Month of year 0..11 *)
      tm_year : int;              (** Year - 1900 *)
      tm_wday : int;              (** Day of week (Sunday is 0) *)
      tm_yday : int;              (** Day of year 0..365 *)
      tm_isdst : bool;            (** Daylight time savings in effect *)
    }
  (** The type representing wallclock time and calendar date. *)

  val time : unit -> float
  (** Return the current time since 00:00:00 GMT, Jan. 1, 1970, in
      seconds. *)

  val gmtime : float -> tm
  (** Convert a time in seconds, as returned by {!time}, into a
      date and a time. Assumes UTC (Coordinated Universal Time), also
      known as GMT. *)
end

module type DEVICE = sig
  (** Device operations.
      Defines the functions to connect and disconnect any device *)

  type +'a io
  (** A potentially blocking I/O operation *)

  type t
  (** The type representing the internal state of the device *)

  type error
  (** An error signalled by the device, normally returned after a
      connection attempt *)

  type id
  (** Type defining an identifier for this device that uniquely
      identifies it among a device tree. *)

  val connect: id -> [ `Error of error | `Ok of t ] io
  (** Connect to the device identified by [id] *)

  val disconnect : t -> unit io
  (** Disconnect from the device.  While this might take some
      time to complete, it can never result in an error. *)
end

module type KV_RO = sig
  (** Static Key/value store. *)

  type error =
    | Unknown_key of string

  include DEVICE
    with type error := error

  (** Abstract type for a page-aligned memory stream. *)
  type page_aligned_stream

  val read: t -> string -> [ `Error of error | `Ok of page_aligned_stream ] io
  (** Read the value associated to a key. *)

  val size: t -> string -> [`Error of error | `Ok of int64] io
  (** Get the value size. *)

end


(** Text console input/output operations. *)
module type CONSOLE = sig
  type error =
    | Invalid_console of string

  include DEVICE with
    type error := error
    and type id = string

  (** [write t buf off len] writes up to [len] chars of [String.sub buf
      off len] to the console [t] and returns the number of bytes
      written. Raises {!Invalid_argument} if [len > buf - off]. *)
  val write : t -> string -> int -> int -> int

  (** [write_all t buf off len] is a thread that writes [String.sub buf
      off len] to the console [t] and returns [len] when done. Raises
      {!Invalid_argument} if [len > buf - off]. *)
  val write_all : t -> string -> int -> int -> int io

  (** [log str] writes as much characters of [str] that can be written
      in one write operation to the default console [t], then writes
      "\r\n" to it. *)
  val log : string -> unit

  (** [log_s str] is a thread that writes [str ^ "\r\n"] in the default
      console [t]. *)
  val log_s : string -> unit io

end

module BLOCK : sig

  module type CLIENT = sig

    (** Abstract type for a page-aligned memory buffer *)
    type page_aligned_buffer

    (** IO operation errors *)
    type error = [
      | `Unknown of string (** an undiagnosed error *)
      | `Unimplemented     (** operation not yet implemented in the code *)
      | `Is_read_only      (** you cannot write to a read/only instance *)
      | `Disconnected      (** the device has been previously disconnected *)
    ]

    include DEVICE with
      type error := error
      and type id = string

    (** Characteristics of the block device. Note some devices may be able
        to make themselves bigger over time. *)
    type info = {
      read_write: bool;    (** True if we can write, false if read/only *)
      sector_size: int;    (** Octets per sector *)
      size_sectors: int64; (** Total sectors per device *)
    }

    (** Query the characteristics of a specific block device *)
    val get_info: t -> info io

    (** [read device sector_start buffers] returns a blocking IO operation which
        attempts to fill [buffers] with data starting at [sector_start].
        Each of [buffers] must be a whole number of sectors in length. The list
        of buffers can be of any length. *)
    val read: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

    (** [write device sector_start buffers] returns a blocking IO operation which
        attempts to write the data contained within [buffers] to [t] starting
        at [sector_start]. When the IO operation completes then all writes have been
        persisted.

        Once submitted, it is not possible to cancel a request and there is no timeout.

        The operation may fail with
        * [`Unimplemented]: the operation has not been implemented, no data has been written
        * [`Is_read_only]: the device is read-only, no data has been written
        * [`Disconnected]: the device has been disconnected at application request,
          an unknown amount of data has been written
        * [`Unknown]: some other permanent, fatal error (e.g. disk is on fire), where
          an unknown amount of data has been written

        Each of [buffers] must be a whole number of sectors in length. The list
        of buffers can be of any length.

        The data will not be copied, so the supplied buffers must not be re-used
        until the IO operation completes. *)
    val write: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

  end

end

module type FS = sig

  (** Abstract type representing an error from the block layer *)
  type block_device_error

  type error = [
    | `Not_a_directory of string             (** Cannot create a directory entry in a file *)
    | `Is_a_directory of string              (** Cannot read or write the contents of a directory *)
    | `Directory_not_empty of string         (** Cannot remove a non-empty directory *)
    | `No_directory_entry of string * string (** Cannot find a directory entry *)
    | `File_already_exists of string         (** Cannot create a file with a duplicate name *)
    | `No_space                              (** No space left on the block device *)
    | `Format_not_recognised of string       (** The block device appears to not be formatted *)
    | `Unknown_error of string
    | `Block_device of block_device_error
  ]

  include DEVICE with
    type error := error

  (** Abstract type for a page-aligned memory buffer *)
  type page_aligned_buffer

  (** Per-file/directory statistics *)
  type stat = {
    filename: string; (** Filename within the enclosing directory *)
    read_only: bool;  (** True means the contents are read-only *)
    directory: bool;  (** True means the entity is a directory; false means a file *)
    size: int64;      (** Size of the entity in bytes *)
  }

  (** [format t size] erases the contents of [t] and creates an empty filesystem
      of size [size] bytes *)
  val format: t -> int64 -> [ `Ok of unit | `Error of error ] io

  (** [create t path] creates an empty file at [path] *)
  val create: t -> string -> [ `Ok of unit | `Error of error ] io

  (** [mkdir t path] creates an empty directory at [path] *)
  val mkdir: t -> string -> [ `Ok of unit | `Error of error ] io

  (** [destroy t path] removes a [path] (which may be a file or an empty
      directory) on filesystem [t] *)
  val destroy: t -> string -> [ `Ok of unit | `Error of error ] io

  (** [stat t path] returns information about file or directory at [path] *)
  val stat: t -> string -> [ `Ok of stat | `Error of error ] io

  (** [listdir t path] returns the names of files and subdirectories
      within the directory [path] *)
  val listdir: t -> string -> [ `Ok of string list | `Error of error ] io

  (** [write t path offset data] writes [data] at [offset] in file [path] on
      filesystem [t] *)
  val write: t -> string -> int -> page_aligned_buffer -> [ `Ok of unit | `Error of error ] io

  (** [read t path offset length] reads up to [length] bytes from file [path] on
      filesystem [t]. If less data is returned than requested, this indicates
      end-of-file. *)
  val read: t -> string -> int -> int -> [ `Ok of page_aligned_buffer list | `Error of error ] io
end


