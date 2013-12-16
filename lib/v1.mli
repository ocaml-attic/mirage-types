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

  type buffer
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

  val to_cstruct : t -> buffer
  (** [to_cstruct t] exposes the contents of a page as a C buffer. *)

  val to_string : t -> string
  (** [to_string t] dump the contents of a page in a string. *)

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

  val id : t -> id
  (** Return the identifier that was used to construct this device *)

  val connect: id -> [ `Error of error | `Ok of t ] io
  (** Connect to the device identified by [id] *)

  val disconnect : t -> unit io
  (** Disconnect from the device.  While this might take some
      time to complete, it can never result in an error. *)

end

type console_error = [
  | `Invalid_console of string
]

module type CONSOLE = sig

  (** Text console input/output operations. *)

  include DEVICE with type error = console_error

  val write : t -> string -> int -> int -> int
  (** [write t buf off len] writes up to [len] chars of [String.sub buf
      off len] to the console [t] and returns the number of bytes
      written. Raises {!Invalid_argument} if [len > buf - off]. *)

  val write_all : t -> string -> int -> int -> unit io
  (** [write_all t buf off len] is a thread that writes [String.sub buf
      off len] to the console [t] and returns when done. Raises
      {!Invalid_argument} if [len > buf - off]. *)

  val log : t -> string -> unit
  (** [log str] writes as much characters of [str] that can be written
      in one write operation to the console [t], then writes
      "\r\n" to it. *)

  val log_s : t -> string -> unit io
  (** [log_s str] is a thread that writes [str ^ "\r\n"] in the
      console [t]. *)

  val fprintf: t -> ('a, t, unit) format -> 'a
  (** [printf c fmt a1 ... an] formats the arguments [a1] to [an]
      according to the format string [format] and output the resulting
      string on the console [c]. *)

end

type block_error = [
  | `Unknown of string (** an undiagnosed error *)
  | `Unimplemented     (** operation not yet implemented in the code *)
  | `Is_read_only      (** you cannot write to a read/only instance *)
  | `Disconnected      (** the device has been previously disconnected *)
]
(** IO operation errors. *)

module type BLOCK = sig

  (** Block-device operations. *)

  type page_aligned_buffer
  (** Abstract type for a page-aligned memory buffer *)

  include DEVICE with type error = block_error

  type info = {
    read_write: bool;    (** True if we can write, false if read/only *)
    sector_size: int;    (** Octets per sector *)
    size_sectors: int64; (** Total sectors per device *)
  }
  (** Characteristics of the block device. Note some devices may be able
      to make themselves bigger over time. *)

  val get_info: t -> info io
  (** Query the characteristics of a specific block device *)

  val read: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
  (** [read device sector_start buffers] returns a blocking IO operation which
      attempts to fill [buffers] with data starting at [sector_start].
      Each of [buffers] must be a whole number of sectors in length. The list
      of buffers can be of any length. *)

  val write: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
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

end

type kv_ro_error =  [
  | `Not_a_directory of string             (** Cannot create a directory entry in a file *)
  | `Is_a_directory of string              (** Cannot read or write the contents of a directory *)
  | `No_directory_entry of string * string (** Cannot find a directory entry *)
  | `Format_not_recognised of string       (** The block device appears to not be formatted *)
  | `Unknown_error of string
  | `Block_device of block_error
]
(** IO errors for read-only key/value stores. *)

module type FS_RO_0 = sig

  (** Read-only key/value store. *)

  include DEVICE

  type page_aligned_buffer
  (** Abstract type for a page-aligned memory buffer *)

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

  val size: t -> string -> [`Error of error | `Ok of int64] io
  (** Get the value size. *)

end

module type FS_RO = FS_RO_0 with type error = kv_ro_error

type network_error = [
  | `Unknown of string (** an undiagnosed error *)
  | `Unimplemented     (** operation not yet implemented in the code *)
  | `Disconnected      (** the device has been previously disconnected *)
]
(** IO operation errors for network implementations. *)

module type NETWORK = sig

  (** Abstract type for a page-aligned memory buffer *)
  type page_aligned_buffer

  (** Abstract type for a memory buffer that may not be page aligned *)
  type buffer

  (** Unique MAC identifier for the device *)
  type macaddr

  include DEVICE with type error = network_error

  val write : t -> buffer -> unit io
  (** [write nf buf] outputs [buf] to netfront [nf]. *)

  val writev : t -> buffer list -> unit io
  (** [writev nf bufs] output a list of buffers to netfront [nf] as a
      single packet. *)

  val listen : t -> (buffer -> unit io) -> unit io
  (** [listen nf fn] is a blocking operation that calls [fn buf] with
      every packet that is read from the interface.  It returns as soon
      as it has initialised, and the function can be stopped by calling
      [disconnect] in the device layer. *)

  val mac : t -> macaddr
  (** [mac nf] is the MAC address of [nf]. *)

  type stats = {
    mutable rx_bytes : int64;
    mutable rx_pkts : int32;
    mutable tx_bytes : int64;
    mutable tx_pkts : int32;
  }

  val get_stats_counters : t -> stats
  val reset_stats_counters : t -> unit
end

type fs_error = [
  | `Directory_not_empty of string         (** Cannot remove a non-empty directory *)
  | `No_space                              (** No space left on the block device *)
  | `File_already_exists of string         (** Cannot create a file with a duplicate name *)
]
(** IO error operations for filesystems. *)

module type FS = sig

  (** Filesystems. *)

  include FS_RO_0 with type error = [ fs_error | kv_ro_error]

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

  (** [write t path offset data] writes [data] at [offset] in file [path] on
      filesystem [t] *)
  val write: t -> string -> int -> page_aligned_buffer -> [ `Ok of unit | `Error of error ] io

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
