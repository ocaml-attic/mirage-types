(*
 * Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org>
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

  type buf

  (** Memory allocation. *)

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

(** Text console input/output operations. *)
module type CONSOLE = sig

  (** Abstract type of a console instance. *)
  type t

  (** Abstract type for a blocking IO operation *)
  type 'a io

  (** [create ()] creates an additional console. Not implemented yet. *)
  val create : unit -> t

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

module type BLOCK_DEVICE = sig

  (** Abstract type of a block device instance. *)
  type t

  (** Abstract type for a blocking IO operation *)
  type 'a io

  (** Abstract type for a page-aligned memory buffer *)
  type page_aligned_buffer

  (** IO operation errors *)
  type error =
  | Unknown (** an undiagnosed error *)

  (** Characteristics of the block device. Note some devices may be able
      to make themselves bigger over time. *)
  type info = {
    read_write: bool;    (** True if we can write, false if read/only *)
    sector_size: int;    (** Octets per sector *)
    size_sectors: int64; (** Total sectors per device *)
  }

  (** Query the characteristics of a specific block device *)
  val get_info: t -> info io

  (** [read t sector_start buffers] returns a blocking IO operation which
      attempts to fill [buffers] with data starting at [sector_start]. *)
  val read: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

  (** [write t sector_start buffers] returns a blocking IO operation which
      attempts to write the data contained within [buffers] to [t] starting
      at [sector_start]. If an error occurs then the write may have partially
      succeeded. *)
  val write: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

end
