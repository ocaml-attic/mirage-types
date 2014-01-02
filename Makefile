# -*- mode: Makefile -*-
#
# Copyright (c) 2013 Richard Mortier <mort@cantab.net>
#
# Permission to use, copy, modify, and distribute this software for any purpose
# with or without fee is hereby granted, provided that the above copyright
# notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.
#

.PHONY: all install clean remove

OCAML=ocamlc -bin-annot -c
OCAMLFIND=ocamlfind

all: lib/v1.cmi lib/v1.cmti

## note order-only prereq (after |): forces removal before install
install: lib/META lib/v1.cmi lib/v1.cmti | remove
	$(OCAMLFIND) install mirage-types $^

clean:
	$(RM) lib/v1.cmt lib/v1.cmti lib/v1.cmi

remove:
	$(OCAMLFIND) remove mirage-types || true

%.cmi %.cmti: %.mli
	$(OCAML) $<
