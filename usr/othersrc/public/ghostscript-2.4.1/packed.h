/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* packed.h */
/* Packed array format for Ghostscript */

/*

Packed elements come in 2 different sizes, 2 bytes or 8 bytes.  The
top bits of the first 16-bit subelement distinguish the 2 forms.  The
'size' of a packed array is the number of elements, not the number of
bytes it occupies.  The encoding:

	00-ttttt texrwsnm	full 8-byte object
	010jjjjj jjjjjjjj	executable operator (for bind)
	011vvvvv vvvvvvvv	integer (biased by packed_min_intval)
	10iiiiii iiiiiiii	literal name
	11iiiiii iiiiiiii	executable name

The jjj index of executable operators is either the index of the
operator in the op_def_table, if the index is less than op_def_count,
or the index of the definition in the op_array_table (subtracting
op_def_count first).

The iii index of names is the one that the name machinery already
maintains.  A name whose index is larger than 16K must be represented
as a full 8-byte element.

There are actually two packed array types, t_mixedarray and
t_shortarray.  A t_mixedarray can have a mix of 2- and 8-bit
elements; a t_shortarray has all 2-byte elements.  In both cases, the
`size' is the number of elements.

Packed array elements can be distinguished from full-size elements,
so we allow the interpreter to simply execute all the different kinds
of arrays directly.  In theory, this could lead to unaligned accesses
to full-size (8-byte) refs.  Some machines can't handle unaligned
accesses of this kind.  Rather than try to tailor the algorithms to
the machine's capabilities, we guarantee that full-size elements in
mixed arrays are always properly aligned.  We do this by converting
up to 3 preceding 2-byte elements into 8-byte elements so that the
alignment is preserved.  The only code this actually affects is in
make_packed_array.  However, the save/restore machinery in isave.c is
sometimes used to save changes to packed arrays, and it currently
only knows how to save full-size, aligned refs.

Note that code in zpacked.c and interp.c knows more about the
representation of packed elements than the definitions in this file
would imply.  Read the code carefully if you change the
representation.

 */

typedef ushort ref_packed;

#define packed_type_shift 13
typedef enum {
	pt_full_ref = 0,
#define pt_min_packed 2
	pt_executable_operator = 2,
	pt_integer = 3,
	pt_literal_name = 4,
	pt_executable_name = 6
} packed_type;
#define pt_tag(pt) ((ref_packed)(pt) << packed_type_shift)
#define r_is_packed(rp)  (*(ref_packed *)(rp) >= pt_tag(pt_min_packed))
#define packed_ref_is_name(prp) (*(prp) >= pt_tag(pt_literal_name))
#define packed_max_full_ref ((2 << packed_type_shift) - 1)
#define packed_min_intval (-(1 << (packed_type_shift - 1)))
#define packed_max_intval ((1 << (packed_type_shift - 1)) - 1)
#define packed_int_mask ((1 << packed_type_shift) - 1)
#define packed_max_name_index ((1 << (packed_type_shift + 1)) - 1)

/* Procedures implemented in zpacked.c */
#define packed_per_ref (sizeof(ref) / sizeof(ref_packed))

/* Make a packed array */
extern	int	make_packed_array(P4(ref *, uint, ref *, const char *));

/* Get an element from a packed array. */
extern	void	packed_get(P2(const ref_packed *, ref *));
/* Advance to the next element in a packed array. */
#define packed_next(packed)\
  (*packed <= packed_max_full_ref ? packed + packed_per_ref : packed + 1)
