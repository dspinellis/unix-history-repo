/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
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

/* name.h */
/* Name table entry structure for Ghostscript */

/* Name structure.  The name table is a simple chained hash table. */
/* There is an optimization to avoid lookup for operator and other */
/* global names. */
struct name_s {
	ushort next_index;	/* next name in chain or 0 */
	ushort index;
/* In order to pack names into 16 bytes, we use a non-standard */
/* representation for the string, omitting the type_attrs field. */
	ushort string_size;
	const byte *string_bytes;
/* pvalue specifies the definition status of the name: */
/*	pvalue == pv_no_defn: no definitions */
#define pv_no_defn ((ref *)0)
/*	pvalue == pv_other: other status */
#define pv_other ((ref *)1)
#define pv_valid(pvalue) ((unsigned long)(pvalue) > 1)
/*	pvalue != pv_no_defn, pvalue != pv_other: pvalue is valid */
	ref *pvalue;		/* if only defined in systemdict */
				/* or userdict, this points to */
				/* the value */
};
/*typedef struct name_s name;*/		/* in ghost.h */

/* Procedures for the name table. */

/* The size argument for name_ref should be a ushort, */
/* but this gets the Apollo compiler confused. */
/* enterflag=-1 means don't enter (return an error if missing); */
/* enterflag=0 means enter if missing, don't copy the string; */
/* enterflag=1 means enter if missing, copy the string. */
extern	int	name_ref(P4(const byte *ptr, uint size, ref *pnref, int enterflag));
extern	void	name_string_ref(P2(const ref *pnref, ref *psref));
extern	void	name_enter(P2(const char *str, ref *pnref));
/* name_from_string essentially implements cvn. */
/* It always enters the name, and copies the executable attribute. */
extern	int	name_from_string(P2(const ref *psref, ref *pnref));

/* Conversion between names and indices. */
#define name_index(pnref) ((pnref)->value.pname->index)
extern	void	name_index_ref(P2(uint index /* should be ushort */,
			          ref *pnref));
extern	uint	name_count(P0());
extern	int	name_is_since_count(P2(ref *, uint));

/* Clean up the name table before a restore. */
extern	void	name_restore(P1(uint old_count));
