/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* gsutil.h */
/* Prototypes for procedures in gsutil.c */

/* ------ String utilities ------ */

/* Compare two strings, returning -1 if the first is less, */
/* 0 if they are equal, and 1 if first is greater. */
/* We can't use memcmp, because we always use unsigned characters. */
extern int bytes_compare(P4(const byte *, uint, const byte *, uint));

/* Test whether a string matches a pattern with wildcards. */
/* '*' = any substring, '?' = any character, '\' quotes next character. */
extern int string_match(P5(const byte *str, uint len, const byte *pstr, uint plen, int ignore_case));

/* Compute a hash for a string */
extern uint string_hash(P2(const byte *, uint));

/* ------ Property list utilities ------ */

/*
 * The recipient of a property list typically wants to extract a set of
 * known properties and disregard the rest.  The following procedure
 * accomplishes this.
 */
#ifndef gs_prop_item_s_DEFINED
struct gs_prop_item_s;
#endif
extern int props_extract(P6(
	struct gs_prop_item_s *plist,	/* plist[count] */
	int count,
	const struct gs_prop_item_s *template, /* template[tcount] */
					/* (the known properties) */
	int tcount,
	struct gs_prop_item_s **pknown,	/* *pknown[tcount] */
					/* (these are set to point to the */
					/* corresponding items in plist, */
					/* or NULL if the item is absent) */
	int finished			/* if true, mark remaining items */
					/* in plist as pv_unknown */
));
