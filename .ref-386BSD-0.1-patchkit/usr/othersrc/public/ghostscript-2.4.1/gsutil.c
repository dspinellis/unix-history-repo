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

/* gsutil.c */
/* Utilities for Ghostscript library */
#include "std.h"
#include "string_.h"
#include "memory_.h"
#include "gsprops.h"
#include "gsutil.h"			/* for prototype checking */

/* ------ String utilities ------ */

/* Compare two strings, returning -1 if the first is less, */
/* 0 if they are equal, and 1 if first is greater. */
/* We can't use memcmp, because we always use unsigned characters. */
int
bytes_compare(const byte *s1, uint len1, const byte *s2, uint len2)
{	register uint len = len1;
	if ( len2 < len ) len = len2;
	   {	register const byte *p1 = s1;
		register const byte *p2 = s2;
		while ( len-- )
			if ( *p1++ != *p2++ )
				return (p1[-1] < p2[-1] ? -1 : 1);
	   }
	/* Now check for differing lengths */
	return (len1 == len2 ? 0 : len1 < len2 ? -1 : 1);
}

/* Test whether a string matches a pattern with wildcards. */
/* '*' = any substring, '?' = any character, '\' quotes next character. */
int
string_match(const byte *str, uint len, const byte *pstr, uint plen,
  int ignore_case)
{	const byte *pback = 0;
	const byte *p = pstr, *pend = pstr + plen;
	const byte *sp = str, *spend = str + len;
	uint matched = 0;
	while ( p < pend )
	   {	byte ch = *p;
		switch ( ch )
		   {
		case '*':
			pback = ++p, matched = 0;
			continue;
		case '?':
			if ( sp == spend ) return 0;	/* str too short */
			p++, sp++, matched++;
			continue;
		case '\\':
			if ( ++p == pend ) return 1;	/* bad pattern */
			ch = *p;
		   }
		if ( sp == spend ) return 0;	/* str too short */
		if ( *sp == ch || ignore_case && (*sp ^ ch) == 0x20 &&
		     (ch &= ~0x20) >= 0x41 && ch <= 0x5a
		   )
			p++, sp++, matched++;
		else if ( pback == 0 )
			return 0;	/* no * to back up to */
		else
		   {	sp += 1 - matched;
			p = pback;
			matched = 0;
		   }
	   }
	return 1;
}

/* Compute a hash for a string */
uint
string_hash(const byte *ptr, uint len)
{	register const byte *p = ptr;
	register uint hash = 0;
	register uint n = len;
	while ( n-- ) hash = hash * 19 + *p++;
	return hash;
}

/* ------ Property list utilities ------ */

/* Extract known properties from a list.  See gsutil.h for more details. */
int
props_extract(gs_prop_item *plist, int count,
  const gs_prop_item *template, int tcount,
  gs_prop_item **pknown, int finished)
{	const gs_prop_item *pti;
	gs_prop_item **ppi;
	gs_prop_item *pli;
	int i, j;
	for ( pti = template, ppi = pknown, j = 0; j < tcount; pti++, ppi++, j++ )
	   {	const char *tstr = pti->pname;
		int tlen = pti->name_size;
		if ( tstr == 0 ) continue;	/* no name */
		if ( tlen < 0 ) tlen = strlen(tstr);
		*ppi = 0;
		for ( pli = plist, i = 0; i < count; pli++, i++ )
		  if ( pli->status == pv_set )
		   {	const char *lstr = pli->pname;
			int llen = pli->name_size;
			if ( lstr == 0 ) continue;
			if ( llen < 0 ) llen = strlen(lstr);
			if ( llen == tlen && !memcmp(tstr, lstr, llen) )
			   {	/* Names match, check types. */
				if ( pli->type != pti->type )
				 { /* Check for int [array] -> float [array] */
				   if ( pli->type == prt_int &&
					pti->type == prt_float
				      )
				     pli->type = prt_float,
				     pli->value.f = pli->value.i;
				   else if ( pli->type == prt_int_array &&
					     pti->type == prt_float_array
					   )
				    { int i;
				      gs_prop_item *vp = pli->value.a.p.v;
				      pli->type = prt_float_array;
				      for ( i = pli->value.a.size; --i >= 0; vp++ )
					vp->type = prt_float,
					vp->value.f = vp->value.i;
				    }
				   else
				    { pli->status = pv_typecheck;
				      break;
				    }
				 }
				*ppi = pli, pli->status = pv_OK;
				break;
			   }
		   }
	   }
	if ( finished )
	  for ( pli = plist, i = 0; i < count; pli++, i++ )
	    if ( pli->status != pv_OK ) pli->status = pv_unknown;
	return 0;
}
