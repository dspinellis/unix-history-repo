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

/* gschar0.c */
/* Composite font decoding for Ghostscript library */
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"			/* for gzstate.h */
#include "gzstate.h"			/* must precede gzdevice */
#include "gzdevice.h"			/* must precede gxchar */
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxfont.h"

/* Get the next character and font from a composite string. */
/* If we run off the end of the string in the middle of a */
/* multi-byte sequence, return gs_error_rangecheck. */
/* If the string is empty, return 1.  Otherwise, return 0. */
int
gs_type0_next(gs_show_enum *penum, ulong *pchar)
{	uint index = penum->index;
	byte *p = penum->str + index;
	uint size = penum->size;
	uint left;
	gs_font *pfont = penum->fstack[penum->fdepth];
	gs_type0_data *pdata = &pfont->data.type0_data;
	uint fidx;
	int modal_mapping;
top:	if ( (left = size - index) == 0 ) return 1;
#define need_left(n)\
  if ( left < n ) return_error(gs_error_rangecheck)
	switch ( pdata->FMapType )
	   {
	default:			/* can't happen */
		return_error(gs_error_invalidfont);

		/* ------ Non-modal mappings ------ */

	case 2:				/* 8/8 mapping */
		modal_mapping = 0;
		need_left(2);
		fidx = *p++;
		*pchar = *p++;
		break;

	case 4:				/* 1/7 mapping */
		modal_mapping = 0;
		fidx = *p >> 7;
		*pchar = *p++ & 0x7f;
		break;

	case 5:				/* 9/7 mapping */
		modal_mapping = 0;
		need_left(2);
		fidx = ((uint)*p << 1) + (p[1] >> 7);
		*pchar = p[1] & 0x7f;
		p += 2;
		break;

	case 6:				/* SubsVector mapping */
		modal_mapping = 0;
	   {	uint subs_count = pdata->subs_size;
		byte *psv = pdata->SubsVector;
		need_left(pdata->subs_width);
#define subs_loop(subs_elt, width)\
  while ( subs_count != 0 && chr >= (schr = subs_elt) )\
    subs_count--, chr -= schr, psv += width;\
  *pchar = chr; break
		switch ( pdata->subs_width )
		   {
		default:		/* can't happen */
			return_error(gs_error_invalidfont);
		case 1:
		   {	byte chr = *p, schr;
			subs_loop(*psv, 1);
		   }
		case 2:
#define w2(p) (((ushort)*p << 8) + p[1])
		   {	ushort chr = w2(p), schr;
			subs_loop(w2(psv), 2);
		   }
#undef w2
		case 3:
#define w3(p) (((ulong)*p << 16) + ((uint)p[1] << 8) + p[2])
		   {	ulong chr = w3(p), schr;
			subs_loop(w3(psv), 3);
		   }
#undef w3
		case 4:
#define w4(p)\
  (((ulong)*p << 24) + ((ulong)p[1] << 16) + ((uint)p[1] << 8) + p[2])
		   {	ulong chr = w4(p), schr;
			subs_loop(w4(psv), 4);
		   }
#undef w4
#undef subs_loop
		   }
		p += pdata->subs_width;
		break;
	   }

		/* ------ Modal mappings ------ */

	case 3:				/* escape mapping */
		modal_mapping = 1;
		if ( *p == pdata->EscChar )
		   {	need_left(2);
			fidx = p[1]; p += 2; goto remap;
		   }
		goto modal;

	case 7:				/* double escape mapping */
		modal_mapping = 1;
		if ( *p == pdata->EscChar )
		   {	need_left(2);
			if ( p[1] == pdata->EscChar )
			   {	need_left(3);
				fidx = p[2] + 256; p += 3; goto remap;
			   }
			fidx = p[1]; p += 2; goto remap;
		   }
		goto modal;

	case 8:				/* shift mapping */
		modal_mapping = 1;
		if ( *p == pdata->ShiftIn )
		   {	fidx = 0; p++; goto remap;
		   }
		else if ( *p == pdata->ShiftOut )
		   {	fidx = 1; p++; goto remap;
		   }
modal:		*pchar = *p++;
		penum->index = p - penum->str;
		return 0;

	   }

	/* Control continues here for non-modal mappings, */
	/* or for modal mappings after a font change. */
remap:	if ( fidx >= pdata->encoding_size )
		return_error(gs_error_rangecheck);
	fidx = pdata->Encoding[fidx];
	penum->index = p - penum->str;
	/* We pre-checked the encoding vector, so we know that */
	/* fidx is now a legal subscript for FDepVector. */
	pfont = pdata->FDepVector[fidx];
	if ( modal_mapping )
	   {	if ( penum->index == 0 ) return 1;
		if ( pfont->FontType == ft_composite )
		   {	penum->fstack[++penum->fdepth] = pfont;
			goto top;
		   }
		*pchar = *p++;
		penum->index--;
	   }
	else
	   {	penum->pfont = pfont;
	   }
	return 0;
}
