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

/* zfont2.c */
/* Font creation operator for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gxfixed.h"
#include "gsmatrix.h"
#include "gxdevice.h"
#include "gschar.h"
#include "gxfont.h"
#include "alloc.h"
#include "dict.h"
#include "font.h"
#include "name.h"
#include "packed.h"
#include "store.h"

/* Imported procedures */
int add_FID(P2(ref *, gs_font *));	/* in zfont */

/* Forward references */
int font_int_param(P6(ref *pdict, ref *pname, int minval, int maxval, int defaultval, int *pvalue));
int build_gs_simple_font(P4(os_ptr, gs_font **, font_type, ref *));
int build_gs_font(P4(os_ptr, gs_font **, font_type, ref *));	/* buildfont0 needs this */

/* Global font-related objects */
/* Names of system-known keys in font dictionaries: */
extern ref name_FID;			/* in zfont.c */
extern ref name_FontMatrix;		/* in zfont.c */
private ref name_FontType;
private ref name_WMode;
private ref name_FontBBox;
private ref name_Encoding;
ref name_UniqueID;			/* used by zfont1 */
private ref name_BuildChar;

/* The global font directory */
extern gs_font_dir *ifont_dir;

/* Initialize the font building operators */
private void
zfont2_init()
{	static const names_def fnd2[] = {

	/* Create the names of the standard elements of */
	/* a font dictionary. */
	   { "FontType", &name_FontType },
	   { "WMode", &name_WMode },
	   { "FontBBox", &name_FontBBox },
	   { "Encoding", &name_Encoding },
	   { "UniqueID", &name_UniqueID },
	   { "BuildChar", &name_BuildChar },

	/* Mark the end of the initalized name list. */
	   names_def_end
	};

	init_names(fnd2);
}

/* .buildfont3 */
/* Build a type 3 (user-defined) font. */
int
zbuildfont3(os_ptr op)
{	int code;
	ref *pbuildchar;
	gs_font *pfont;
	check_type(*op, t_dictionary);
	code = dict_find(op, &name_BuildChar, &pbuildchar);
	if ( code <= 0 ) return e_invalidfont;
	check_proc(*pbuildchar);
	return build_gs_simple_font(op, &pfont, ft_user_defined, pbuildchar);
}

/* ------ Initialization procedure ------ */

op_def zfont2_op_defs[] = {
	{"1.buildfont3", zbuildfont3},
	op_def_end(zfont2_init)
};

/* ------ Subroutines ------ */

/* Get an integer parameter from a font-related dictionary. */
/* Return 0 if found, 1 if defaulted, <0 if missing or out of range. */
int
font_int_param(ref *pdict, ref *pname, int minval, int maxval,
  int defaultval, int *pvalue)
{	ref *pdval;
	if ( dict_find(pdict, pname, &pdval) <= 0 )
	   {	*pvalue = defaultval;
		return 1;
	   }
	if ( !r_has_type(pdval, t_integer) )
		return e_typecheck;
	if ( pdval->value.intval < minval || pdval->value.intval > maxval )
		return e_rangecheck;
	*pvalue = (int)pdval->value.intval;
	return 0;
}		

/* Do the common work for building a font of any non-composite FontType. */
/* The caller guarantees that *op is a dictionary. */
int
build_gs_simple_font(os_ptr op, gs_font **ppfont, font_type ftype, ref *pbuildchar)
{	ref *pbbox;
	float bbox[4];
	long unique_id;
	ref *puniqueid;
	int code;
	gs_font *pfont;
	if ( dict_find(op, &name_FontBBox, &pbbox) <= 0 )
	  return e_invalidfont;
	switch ( r_type(pbbox) )
	   {
	default: return e_invalidfont;
	case t_array: case t_mixedarray: case t_shortarray: ;
		if ( r_size(pbbox) != 4 ) return e_invalidfont;
	   }
	   {	ushort *pbe = pbbox->value.packed;
		ref rbe[4];
		int i;
		for ( i = 0; i < 4; i++ )
		   {	packed_get(pbe, rbe + i);
			pbe = packed_next(pbe);
		   }
		if ( num_params(rbe + 3, 4, bbox) < 0 )
			return e_invalidfont;
	   }
	/* If no UniqueID entry, set the UniqueID member to -1, */
	/* because UniqueID need not be present in all fonts, */
	/* and if it is, the legal range is 0 to 2^24-1. */
	if ( dict_find(op, &name_UniqueID, &puniqueid) <= 0 )
		unique_id = -1;
	else
	   {	if ( !r_has_type(puniqueid, t_integer) ||
		     puniqueid->value.intval < 0 ||
		     puniqueid->value.intval > ((1L << 24) - 1)
		   )
			return e_invalidfont;
		unique_id = puniqueid->value.intval;
	   }
	code = build_gs_font(op, ppfont, ftype, pbuildchar);
	if ( code != 0 ) return code;	/* invalid or scaled font */
	pfont = *ppfont;
	pfont->data.base.FontBBox.p.x = bbox[0];
	pfont->data.base.FontBBox.p.y = bbox[1];
	pfont->data.base.FontBBox.q.x = bbox[2];
	pfont->data.base.FontBBox.q.y = bbox[3];
	pfont->data.base.UniqueID = unique_id;
	return 0;
}

/* Do the common work for building a font of any FontType. */
/* The caller guarantees that *op is a dictionary. */
/* Return 0 for a new font, 1 for a font made by makefont or scalefont, */
/* or a negative error code. */
int
build_gs_font(os_ptr op, gs_font **ppfont, font_type ftype, ref *pbuildchar)
{	ref *pftype;
	ref *pmatrix;
	gs_matrix mat;
	ref *pencoding;
	int wmode;
	int code;
	gs_font *pfont;
	ref *pfid;
	ref *aop = dict_access_ref(op);
	if ( dict_find(op, &name_FontType, &pftype) <= 0 ||
	    !r_has_type(pftype, t_integer) ||
	    pftype->value.intval != (int)ftype ||
	    dict_find(op, &name_FontMatrix, &pmatrix) <= 0 ||
	    dict_find(op, &name_Encoding, &pencoding) <= 0 ||
	    read_matrix(pmatrix, &mat) < 0
	   )
	  return e_invalidfont;
	switch ( r_type(pencoding) )
	   {
	default: return e_invalidfont;
	case t_array: case t_mixedarray: case t_shortarray: ;
	   }
	code = font_int_param(op, &name_WMode, 0, 1, 0, &wmode);
	if ( code < 0 ) return code;
	code = dict_find(op, &name_FID, &pfid);
	if ( r_has_attr(aop, a_write) )
	   {	/* Assume this is a new font */
		font_data *pdata;
		if ( code > 0 ) return e_invalidfont;	/* has FID already */
		if ( (pfont = (gs_font *)alloc(1, sizeof(gs_font), "buildfont(font)")) == 0 ||
		     (pdata = (font_data *)alloc(1, sizeof(font_data), "buildfont(data)")) == 0
		   )
		  return e_VMerror;
		if ( (code = add_FID(op, pfont)) < 0 ) return code;
		ref_assign(&pdata->dict, op);
		ref_assign(&pdata->BuildChar, pbuildchar);
		ref_assign(&pdata->Encoding, pencoding);
		pfont->base = pfont;
		pfont->dir = ifont_dir;
		pfont->client_data = (char *)pdata;
		pfont->FontType = ftype;
		pfont->FontMatrix = mat;
		pfont->WMode = wmode;
		pfont->build_char_proc = gs_no_build_char_proc;
	   }
	else
	   {	/* Assume this was made by makefont or scalefont */
		if ( code <= 0 || !r_has_type(pfid, t_fontID) )
			return e_invalidfont;
		pfont = pfid->value.pfont;
	   }
	*ppfont = pfont;
	return 0;
}
