/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* zfont1.c */
/* Type 1 font creation operator for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gxfixed.h"
#include "gsmatrix.h"
#include "gxdevice.h"
#include "gschar.h"
#include "gxfont.h"
#include "dict.h"
#include "font.h"
#include "name.h"
#include "store.h"

/* Imported procedures from zfont2.c */
extern int build_gs_simple_font(P4(os_ptr, gs_font **, font_type, ref *));
extern int font_int_param(P6(ref *pdict, ref *pname, int minval, int maxval,
			     int defaultval, int *pvalue));

/* Forward references */
private int near font_bool_param(P4(ref *pdict, ref *pname,
				    int defaultval, int *pvalue));
private int near font_float_param(P4(ref *pdict, ref *pname,
				     floatp defaultval, float *pvalue));
private int near font_int_array(P4(ref *pdict, ref *pname,
				   uint maxlen, int *ivec));
private int near font_float_array(P4(ref *pdict, ref *pname,
				     uint maxlen, float *fvec));

/* Type 1 auxiliary procedures (defined in zchar.c) */
extern int z1_subr_proc(P3(gs_type1_data *, int, byte **));
extern int z1_pop_proc(P2(gs_type1_data *, fixed *));

/* Names of system-known keys in font dictionaries: */
private ref name_PaintType;
extern ref name_UniqueID;		/* in zfont2 */
private ref name_Type1BuildChar;
private ref name_Private;
private ref name_CharStrings;		/* only needed for seac */
/* Names of system-known keys in type 1 font Private dictionary: */
private ref name_BlueFuzz;
private ref name_BlueScale;
private ref name_BlueShift;
private ref name_BlueValues;
private ref name_ExpansionFactor;
private ref name_FamilyBlues;
private ref name_FamilyOtherBlues;
private ref name_ForceBold;
private ref name_LanguageGroup;
private ref name_lenIV;
private ref name_OtherBlues;
private ref name_RndStemUp;
private ref name_StdHW;
private ref name_StdVW;
private ref name_StemSnapH;
private ref name_StemSnapV;
private ref name_Subrs;

/* Default value of lenIV */
#define default_lenIV 4

/* Initialize the font building operators */
private void
zfont1_init()
{	static names_def fnd1[] = {

	/* Create the names of the standard elements of */
	/* a font dictionary. */
	   { "PaintType", &name_PaintType },
	   { "Type1BuildChar", &name_Type1BuildChar },
	   { "Private", &name_Private },
	   { "CharStrings", &name_CharStrings },

	/* Create the names of the known entries in */
	/* a type 1 font Private dictionary. */
	   { "BlueFuzz", &name_BlueFuzz },
	   { "BlueScale", &name_BlueScale },
	   { "BlueShift", &name_BlueShift },
	   { "BlueValues", &name_BlueValues },
	   { "ExpansionFactor", &name_ExpansionFactor },
	   { "FamilyBlues", &name_FamilyBlues },
	   { "FamilyOtherBlues", &name_FamilyOtherBlues },
	   { "ForceBold", &name_ForceBold },
	   { "LanguageGroup", &name_LanguageGroup },
	   { "lenIV", &name_lenIV },
	   { "OtherBlues", &name_OtherBlues },
	   { "RndStemUp", &name_RndStemUp },
	   { "StdHW", &name_StdHW },
	   { "StdVW", &name_StdVW },
	   { "StemSnapH", &name_StemSnapH },
	   { "StemSnapV", &name_StemSnapV },
	   { "Subrs", &name_Subrs },

	/* Mark the end of the initalized name list. */
	   names_def_end
	};

	init_names(fnd1);

	/* Make the standard BuildChar procedures executable. */
	r_set_attrs(&name_Type1BuildChar, a_executable);
}

/* .buildfont1 */
/* Build a type 1 (Adobe encrypted) font. */
int
zbuildfont1(os_ptr op)
{	gs_type1_data data1;
	ref *psubrs;
	ref *pcharstrings;
	ref *pprivate;
	ref *puniqueid;
	static ref no_subrs;
	gs_font *pfont;
	font_data *pdata;
	int code;
	check_type(*op, t_dictionary);
	code = font_int_param(op, &name_PaintType, 0, 3, 0, &data1.PaintType);
	if ( code < 0 ) return 0;
	if ( dict_find(op, &name_CharStrings, &pcharstrings) <= 0 ||
	    !r_has_type(pcharstrings, t_dictionary) ||
	    dict_find(op, &name_Private, &pprivate) <= 0 ||
	    !r_has_type(pprivate, t_dictionary)
	   )
		return e_invalidfont;
	if ( dict_find(pprivate, &name_Subrs, &psubrs) > 0 )
	   {	check_array_else(*psubrs, e_invalidfont);
	   }
	else
	   {	make_tasv(&no_subrs, t_string, 0, 0, bytes, (byte *)0),
		psubrs = &no_subrs;
	   }
	/* Get the rest of the information from the Private dictionary. */
	if ( (code = font_int_param(pprivate, &name_lenIV, 0, 255,
				    default_lenIV, &data1.lenIV)) < 0 ||
	     (code = font_int_param(pprivate, &name_BlueFuzz, 0, 1999, 1,
				    &data1.BlueFuzz)) < 0 ||
	     (code = font_float_param(pprivate, &name_BlueScale, 0.04379,
				      &data1.BlueScale)) < 0 ||
	     (code = font_int_param(pprivate, &name_BlueShift, 0, 1999, 7,
				    &data1.BlueShift)) < 0 ||
	     (code = data1.BlueValues.count = font_int_array(pprivate,
		&name_BlueValues, max_BlueValues * 2,
		&data1.BlueValues.data[0])) < 0 ||
	     (code = font_float_param(pprivate, &name_ExpansionFactor, 0.06,
				      &data1.ExpansionFactor)) < 0 ||
	     (code = data1.FamilyBlues.count = font_int_array(pprivate,
		&name_FamilyBlues, max_FamilyBlues * 2,
		&data1.FamilyBlues.data[0])) < 0 ||
	     (code = data1.FamilyOtherBlues.count = font_int_array(pprivate,
		&name_FamilyOtherBlues, max_FamilyOtherBlues * 2,
		&data1.FamilyOtherBlues.data[0])) < 0 ||
	     (code = font_bool_param(pprivate, &name_ForceBold, 0,
				     &data1.ForceBold)) < 0 ||
	     (code = font_int_param(pprivate, &name_LanguageGroup, 0, 1, 0,
				    &data1.LanguageGroup)) < 0 ||
	     (code = data1.OtherBlues.count = font_int_array(pprivate,
		&name_OtherBlues, max_OtherBlues * 2,
		&data1.OtherBlues.data[0])) < 0 ||
	     (code = font_bool_param(pprivate, &name_RndStemUp, 0,
				     &data1.RndStemUp)) < 0 ||
	     (code = data1.StdHW.count = font_float_array(pprivate,
		&name_StdHW, 1, &data1.StdHW.data[0])) < 0 ||
	     (code = data1.StdVW.count = font_float_array(pprivate,
		&name_StdVW, 1, &data1.StdVW.data[0])) < 0 ||
	     (code = data1.StemSnapH.count = font_float_array(pprivate,
		&name_StemSnapH, max_StemSnap,
		&data1.StemSnapH.data[0])) < 0 ||
	     (code = data1.StemSnapV.count = font_float_array(pprivate,
		&name_StemSnapV, max_StemSnap,
		&data1.StemSnapV.data[0])) < 0
	   )
		return code;
	/* Do the work common to all non-composite font types. */
	code = build_gs_simple_font(op, &pfont, ft_encrypted, &name_Type1BuildChar);
	if ( code != 0 ) return code;
	/* This is a new font, fill it in. */
	pdata = (font_data *)pfont->client_data;
	pfont->data.base.type1_data = data1;
	ref_assign(&pdata->CharStrings, pcharstrings);
	ref_assign(&pdata->Subrs, psubrs);
	pfont->data.base.type1_data.subr_proc = z1_subr_proc;
	pfont->data.base.type1_data.pop_proc = z1_pop_proc;
	pfont->data.base.type1_data.proc_data = (char *)pdata;
	/* Check that the UniqueIDs match.  This is part of the */
	/* Adobe protection scheme, but we may as well emulate it. */
	if ( pfont->data.base.UniqueID >= 0 )
	   {	if ( dict_find(pprivate, &name_UniqueID, &puniqueid) <= 0 ||
		     !r_has_type(puniqueid, t_integer) ||
		     puniqueid->value.intval != pfont->data.base.UniqueID
		   )
			pfont->data.base.UniqueID = -1;
	   }
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfont1_op_defs[] = {
	{"1.buildfont1", zbuildfont1},
	op_def_end(zfont1_init)
};

/* ------ Subroutines ------ */

/* Get a Boolean parameter from a font-related dictionary. */
/* Return 0 if found, 1 if defaulted, <0 if wrong type. */
private int near
font_bool_param(ref *pdict, ref *pname, int defaultval, int *pvalue)
{	ref *pdval;
	if ( dict_find(pdict, pname, &pdval) <= 0 )
	   {	*pvalue = defaultval;
		return 1;
	   }
	if ( !r_has_type(pdval, t_boolean) ) return e_typecheck;
	*pvalue = pdval->value.index;
	return 0;
}		

/* Get a float parameter from a font-related dictionary. */
/* Return 0 if found, 1 if defaulted, <0 if wrong type. */
private int near
font_float_param(ref *pdict, ref *pname, floatp defaultval, float *pvalue)
{	ref *pdval;
	if ( dict_find(pdict, pname, &pdval) <= 0 )
	   {	*pvalue = defaultval;
		return 1;
	   }
	switch ( r_type(pdval) )
	   {
	case t_integer: *pvalue = pdval->value.intval; return 0;
	case t_real: *pvalue = pdval->value.realval; return 0;
	   }
	return e_typecheck;
}		

/* Get an integer array from a Type 1 font Private dictionary. */
/* Return the element count if OK, 0 if missing, <0 if invalid. */
private int near
font_int_array(ref *pdict, ref *pname, uint maxlen, int *ivec)
{	ref *pdval, *pa;
	int *pi = ivec;
	uint size;
	int i;
	if ( dict_find(pdict, pname, &pdval) <= 0 ) return 0;
	if ( !r_has_type(pdval, t_array) ) return e_invalidfont;
	size = r_size(pdval);
	if ( size > maxlen ) return e_limitcheck;
	pa = pdval->value.refs;
	for ( i = 0; i < size; i++, pa++, pi++ )
	   {	if ( !r_has_type(pa, t_integer) ) return e_typecheck;
		if ( pa->value.intval != (int)pa->value.intval )
			return e_rangecheck;
		*pi = (int)pa->value.intval;
	   }
	return size;
}		

/* Get a float array from a Type 1 font Private dictionary. */
/* Return the element count if OK, 0 if missing, <0 if invalid. */
private int near
font_float_array(ref *pdict, ref *pname, uint maxlen, float *fvec)
{	ref *pdval;
	uint size;
	int code;
	if ( dict_find(pdict, pname, &pdval) <= 0 ) return 0;
	if ( !r_has_type(pdval, t_array) ) return e_invalidfont;
	size = r_size(pdval);
	if ( size > maxlen ) return e_limitcheck;
	code = num_params(pdval->value.refs + size - 1, size, fvec);
	return (code >= 0 ? size : code);
}		
