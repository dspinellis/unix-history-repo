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

/* zfont0.c */
/* Composite font creation operator for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
/*
 * The following lines used to say:
 *	#include "gsmatrix.h"
 *	#include "gxdevice.h"		/. for gxfont.h ./
 * Tony Li says the longer list is necessary to keep the GNU compiler
 * happy, but this is pretty hard to understand....
 */
#include	"gxfixed.h"
#include	"gxmatrix.h"
#include	"gzstate.h"		/* must precede gxdevice */
#include	"gxdevice.h"		/* must precede gxfont */
#include	"gschar.h"
#include "gxfont.h"
#include "alloc.h"
#include "font.h"
#include "dict.h"
#include "name.h"
#include "state.h"
#include "store.h"

/* Imports */
extern int build_gs_font(P4(os_ptr, gs_font **, font_type, ref *));
extern ref name_FID;

/* Forward references */
private int ensure_char_entry(P4(os_ptr, ref *, byte *, int));

/* Names of system-known keys in type 0 font dictionaries: */
private ref name_EscChar;
private ref name_FDepVector;
private ref name_FMapType;
private ref name_PrefEnc;
private ref name_ShiftIn;
private ref name_ShiftOut;
private ref name_SubsVector;
private ref name_Type0BuildChar;

/* Initialize the composite font operators */
private void
zfont0_init()
{	static names_def fnd0[] = {
	   { "EscChar", &name_EscChar },
	   { "FDepVector", &name_FDepVector },
	   { "FMapType", &name_FMapType },
	   { "PrefEnc", &name_PrefEnc },
	   { "ShiftIn", &name_ShiftIn },
	   { "ShiftOut", &name_ShiftOut },
	   { "SubsVector", &name_SubsVector },
	   { "Type0BuildChar", &name_Type0BuildChar },
	   names_def_end
	};
	init_names(fnd0);

	/* Make the standard BuildChar procedure executable. */
	r_set_attrs(&name_Type0BuildChar, a_executable);
}

/* .buildfont0 */
/* Build a type 0 (composite) font. */
int
zbuildfont0(os_ptr op)
{	ref *pfmaptype;
	gs_type0_data data;
	ref *pfdepvector;
	ref *pprefenc;
	ref *psubsvector;
	gs_font *pfont;
	font_data *pdata;
	int i;
	int code;
	check_type(*op, t_dictionary);
	if ( dict_find(op, &name_FMapType, &pfmaptype) <= 0 ||
	     !r_has_type(pfmaptype, t_integer) ||
	     pfmaptype->value.intval < (int)fmap_type_min ||
	     pfmaptype->value.intval > (int)fmap_type_max ||
	     dict_find(op, &name_FDepVector, &pfdepvector) <= 0 ||
	     !r_has_type(pfdepvector, t_array)
	   )
		return e_invalidfont;
	data.FMapType = (fmap_type)pfmaptype->value.intval;
	/* Check that every element of the FDepVector is a font. */
	data.fdep_size = r_size(pfdepvector);
	for ( i = 0; i < data.fdep_size; i++ )
	   {	ref *pdep = pfdepvector->value.refs + i;
		ref *pfid;
		gs_font *psub;
		if ( !r_has_type(pdep, t_dictionary) ||
		     dict_find(pdep, &name_FID, &pfid) <= 0 ||
		     !r_has_type(pfid, t_fontID)
		   )
			return e_invalidfont;
		/*
		 * Check the inheritance rules.  Allowed configurations
		 * (paths from root font) are defined by the regular
		 * expression:
		 *	(shift | double_escape escape* | escape*)
		 *	  non_modal* non_composite
		 */
		psub = pfid->value.pfont;
		if ( psub->FontType == ft_composite )
		   {	fmap_type fmt = psub->data.type0_data.FMapType;
			if ( fmt == fmap_double_escape ||
			     fmt == fmap_shift ||
			     fmt == fmap_escape &&
			      !(data.FMapType == fmap_escape ||
				data.FMapType == fmap_double_escape)
			   )
				return e_invalidfont;
		   }
	   }
	switch ( data.FMapType )
	   {
	case fmap_escape: case fmap_double_escape:	/* need EscChar */
		code = ensure_char_entry(op, &name_EscChar, &data.EscChar, 255);
		break;
	case fmap_shift:			/* need ShiftIn & ShiftOut */
		code = ensure_char_entry(op, &name_ShiftIn, &data.ShiftIn, 15);
		if ( code == 0 )
		  code = ensure_char_entry(op, &name_ShiftOut, &data.ShiftOut, 14);
		break;
	case fmap_SubsVector:			/* need SubsVector */
		if ( dict_find(op, &name_SubsVector, &psubsvector) <= 0 ||
		     !r_has_type(psubsvector, t_string) ||
		     r_size(psubsvector) == 0 ||
		     (data.subs_width = (int)*psubsvector->value.bytes + 1) > 4 ||
		     (r_size(psubsvector) - 1) % data.subs_width != 0
		   )
			return e_invalidfont;
	default:
		code = 0;
	   }
	if ( code < 0 ) return code;
	code = build_gs_font(op, &pfont, ft_composite, &name_Type0BuildChar);
	if ( code != 0 ) return code;
	if ( dict_find(op, &name_PrefEnc, &pprefenc) <= 0 )
	   {	ref nul;
		make_null_new(&nul);
		if ( (code = dict_put(op, &name_PrefEnc, &nul)) < 0 )
			return code;
	   }
	/* Fill in the font data */
	pdata = (font_data *)(pfont->client_data);
	data.subs_size = (r_size(psubsvector) - 1) / data.subs_width;
	data.SubsVector = psubsvector->value.bytes + 1;
	data.encoding_size = r_size(&pdata->Encoding);
	data.Encoding =
	  (uint *)alloc(data.encoding_size, sizeof(uint),
		       "buildfont0(Encoding)");
	/* Fill in the encoding vector, checking to make sure that */
	/* each element is an integer between 0 and fdep_size-1. */
	for ( i = 0; i < data.encoding_size; i++ )
	   {	ref *penc = pdata->Encoding.value.refs + i;
		if ( !r_has_type(penc, t_integer) ||
		     (ulong)penc->value.intval >= data.fdep_size
		   )
			return e_invalidfont;
		data.Encoding[i] = (uint)penc->value.intval;
	   }
	data.FDepVector =
	  (gs_font **)alloc(data.fdep_size, sizeof(gs_font *),
			   "buildfont0(FDepVector)");
	for ( i = 0; i < data.fdep_size; i++ )
	   {	ref *pfid;
		/* The lookup can't fail, because of the pre-check above. */
		dict_find(pfdepvector->value.refs + i, &name_FID, &pfid);
		data.FDepVector[i] = pfid->value.pfont;
	   }
	pfont->data.type0_data = data;
	return 0;
}
/* Private routine to find or add an integer entry in a font dictionary. */
private int
ensure_char_entry(os_ptr op, ref *pname, byte *pvalue, int default_value)
{	ref *pentry;
	if ( dict_find(op, pname, &pentry) <= 0 )
	   {	ref ent;
		make_int(&ent, default_value);
		return dict_put(op, pname, &ent);
	   }
	else
	  if ( !r_has_type(pentry, t_integer) ||
	       (ulong)(pentry->value.intval) > 255
	     )
		return e_invalidfont;
	*pvalue = (byte)pentry->value.intval;
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfont0_op_defs[] = {
	{"1.buildfont0", zbuildfont0},
	op_def_end(zfont0_init)
};
