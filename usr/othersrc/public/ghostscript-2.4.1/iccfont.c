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

/* iccfont.c */
/* Initialization support for compiled fonts */
#include "string_.h"
#include "ghost.h"
#include "alloc.h"
#include "ccfont.h"
#include "dict.h"
#include "dstack.h"
#include "errors.h"
#include "name.h"
#include "save.h"			/* for alloc_refs */
#include "store.h"

extern int array_get(P3(const ref *, long, ref *));
extern ref name_StandardEncoding;

/* Check for reaching the end of the keys. */
#define more_keys(kp) ((kp)->num_enc_keys | (kp)->num_str_keys)

/* Put the next entry into a dictionary. */
/* We know that more_keys(kp) is true. */
private int
cfont_put_next(ref *pdict, cfont_dict_keys _ss *kp, const ref *pvalue,
  ref * _ss *pencodings)
{	ref kname;
	int code;
	if ( *pencodings == 0 )
	   {	/* Create the dictionary and look up the encodings. */
		code = dict_create(kp->num_enc_keys + kp->num_str_keys + kp->extra_slots, pdict);
		if ( code < 0 ) return code;
		make_tasv(&kname, t_string, 0, 17, bytes,
			  (byte *)"ISOLatin1Encoding");
		if ( dict_find(&systemdict, &name_StandardEncoding, &pencodings[0]) <= 0 ||
		     dict_find(&systemdict, &kname, &pencodings[1]) <= 0
		   )
			return e_undefined;
	   }
	if ( kp->num_enc_keys )
	   {	charindex _ds *skp = kp->enc_keys++;
		code = array_get(pencodings[skp->encx], (long)(skp->charx), &kname);
		kp->num_enc_keys--;
	   }
	else		/* must have kp->num_str_keys != 0 */
	   {	const char *skp = *(kp->str_keys++);
		code = name_ref((const byte *)skp, strlen(skp), &kname, 0);
		kp->num_str_keys--;
	   }
	return dict_put(pdict, &kname, pvalue);
}

/* Create a dictionary with general ref values. */
int
cfont_ref_dict_create(ref *pdict, const cfont_dict_keys _ds *kp,
  const ref _ds * _ds *values)
{	cfont_dict_keys keys;
	const ref _ds * _ds *vp = values;
	ref *pencodings[2];
	keys = *kp;
	pencodings[0] = 0;
	while ( more_keys(&keys) )
	   {	const ref *pvalue = *vp++;
		ref nref;
		int code;
		if ( r_has_type(pvalue, t_name) )
		   {	/* The "name" is really a string. */
			/* Convert it to a real name now. */
			code = name_ref(pvalue->value.bytes, r_size(pvalue),
					&nref, 0);
			if ( code < 0 ) return code;
			pvalue = &nref;
		   }
		code = cfont_put_next(pdict, &keys, pvalue, pencodings);
		if ( code < 0 ) return code;
	   }
	return 0;
}

/* Create a dictionary with string values. */
int
cfont_string_dict_create(ref *pdict, const cfont_dict_keys _ds *kp,
  const charray _ds *values)
{	cfont_dict_keys keys;
	const charray _ds *vp = values;
	uint attrs = kp->value_attrs;
	ref *pencodings[2];
	ref vstring;
	keys = *kp;
	pencodings[0] = 0;
	while ( more_keys(&keys) )
	   {	int code;
		make_tasv(&vstring, t_string, attrs,
			  vp->len, bytes, (byte *)vp->str);
		vp++;
		code = cfont_put_next(pdict, &keys, &vstring, pencodings);
		if ( code < 0 ) return code;
	   }
	return 0;
}

/* Create a dictionary with number values. */
int
cfont_num_dict_create(ref *pdict, const cfont_dict_keys _ds *kp,
  const float _ds *values)
{	cfont_dict_keys keys;
	const float _ds *vp = values;
	ref *pencodings[2];
	ref vnum;
	keys = *kp;
	pencodings[0] = 0;
	while ( more_keys(&keys) )
	   {	float val = *vp++;
		int code;
		if ( val == (int)val )
			make_int(&vnum, (int)val);
		else
			make_real(&vnum, val);
		code = cfont_put_next(pdict, &keys, &vnum, pencodings);
		if ( code < 0 ) return code;
	   }
	return 0;
}

/* Create an array with name values. */
int
cfont_name_array_create(ref *parray, const char _ds * _ds *str_list,
  int size)
{	ref *aptr = alloc_refs(size, "cfont_name_array_create");
	int i;
	const char _ds * _ds *pstr = str_list;
	if ( aptr == 0 ) return e_VMerror;
	make_tasv(parray, t_array, a_read + a_execute, size, refs, aptr);
	for ( i = 0; i < size; i++, aptr++, pstr++ )
	   {	ref nref;
		int code = name_ref((byte *)*pstr, strlen(*pstr), &nref, 0);
		if ( code < 0 ) return code;
		ref_assign_new(aptr, &nref);
	   }
	return 0;
}
