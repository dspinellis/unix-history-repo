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

/* ccfont.h */
/* Header for fonts compiled into C. */

/* Define type-specific refs for initializing arrays. */
#define ref_(t) struct { struct tas_s tas; t value; }
typedef struct { short len; char _ds *str; } charray;
typedef struct { byte encx, charx; } charindex;
#define array_v(n,p,ea)\
 { {(t_array<<r_type_shift)+(ea), n}, (ref *)(p) }
#define boolean_v(b) { {t_boolean<<r_type_shift}, (ushort)(b) }
#define name_v(n,s)\
 { {t_name<<r_type_shift, n}, s }
#define null_v() { {t_null<<r_type_shift} }
#define real_v(v) { {t_real<<r_type_shift}, (float)(v) }
#define integer_v(i) { {t_integer<<r_type_shift}, (long)(i) }
#define string_v(n,s)\
 { {(t_string<<r_type_shift)+a_read+a_execute, n}, s }

/* Define the combined access masks */
#define a_readonly (a_read + a_execute)

/* Support routines in iccfont.c */
typedef struct {
	const charindex _ds *enc_keys;	/* keys from encoding vectors */
	uint num_enc_keys;
	const char _ds * _ds *str_keys;	/* string keys */
	uint num_str_keys;
	uint extra_slots;		/* (need 1 extra for fonts) */
	uint dict_attrs;		/* protection for dictionary */
	uint value_attrs;		/* protection for values */
					/* (only used for string dicts) */
} cfont_dict_keys;
extern int cfont_ref_dict_create(P3(ref *, const cfont_dict_keys _ds *, const ref _ds * _ds *));
extern int cfont_string_dict_create(P3(ref *, const cfont_dict_keys _ds *, const charray _ds *));
extern int cfont_num_dict_create(P3(ref *, const cfont_dict_keys _ds *, const float _ds *));
extern int cfont_name_array_create(P3(ref *, const char _ds * _ds *, int));
