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

/* gxfont.h */
/* Internal font definition for Ghostscript library */
/* Requires gsmatrix.h, gxdevice.h */
#include "gsfont.h"

/* A font object as seen by clients. */
/* See the PostScript Language Reference Manual for details. */

#ifndef gs_show_enum_s_DEFINED
struct gs_show_enum_s;
#endif

typedef int (*gs_proc_build_char)
     (P5(struct gs_show_enum_s *, struct gs_state_s *, struct gs_font_s *,
	 char_code, char * /* build_char_data */));

int gs_no_build_char_proc
     (P5(struct gs_show_enum_s *, struct gs_state_s *, struct gs_font_s *,
	 char_code, char *));

/* Define the known font types. */
/* These numbers must be the same as the values of FontType */
/* in font dictionaries. */
typedef enum {
	ft_composite = 0,
	ft_encrypted = 1,
	ft_user_defined = 3
} font_type;

/* Define the composite font mapping types. */
/* These numbers must be the same as the values of FMapType */
/* in type 0 font dictionaries. */
typedef enum {
	fmap_8_8 = 2,
	fmap_escape = 3,
	fmap_1_7 = 4,
	fmap_9_7 = 5,
	fmap_SubsVector = 6,
	fmap_double_escape = 7,
	fmap_shift = 8
} fmap_type;
#define fmap_type_min 2
#define fmap_type_max 8
#define fmap_type_is_modal(fmt)\
  ((fmt) == fmap_escape || (fmt) == fmap_double_escape || (fmt) == fmap_shift)

/* This is the type-specific information for a type 0 (composite) gs_font. */
typedef struct gs_type0_data_s gs_type0_data;
struct gs_type0_data_s {
	fmap_type FMapType;
	byte EscChar, ShiftIn, ShiftOut;
	byte *SubsVector;
	  uint subs_size;		/* bytes per entry */
	  uint subs_width;		/* # of entries */
	uint *Encoding;
	  uint encoding_size;
	gs_font **FDepVector;
	  uint fdep_size;
};

/* This is the type-specific information for a type 1 (encrypted) gs_font. */
#define zone_table(tname, size)\
	struct {\
		int count;\
		int data[(size)*2];\
	} tname
#define stem_table(tname, size)\
	struct {\
		int count;\
		float data[size];\
	} tname
typedef struct gs_type1_data_s gs_type1_data;
struct gs_type1_data_s {
	int PaintType;			/* PaintType */
	int (*subr_proc)(P3(gs_type1_data *pdata,
			    int index, byte **pcharstring));
	int (*pop_proc)(P2(gs_type1_data *, fixed *));
	char *proc_data;		/* data for subr_proc */
	int lenIV;			/* # of leading garbage bytes */
	/* The following hint information is not used yet. */
	/* See chapter 5 of the "Adobe Type 1 Font Format" book. */
	int BlueFuzz;
	float BlueScale;
	int BlueShift;
#define max_BlueValues 7
	zone_table(BlueValues, max_BlueValues);
	float ExpansionFactor;
	int ForceBold;
#define max_FamilyBlues 7
	zone_table(FamilyBlues, max_FamilyBlues);
#define max_FamilyOtherBlues 5
	zone_table(FamilyOtherBlues, max_FamilyOtherBlues);
	int LanguageGroup;
#define max_OtherBlues 5
	zone_table(OtherBlues, max_OtherBlues);
	int RndStemUp;
	stem_table(StdHW, 1);
	stem_table(StdVW, 1);
#define max_StemSnap 12
	stem_table(StemSnapH, max_StemSnap);
	stem_table(StemSnapV, max_StemSnap);
};
#define gs_type1_data_s_DEFINED

/* Even though it costs a little extra space, it's more convenient to */
/* include all the necessary information for >>all<< known font types */
/* (user-defined, encrypted, and composite) in the font structure. */
struct gs_font_s {
	gs_font *next, *prev;		/* chain for scaled font cache */
	gs_font *base;			/* original (unscaled) base font */
	gs_font_dir *dir;		/* directory where registered */
	char *client_data;		/* additional client data */
	gs_matrix FontMatrix;
	font_type FontType;
	int WMode;			/* 0 or 1 */
	gs_proc_build_char build_char_proc;	/* BuildChar */
	char *build_char_data;		/* private data for BuildChar */
	union _d {
		/* Composite (type 0) fonts */
		gs_type0_data type0_data;
		/* Base (non-type 0) fonts */
		struct _b {
			gs_rect FontBBox;
			long UniqueID;
			/* Type 1 data */
			gs_type1_data type1_data;
		} base;
	} data;
};
