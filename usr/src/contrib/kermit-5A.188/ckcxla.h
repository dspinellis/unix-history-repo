/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/
#ifndef CKCXLA_H			/* Guard against multiple inclusion */
#define CKCXLA_H

#ifdef NOCSETS
#ifdef KANJI
#undef KANJI
#endif /* KANJI */
#ifdef CYRILLIC
#undef CYRILLIC
#endif /* CYRILLIC */
#ifdef LATIN2
#undef LATIN2
#endif /* LATIN2 */

#else /* Rest of this file... */

#ifndef NOLATIN2			/* If they didn't say "no Latin-2" */
#ifndef LATIN2				/* Then if LATIN2 isn't already */
#define LATIN2				/* defined, define it. */
#endif /* LATIN2 */
#endif /* NOLATIN2 */

#ifndef NOCYRIL				/* If they didn't say "no Cyrillic" */
#ifndef CYRILLIC			/* Then if CYRILLIC isn't already */
#define CYRILLIC			/* defined, define it. */
#endif /* CYRILLIC */
#endif /* NOCYRIL */

/* File ckcxla.h -- Character-set-related definitions, system independent */

/* Codes for Kermit Transfer Syntax Level (obsolete) */

#define TS_L0 0		 /* Level 0 (Transparent) */
#define TS_L1 1		 /* Level 1 (one standard character set) */
#define TS_L2 2		 /* Level 2 (multiple character sets in same file) */

#define UNK 63		 /* Symbol to use for unknown character (63 = ?) */

/*
  Codes for the base alphabet of a given character set.
  These are assigned in roughly ISO 8859 order.
  (Each is assumed to include ASCII/Roman.)
*/
#define AL_UNIV    0			/* Universal (like ISO 10646) */
#define AL_ROMAN   1			/* Roman (Latin) alphabet */
#define AL_CYRIL   2			/* Cyrillic alphabet */
#define AL_ARABIC  3			/* Arabic */
#define AL_GREEK   4			/* Greek */
#define AL_HEBREW  5			/* Hebrew */
#define AL_KANA    6			/* Japanese Katakana */
#define AL_JAPAN   7			/* Japanese Katakana+Kanji ideograms */
#define AL_HAN     8			/* Chinese/Japanese/Korean ideograms */
#define AL_INDIA   9			/* Indian scripts (ISCII) */
					/* Add more here... */
#define AL_UNK   999			/* Unknown (transparent) */

/* Codes for languages */
/*
  NOTE: It would perhaps be better to use ISO 639-1988 2-letter "Codes for 
  Representation of Names of Languages" here, shown in the comments below.
*/
#define L_ASCII       0  /* EN ASCII, English */
#define L_USASCII     0  /* EN ASCII, English */
#define L_DUTCH       1  /* NL Dutch */
#define L_FINNISH     2  /* FI Finnish */
#define L_FRENCH      3  /* FR French */
#define L_GERMAN      4  /* DE German */
#define L_HUNGARIAN   5	 /* HU Hungarian */
#define L_ITALIAN     6  /* IT Italian */
#define L_NORWEGIAN   7  /* NO Norwegian */
#define L_PORTUGUESE  8  /* PT Portuguese */
#define L_SPANISH     9  /* ES Spanish */
#define L_SWEDISH    10  /* SV Swedish */
#define L_SWISS      11  /* RM Swiss (Rhaeto-Romance) */
#define L_DANISH     12  /* DA Danish */
#define L_ICELANDIC  13  /* IS Icelandic */

#ifdef CYRILLIC		 /* RU Russian */
#define L_RUSSIAN 14
#ifndef KANJI
#define MAXLANG 14
#endif /* KANJI */
#endif /* CYRILLIC */

#ifdef KANJI		 /* JA Japanese */
#ifndef CYRILLIC
#define L_JAPANESE 14
#define MAXLANG 14
#else
#define L_JAPANESE 15
#define MAXLANG 15
#endif /* CYRILLIC */
#endif /* KANJI */

#ifndef MAXLANG
#define MAXLANG 14
#endif /* MAXLANG */

#ifdef COMMENT
/*
  The ones below are not used yet.  This list needs to be expanded and
  organized, and something must be done about the hard coded numbers, because
  they will be wrong if CYRILLIC and/or KANJI are deselected.  But we can't do
  "#define L_CHINESE L_JAPANESE + 1" because that causes recursion in the
  preprocessor.
*/
#define L_CHINESE    16			/* ZH */
#define L_KOREAN     17			/* KO */
#define L_ARABIC     18			/* AR */
#define L_HEBREW     19			/* IW */
#define L_GREEK      20			/* EL */
#define L_TURKISH    21			/* TR */
/* etc... */
#endif /* COMMENT */

/*
  File character-sets are defined in the system-specific ck?xla.h file,
  except for the following one, which must be available to all versions:
*/
#define FC_TRANSP  254			/* Transparent */

/*
  Designators for Kermit's transfer character sets.  These are all standard
  sets, or based on them.  Symbols must be unique in the first 8 characters,
  because some C preprocessors have this limit.
*/
/* LIST1 */
#define TC_TRANSP  0   /* Transparent, no character translation */
#define TC_USASCII 1   /* US 7-bit ASCII */
#define TC_1LATIN  2   /* ISO 8859-1, Latin-1 */
#define TC_2LATIN  3   /* ISO 8859-2, Latin-2 */
#define TC_CYRILL  4   /* ISO 8859-5, Latin/Cyrillic */
#define TC_JEUC    5   /* Japanese EUC */

#define MAXTCSETS  5   /* Highest Transfer Character Set Number */

#ifdef COMMENT
/*
  Not used yet.
*/
#define TC_3LATIN  6  /* ISO 8859-3, Latin-3 */
#define TC_4LATIN  7  /* ISO 8859-4, Latin-4 */
#define TC_5LATIN  8  /* ISO 8859-9, Latin-5 */
#define TC_ARABIC  9  /* ISO-8859-6, Latin/Arabic */
#define TC_GREEK  10  /* ISO-8859-7, Latin/Greek */
#define TC_HEBREW 11  /* ISO-8859-8, Latin/Hebrew */
#define TC_JIS208 12  /* Japanese JIS X 0208 multibyte set */
#define TC_CHINES 13  /* Chinese Standard GB 2312-80 */
#define TC_KOREAN 14  /* Korean KS C 5601-1987 */
#define TC_I10646 15  /* ISO DIS 10646 (not defined yet) */
/* etc... */
#endif /* COMMENT */

/* Structure for character-set information */

struct csinfo {
    char *name;				/* Name of character set */
    int size;				/* Size (128 or 256)     */
    int code;				/* Like TC_1LATIN, etc.  */
    char *designator;			/* Designator, like I2/100 = Latin-1 */
    int alphabet;			/* Base alphabet */
};

/* Structure for language information */

struct langinfo {
    int id;				/* Language ID code (L_whatever) */
    int fc;				/* File character set to use */
    int tc;				/* Transfer character set to use */
    char *description;			/* Description of language */
};

/* Now take in the system-specific definitions */

#ifdef UNIX
#include "ckuxla.h"
#endif /* UNIX */

#ifdef OSK				/* OS-9 */
#include "ckuxla.h"
#endif /* OS-9 */

#ifdef VMS				/* VAX/VMS */
#include "ckuxla.h"
#endif /* VMS */

#ifdef GEMDOS				/* Atari ST */
#include "ckuxla.h"
#endif /* GEMDOS */

#ifdef MAC				/* Macintosh */
#include "ckmxla.h"
#endif /* MAC */

#ifdef OS2				/* OS/2 */
#include "ckuxla.h"			/* Uses big UNIX version */
#endif /* OS2 */

#ifdef AMIGA				/* Commodore Amiga */
#include "ckuxla.h"
#endif /* AMIGA */

#ifdef datageneral			/* Data General MV AOS/VS */
#include "ckuxla.h"
#endif /* datageneral */

#endif /* NOCSETS */

#endif /* CKCXLA_H */

#ifdef KANJI
_PROTOTYP( int xkanjf, (void) );
_PROTOTYP( int xkanjz, (int (*)(char)) );
_PROTOTYP( int xkanji, (int, int (*)(char)) );
#endif /* KANJI */

/* End of ckcxla.h */
