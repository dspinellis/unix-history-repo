#ifndef NOCSETS
char *xlav = "Character Set Translation 5A(018), 21 Nov 92";

/*  C K U X L A  */

/*  C-Kermit tables and functions supporting character set translation.  */
/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/

/*
  CAVEAT PROGRAMMOR: The mechanism used here turns out to be somewhat
  inflexible and maybe a little dangerous.  It is designed for Kermit's
  character-at-a-time processing during protocol operations.  Elaborate
  kludges are used for translating one character into two (like stuffing an
  extra character into the input stream), or two into one, or two into two.

  The whole translation business needs to be redesigned to be string-oriented
  rather than character oriented, so (a) we can have more flexible
  translations, and (b) we don't have to be concerned about which input stream
  we are using.  The current mechanism is also quite inappropriate
  for multibyte character sets and for flexible user-defined translations.

  For the future: perhaps it would be better to represent characters
  internally using a universal character set like UNICODE (ISO 10646 BMP),
  the ultimate "transfer character set".
*/
#include "ckcdeb.h"			/* Includes... */
#include "ckcker.h"
#include "ckucmd.h"
#include "ckcxla.h"

/* Character set translation data and functions */

extern int zincnt;			/* File i/o macros and variables */
extern char *zinptr;
extern int zoutcnt;
extern char *zoutptr;

int tslevel  = TS_L0;			/* Transfer syntax level (0,1,2) */
int tcharset = TC_TRANSP;		/* Transfer syntax character set */
int tcsr     = FC_USASCII;		/* Remote terminal character set */
int language = L_USASCII;		/* Language */

/*
  Default local file and terminal character set.
  Normally ASCII, but for some systems we know otherwise.
*/
#ifdef datageneral			/* Data General AOS/VS */
int fcharset = FC_DGMCS;		/* uses the DG International set */
int tcsl     = FC_DGMCS;
#else
#ifdef NEXT				/* The NeXT workstation */
int fcharset = FC_NEXT;			/* uses its own 8-bit set */
int tcsl     = FC_NEXT;
#else
#ifdef MAC				/* The Macintosh */
int fcharset = FC_APPQD;		/* uses an extended version of */
int tcsl     = FC_APPQD;		/* Apple Quickdraw */
#else
#ifdef AUX
int fcharset = FC_APPQD;		/* Ditto for Apple A/UX */
int tcsl     = FC_APPQD;
#else
#ifdef AMIGA				/* The Commodore Amiga */
int fcharset = FC_1LATIN;		/* uses Latin-1 */
int tcsl     = FC_1LATIN;
#else					/* All others */
int fcharset = FC_USASCII;		/* use ASCII by default */
int tcsl     = FC_USASCII;
#endif /* AMIGA */
#endif /* AUX */
#endif /* MAC */
#endif /* NEXT */
#endif /* datageneral */



_PROTOTYP( CHAR xnel1, (CHAR c) );	/* NeXT to Latin-1 */
_PROTOTYP( CHAR xl143, (CHAR c) );	/* Latin-1 to IBM CP437 */
_PROTOTYP( CHAR xl1as, (CHAR c) );	/* Latin-1 to US ASCII */
_PROTOTYP( CHAR zl1as, (CHAR c) );	/* Latin-1 to US ASCII */

#ifdef CYRILLIC
_PROTOTYP( CHAR xassk, (CHAR c) );	/* ASCII to Short KOI */
_PROTOTYP( CHAR xskcy, (CHAR c) );	/* Short KOI to Latin/Cyrillic */
#endif /* CYRILLIC */

#ifdef LATIN2
_PROTOTYP( CHAR xnel2, (CHAR c) );	/* NeXT to Latin-2 */
_PROTOTYP( CHAR xl243, (CHAR c) );	/* Latin-2 to IBM CP437 */
_PROTOTYP( CHAR xl2as, (CHAR c) );	/* Latin-2 to US ASCII */
_PROTOTYP( CHAR zl2as, (CHAR c) );	/* Latin-2 to US ASCII */
#endif /* LATIN2 */

/* Transfer character-set info */

struct csinfo tcsinfo[] = {
/*  Name                       size code      designator  alphabet           */
    "TRANSPARENT, no translation", 256, TC_TRANSP,  "",       AL_UNK,   /* 0 */
    "ASCII",                       128, TC_USASCII, "",       AL_ROMAN, /* 1 */
    "LATIN1, ISO 8859-1",          256, TC_1LATIN,  "I6/100", AL_ROMAN, /* 2 */
#ifdef LATIN2
    "LATIN2, ISO 8859-2",          256, TC_2LATIN,  "I6/101", AL_ROMAN, /* 3 */
#else
    "LATIN2 not supported",          0, TC_TRANSP,   "XXX",   AL_UNK,   /* 3 */
#endif /* LATIN2 */
#ifdef CYRILLIC
    "CYRILLIC, ISO 8859-5",        256, TC_CYRILL,   "I6/144",AL_CYRIL, /* 4 */
#else
    "CYRILLIC not supported",        0, TC_TRANSP,   "XXX",   AL_UNK,   /* 4 */
#endif /* CYRILLIC */
/*
  NOTE: the second entry is obsolete, but retained temporarily.
  The proper designator is given in the first entry.  C-Kermit sends the
  first (proper) form, but accepts either form.
*/
#ifdef KANJI
    "KANJI (EUC)", 16384, TC_JEUC,  "I14/87/13", AL_JAPAN, /* 5 */
    "KANJI (EUC)", 16384, TC_JEUC,  "I14/87E",   AL_JAPAN  /* 5 */
#else
    "KANJI not supported", 0, TC_TRANSP,"XXX",   AL_UNK,   /* 5 */
#endif /* KANJI */
};
int ntcsets = (sizeof(tcsinfo) / sizeof(struct csinfo));

struct keytab tcstab[] = {		/* Keyword table for */
    "ascii",         TC_USASCII, 0,	/* SET TRANSFER CHARACTER-SET */
#ifdef CYRILLIC
    "cyrillic-iso",  TC_CYRILL,  0,
#endif /* CYRILLIC */
#ifdef KANJI
    "japanese-euc",  TC_JEUC,    0,
#endif /* KANJI */
#ifdef LATIN2
/*
  If Latin-2 is defined, let the following be invisible, non-unique
  abbreviations for LATIN1.
*/
    "l",             TC_1LATIN,  CM_ABR|CM_INV,
    "la",            TC_1LATIN,  CM_ABR|CM_INV,
    "lat",           TC_1LATIN,  CM_ABR|CM_INV,
    "lati",          TC_1LATIN,  CM_ABR|CM_INV,
    "latin",         TC_1LATIN,  CM_ABR|CM_INV,
#endif /* LATIN2 */
    "latin1-iso",    TC_1LATIN,  0,
#ifdef LATIN2
    "latin2-iso",    TC_2LATIN,  0,
#endif /* LATIN2 */
    "transparent",   TC_TRANSP,  0
};
int ntcs = (sizeof(tcstab) / sizeof(struct keytab));

/* File character set information structure, indexed by character set code, */
/* as defined in ckuxla.h.  This table must be in order of file character */
/* set number! */ 

struct csinfo fcsinfo[] = { /* File character set information... */
  /* Descriptive Name              Size  Designator */
    "US ASCII",                     128, FC_USASCII, NULL, AL_ROMAN,
    "British/UK ISO-646",           128, FC_UKASCII, NULL, AL_ROMAN,
    "Dutch ISO-646",                128, FC_DUASCII, NULL, AL_ROMAN,
    "Finnish ISO-646",              128, FC_FIASCII, NULL, AL_ROMAN,
    "French ISO-646",               128, FC_FRASCII, NULL, AL_ROMAN,
    "Canadian-French ISO-646",      128, FC_FCASCII, NULL, AL_ROMAN,
    "German ISO-646",               128, FC_GEASCII, NULL, AL_ROMAN,
    "Hungarian ISO-646",            128, FC_HUASCII, NULL, AL_ROMAN,
    "Italian ISO-646",              128, FC_ITASCII, NULL, AL_ROMAN,
    "Norwegian/Danish ISO-646",     128, FC_NOASCII, NULL, AL_ROMAN,
    "Portuguese ISO-646",           128, FC_POASCII, NULL, AL_ROMAN,
    "Spanish ISO-646",              128, FC_SPASCII, NULL, AL_ROMAN,
    "Swedish ISO-646",              128, FC_SWASCII, NULL, AL_ROMAN,
    "Swiss ISO-646",                128, FC_CHASCII, NULL, AL_ROMAN,
    "ISO 8859-1 Latin-1",           256, FC_1LATIN,  NULL, AL_ROMAN,
#ifdef LATIN2
    "ISO 8859-2 Latin-2",           256, FC_2LATIN,  NULL, AL_ROMAN,
#endif /* LATIN2 */
    "DEC Multinational",            256, FC_DECMCS,  NULL, AL_ROMAN,
    "NeXT Multinational",           256, FC_NEXT,    NULL, AL_ROMAN,
    "IBM Code Page 437",            256, FC_CP437,   NULL, AL_ROMAN,
    "IBM Code Page 850",            256, FC_CP850,   NULL, AL_ROMAN,
#ifdef LATIN2
    "IBM Code Page 852",            256, FC_CP852,   NULL, AL_ROMAN,
#endif /* LATIN2 */
    "Apple Macintosh Latin",        256, FC_APPQD,   NULL, AL_ROMAN,
    "Data General International",   256, FC_DGMCS,   NULL, AL_ROMAN
#ifdef CYRILLIC
,   "ISO 8859-5 Latin/Cyrillic",    256, FC_CYRILL,  NULL, AL_CYRIL,
    "CP866 Cyrillic",		    256, FC_CP866,   NULL, AL_CYRIL,
    "Short KOI",                    128, FC_KOI7,    NULL, AL_CYRIL,
    "Old KOI-8 Cyrillic",           256, FC_KOI8,    NULL, AL_CYRIL
#endif /* CYRILLIC */
#ifdef KANJI
,   "Japanese JIS7",		    256, FC_JIS7,    NULL, AL_JAPAN,
    "Japanese Shift JIS",	  16384, FC_SHJIS,   NULL, AL_JAPAN,
    "Japanese EUC",		  16384, FC_JEUC,    NULL, AL_JAPAN,
    "Japanese DEC Kanji",	  16384, FC_JDEC,    NULL, AL_JAPAN
#endif /* KANJI */
};    

/* Local file character sets */
/* Includes 7-bit National Replacement Character Sets of ISO 646 */
/* Plus ISO Latin-1, DEC Multinational Character Set (MCS), NeXT char set */

struct keytab fcstab[] = { /* Keyword table for 'set file character-set' */

/*
  IMPORTANT: This table is replicated below as ttcstab (terminal character 
  set table).  The only differences are the addition of TRANSPARENT
  and the removal of the Kanji sets, which are not supported yet.
  If you make changes to this table, also change ttcstab.
*/

/* Keyword             Value       Flags */
    "apple-quickdraw",    FC_APPQD,   CM_INV, /* Apple Quickdraw */
    "ascii",              FC_USASCII, 0, /* ASCII */
    "british",            FC_UKASCII, 0, /* British NRC */
    "canadian-french",    FC_FCASCII, 0, /* French Canadian NRC */
    "cp437",              FC_CP437,   0, /* IBM CP437 */
    "cp850",              FC_CP850,   0, /* IBM CP850 */
#ifdef LATIN2
    "cp852",              FC_CP852,   0, /* IBM CP852 */
#endif /* LATIN2 */
#ifdef CYRILLIC
    "cp866-cyrillic",     FC_CP866,   0, /* CP866 Cyrillic */
    "cyrillic-iso",       FC_CYRILL,  0, /* ISO Latin/Cyrillic Alphabet */
#endif /* CYRILLIC */
    "danish",             FC_NOASCII, 0, /* Norwegian and Danish NRC */
#ifdef KANJI
    "dec-kanji",          FC_JDEC,    0, /* Japanese DEC Kanji */
#endif /* KANJI */
    "dec-multinational",  FC_DECMCS,  0, /* DEC multinational character set */
    "dg-international",   FC_DGMCS,   0, /* Data General multinational */
    "dutch",              FC_DUASCII, 0, /* Dutch NRC */
    "finnish",            FC_FIASCII, 0, /* Finnish NRC */
    "french",             FC_FRASCII, 0, /* French NRC */
    "fr-canadian",        FC_FCASCII, CM_INV, /* French Canadian NRC */
    "german",             FC_GEASCII, 0, /* German NRC */
    "hungarian",          FC_HUASCII, 0, /* Hungarian NRC */
    "italian",            FC_ITASCII, 0, /* Italian NRC */
#ifdef KANJI
    "japanese-euc",       FC_JEUC,    0, /* Japanese EUC */
    "jis7-kanji",         FC_JIS7,    0, /* Japanese JIS7 7bit code */
#endif /* KANJI */
#ifdef CYRILLIC
    "koi8-cyrillic",      FC_KOI8,    0, /* Old KOI-8 Cyrillic */
#endif /* CYRILLIC */
#ifdef LATIN2
    "l",                  FC_1LATIN,  CM_ABR|CM_INV,
    "la",                 FC_1LATIN,  CM_ABR|CM_INV,
    "lat",                FC_1LATIN,  CM_ABR|CM_INV,
    "lati",               FC_1LATIN,  CM_ABR|CM_INV,
    "latin",              FC_1LATIN,  CM_ABR|CM_INV,
#endif /* LATIN2 */
    "latin1-iso",         FC_1LATIN,  0, /* ISO Latin Alphabet 1 */
#ifdef LATIN2
    "latin2-iso",         FC_2LATIN,  0, /* ISO Latin Alphabet 2 */
#endif /* LATIN2 */
    "macintosh-latin",    FC_APPQD,   0, /* "Extended Mac Latin" */
    "next-multinational", FC_NEXT,    0, /* NeXT workstation */
    "norwegian",          FC_NOASCII, 0, /* Norwegian and Danish NRC */
    "portuguese",         FC_POASCII, 0, /* Portuguese NRC */
#ifdef KANJI
    "shift-jis-kanji",    FC_SHJIS,   0, /* Japanese Kanji Shift-JIS */
#endif /* KANJI */
#ifdef CYRILLIC
    "short-koi",          FC_KOI7,    0, /* Short KOI Cyrillic */
#endif /* CYRILLIC */
    "spanish",            FC_SPASCII, 0, /* Spanish NRC */
    "swedish",            FC_SWASCII, 0, /* Swedish NRC */
    "swiss",              FC_CHASCII, 0	 /* Swiss NRC */
};
int nfilc = (sizeof(fcstab) / sizeof(struct keytab)); /* size of this table */


struct keytab ttcstab[] = { /* Keyword table for SET TERMINAL CHARACTER-SET */
/*
  IMPORTANT: This table is a replica of fcstab, immediately above, with the
  addition of TRANSPARENT.  If you make changes to this table, make the
  corresponding changes to fcstab.
*/
/* Keyword               Value       Flags */
    "apple-quickdraw",    FC_APPQD,   CM_INV, /* Apple Quickdraw */
    "ascii",              FC_USASCII, 0, /* ASCII */
    "british",            FC_UKASCII, 0, /* British NRC */
    "canadian-french",    FC_FCASCII, 0, /* French Canadian NRC */
    "cp437",              FC_CP437,   0, /* IBM CP437 */
    "cp850",              FC_CP850,   0, /* IBM CP850 */
#ifdef LATIN2
    "cp852",              FC_CP852,   0, /* IBM CP852 */
#endif /* LATIN2 */
#ifdef CYRILLIC
    "cp866-cyrillic",     FC_CP866,   0, /* CP866 Cyrillic */
    "cyrillic-iso",       FC_CYRILL,  0, /* ISO Latin/Cyrillic Alphabet */
#endif /* CYRILLIC */
    "danish",             FC_NOASCII, 0, /* Norwegian and Danish NRC */
#ifdef COMMENT
#ifdef KANJI
    "dec-kanji",          FC_JDEC,    0, /* Japanese DEC Kanji */
#endif /* KANJI */
#endif /* COMMENT */
    "dec-multinational",  FC_DECMCS,  0, /* DEC multinational character set */
    "dg-international",   FC_DGMCS,   0, /* Data General multinational */
    "dutch",              FC_DUASCII, 0, /* Dutch NRC */
    "finnish",            FC_FIASCII, 0, /* Finnish NRC */
    "french",             FC_FRASCII, 0, /* French NRC */
    "fr-canadian",        FC_FCASCII, CM_INV, /* French Canadian NRC */
    "german",             FC_GEASCII, 0, /* German NRC */
    "hungarian",          FC_HUASCII, 0, /* Hungarian NRC */
    "italian",            FC_ITASCII, 0, /* Italian NRC */
#ifdef COMMENT
/* Kanji terminal character sets not implemented yet */
#ifdef KANJI
    "japanese-euc",       FC_JEUC,    0, /* Japanese EUC */
    "jis7-kanji",         FC_JIS7,    0, /* Japanese JIS7 7bit code */
#endif /* KANJI */
#endif /* COMMENT */
#ifdef CYRILLIC
    "koi8-cyrillic",      FC_KOI8,    0, /* Old KOI-8 Cyrillic */
#endif /* CYRILLIC */
#ifdef LATIN2
    "l",                  FC_1LATIN,  CM_ABR|CM_INV,
    "la",                 FC_1LATIN,  CM_ABR|CM_INV,
    "lat",                FC_1LATIN,  CM_ABR|CM_INV,
    "lati",               FC_1LATIN,  CM_ABR|CM_INV,
    "latin",              FC_1LATIN,  CM_ABR|CM_INV,
#endif /* LATIN2 */
    "latin1-iso",         FC_1LATIN,  0, /* ISO Latin Alphabet 1 */
#ifdef LATIN2
    "latin2-iso",         FC_2LATIN,  0, /* ISO Latin Alphabet 2 */
#endif /* LATIN2 */
    "macintosh-latin",    FC_APPQD,   0, /* "Extended Mac Latin */
    "next-multinational", FC_NEXT,    0, /* NeXT workstation */
    "norwegian",          FC_NOASCII, 0, /* Norwegian and Danish NRC */
    "portuguese",         FC_POASCII, 0, /* Portuguese NRC */
#ifdef COMMENT
/* Kanji terminal character sets not implemented yet. */
#ifdef KANJI
    "shift-jis-kanji",    FC_SHJIS,   0, /* Japanese Kanji Shift-JIS */
#endif /* KANJI */
#endif /* COMMENT */
#ifdef CYRILLIC
    "short-koi",          FC_KOI7,    0, /* Short KOI Cyrillic */
#endif /* CYRILLIC */
    "spanish",            FC_SPASCII, 0, /* Spanish NRC */
    "swedish",            FC_SWASCII, 0, /* Swedish NRC */
    "swiss",              FC_CHASCII, 0, /* Swiss NRC */
    "transparent",        FC_TRANSP,  0  /* Transparent */
};
int ntermc = (sizeof(ttcstab) / sizeof(struct keytab)); /* size of table */

/*
 Languages:

 This table allows C-Kermit to have a SET LANGUAGE command to apply special
 language-specific rules when translating from a character set that contains
 national characters into plain ASCII, like German umlaut-a becomes ae.

 Originally, I thought it would be a good idea to let SET LANGUAGE also select
 an appropriate FILE CHARACTER-SET and TRANSFER CHARACTER-SET automatically,
 and these are included in the langinfo structure.  Later I realized that this
 was a bad idea.  Users are confused by unexpected side effects.  If this
 functionality is desired, it's better to define a macro to do it.
*/

struct langinfo langs[] = {
/*  Language code   File Charset Xfer Charset Name */
    L_USASCII,      FC_USASCII,  TC_USASCII,  "ASCII (American English)",
    L_DANISH,       FC_NOASCII,  TC_1LATIN,   "Danish",
    L_DUTCH,        FC_DUASCII,  TC_1LATIN,   "Dutch",
    L_FINNISH,      FC_FIASCII,  TC_1LATIN,   "Finnish",
    L_FRENCH,       FC_FRASCII,  TC_1LATIN,   "French",
    L_GERMAN,       FC_GEASCII,  TC_1LATIN,   "German",
    L_HUNGARIAN,    FC_HUASCII,  TC_2LATIN,   "Hungarian",
    L_ICELANDIC,    FC_USASCII,  TC_1LATIN,   "Icelandic",
    L_ITALIAN,      FC_ITASCII,  TC_1LATIN,   "Italian",
#ifdef KANJI
    L_JAPANESE,     FC_JEUC,     TC_JEUC,     "Japanese",
#endif /* KANJI */
    L_NORWEGIAN,    FC_NOASCII,  TC_1LATIN,   "Norwegian",
    L_PORTUGUESE,   FC_POASCII,  TC_1LATIN,   "Portuguese",
#ifdef CYRILLIC
    L_RUSSIAN,      FC_CP866,    TC_CYRILL,   "Russian",
#endif /* CYRILLIC */
    L_SPANISH,      FC_SPASCII,  TC_1LATIN,   "Spanish",
    L_SWEDISH,      FC_SWASCII,  TC_1LATIN,   "Swedish",
    L_SWISS,        FC_CHASCII,  TC_1LATIN,   "Swiss"
};
int nlangs = (sizeof(langs) / sizeof(struct langinfo));

/*
  Keyword table for the SET LANGUAGE command.
  Only a few of these (German, Scandinavian, etc) actually do anything.
  The language is used to invoke special translation rules when converting
  from an 8-bit character set to ASCII; for example, German u-diaeresis
  becomes "ue", Dutch y-diaeresis becomes "ij".  Languages without associated
  rules are invisible (CM_INV).
*/
struct keytab lngtab[] = {
    "ascii",            L_USASCII,    CM_INV,
    "danish",           L_DANISH,     0,
    "dutch",            L_DUTCH,      0,
    "english",          L_USASCII,    CM_INV,
    "finnish",          L_FINNISH,    0,
    "french",           L_FRENCH,     0,
    "german",           L_GERMAN,     0,
    "hungarian",        L_HUNGARIAN,  CM_INV,
    "icelandic",        L_ICELANDIC,  0,
    "italian",          L_ITALIAN,    CM_INV,
#ifdef KANJI
    "japanese",         L_JAPANESE,   CM_INV,
#endif /* KANJI */
    "norwegian",        L_NORWEGIAN,  0,
    "none",             L_USASCII,    0,
    "portuguese",       L_PORTUGUESE, CM_INV,
#ifdef CYRILLIC
    "russian",          L_RUSSIAN,    0,
#endif /* CYRILLIC */
    "spanish",          L_SPANISH,    CM_INV,
    "swedish",          L_SWEDISH,    0,
#ifdef CYRILLIC
    "ukrainian",        L_RUSSIAN,    0
#endif /* CYRILLIC */
};
int nlng = (sizeof(lngtab) / sizeof(struct keytab)); /* how many languages */


/* Translation tables ... */

/*
  For each pair of (transfer,file) character sets, we need two translation
  functions, one for sending, one for receiving.
*/

/*
  Here is the first table, Latin-1 to ASCII, fully annotated...
  This one is absolutely NOT invertible, since we're going from an 8-bit
  set to a 7-bit set.  Accented letters are mapped to unaccented
  equivalents, C1 control characters are all translated to "?", etc.
*/
CHAR
yl1as[] = {  /* ISO 8859-1 Latin Alphabet 1 to US ASCII */
      /*  Source character    Description               => Translation */
      /*  Dec row/col Set                                           */
  0,  /*  000  00/00  C0 NUL  Ctrl-@                    =>  (self)  */
  1,  /*  001  00/01  C0 SOH  Ctrl-A                    =>  (self)  */
  2,  /*  002  00/02  C0 STX  Ctrl-B                    =>  (self)  */
  3,  /*  003  00/03  C0 ETX  Ctrl-C                    =>  (self)  */
  4,  /*  004  00/04  C0 EOT  Ctrl-D                    =>  (self)  */
  5,  /*  005  00/05  C0 ENQ  Ctrl-E                    =>  (self)  */
  6,  /*  006  00/06  C0 ACK  Ctrl-F                    =>  (self)  */
  7,  /*  007  00/07  C0 BEL  Ctrl-G                    =>  (self)  */
  8,  /*  008  00/08  C0 BS   Ctrl-H                    =>  (self)  */
  9,  /*  009  00/09  C0 HT   Ctrl-I                    =>  (self)  */
 10,  /*  010  00/10  C0 LF   Ctrl-J                    =>  (self)  */
 11,  /*  011  00/11  C0 VT   Ctrl-K                    =>  (self)  */
 12,  /*  012  00/12  C0 FF   Ctrl-L                    =>  (self)  */
 13,  /*  013  00/13  C0 CR   Ctrl-M                    =>  (self)  */
 14,  /*  014  00/14  C0 SO   Ctrl-N                    =>  (self)  */
 15,  /*  015  00/15  C0 SI   Ctrl-O                    =>  (self)  */
 16,  /*  016  01/00  C0 DLE  Ctrl-P                    =>  (self)  */
 17,  /*  017  01/01  C0 DC1  Ctrl-Q                    =>  (self)  */
 18,  /*  018  01/02  C0 DC2  Ctrl-R                    =>  (self)  */
 19,  /*  019  01/03  C0 DC3  Ctrl-S                    =>  (self)  */
 20,  /*  020  01/04  C0 DC4  Ctrl-T                    =>  (self)  */
 21,  /*  021  01/05  C0 NAK  Ctrl-U                    =>  (self)  */
 22,  /*  022  01/06  C0 SYN  Ctrl-V                    =>  (self)  */
 23,  /*  023  01/07  C0 ETB  Ctrl-W                    =>  (self)  */
 24,  /*  024  01/08  C0 CAN  Ctrl-X                    =>  (self)  */
 25,  /*  025  01/09  C0 EM   Ctrl-Y                    =>  (self)  */
 26,  /*  026  01/10  C0 SUB  Ctrl-Z                    =>  (self)  */
 27,  /*  027  01/11  C0 ESC  Ctrl-[                    =>  (self)  */
 28,  /*  028  01/12  C0 FS   Ctrl-\                    =>  (self)  */
 29,  /*  029  01/13  C0 GS   Ctrl-]                    =>  (self)  */
 30,  /*  030  01/14  C0 RS   Ctrl-^                    =>  (self)  */
 31,  /*  031  01/15  C0 US   Ctrl-_                    =>  (self)  */
 32,  /*  032  02/00     SP   Space                     =>  (self)  */
 33,  /*  033  02/01  G0 !    Exclamation mark          =>  (self)  */
 34,  /*  034  02/02  G0 "    Doublequote               =>  (self)  */
 35,  /*  035  02/03  G0 #    Number sign               =>  (self)  */
 36,  /*  036  02/04  G0 $    Dollar sign               =>  (self)  */
 37,  /*  037  02/05  G0 %    Percent sign              =>  (self)  */
 38,  /*  038  02/06  G0 &    Ampersand                 =>  (self)  */
 39,  /*  039  02/07  G0 '    Apostrophe                =>  (self)  */
 40,  /*  040  02/08  G0 (    Left parenthesis          =>  (self)  */
 41,  /*  041  02/09  G0 )    Right parenthesis         =>  (self)  */
 42,  /*  042  02/10  G0 *    Asterisk                  =>  (self)  */
 43,  /*  043  02/11  G0 +    Plus sign                 =>  (self)  */
 44,  /*  044  02/12  G0 ,    Comma                     =>  (self)  */
 45,  /*  045  02/13  G0 -    Hyphen, minus sign        =>  (self)  */
 46,  /*  046  02/14  G0 .    Period, full stop         =>  (self)  */
 47,  /*  047  02/15  G0 /    Slash, solidus            =>  (self)  */
 48,  /*  048  03/00  G0 0    Digit 0                   =>  (self)  */
 49,  /*  049  03/01  G0 1    Digit 1                   =>  (self)  */
 50,  /*  050  03/02  G0 2    Digit 2                   =>  (self)  */
 51,  /*  051  03/03  G0 3    Digit 3                   =>  (self)  */
 52,  /*  052  03/04  G0 4    Digit 4                   =>  (self)  */
 53,  /*  053  03/05  G0 5    Digit 5                   =>  (self)  */
 54,  /*  054  03/06  G0 6    Digit 6                   =>  (self)  */
 55,  /*  055  03/07  G0 7    Digit 7                   =>  (self)  */
 56,  /*  056  03/08  G0 8    Digit 8                   =>  (self)  */
 57,  /*  057  03/09  G0 9    Digit 9                   =>  (self)  */
 58,  /*  058  03/10  G0 :    Colon                     =>  (self)  */
 59,  /*  059  03/11  G0 ;    Semicolon                 =>  (self)  */
 60,  /*  060  03/12  G0 <    Less-than sign            =>  (self)  */
 61,  /*  061  03/13  G0 =    Equals sign               =>  (self)  */
 62,  /*  062  03/14  G0 >    Greater-than sign         =>  (self)  */
 63,  /*  063  03/15  G0 ?    Question mark             =>  (self)  */
 64,  /*  064  04/00  G0 @    Commercial at sign        =>  (self)  */
 65,  /*  065  04/01  G0 A    Letter A                  =>  (self)  */
 66,  /*  066  04/02  G0 B    Letter B                  =>  (self)  */
 67,  /*  067  04/03  G0 C    Letter C                  =>  (self)  */
 68,  /*  068  04/04  G0 D    Letter D                  =>  (self)  */
 69,  /*  069  04/05  G0 E    Letter E                  =>  (self)  */
 70,  /*  070  04/06  G0 F    Letter F                  =>  (self)  */
 71,  /*  071  04/07  G0 G    Letter G                  =>  (self)  */
 72,  /*  072  04/08  G0 H    Letter H                  =>  (self)  */
 73,  /*  073  04/09  G0 I    Letter I                  =>  (self)  */
 74,  /*  074  04/10  G0 J    Letter J                  =>  (self)  */
 75,  /*  075  04/11  G0 K    Letter K                  =>  (self)  */
 76,  /*  076  04/12  G0 L    Letter L                  =>  (self)  */
 77,  /*  077  04/13  G0 M    Letter M                  =>  (self)  */
 78,  /*  078  04/14  G0 N    Letter N                  =>  (self)  */
 79,  /*  079  04/15  G0 O    Letter O                  =>  (self)  */
 80,  /*  080  05/00  G0 P    Letter P                  =>  (self)  */
 81,  /*  081  05/01  G0 Q    Letter Q                  =>  (self)  */
 82,  /*  082  05/02  G0 R    Letter R                  =>  (self)  */
 83,  /*  083  05/03  G0 S    Letter S                  =>  (self)  */
 84,  /*  084  05/04  G0 T    Letter T                  =>  (self)  */
 85,  /*  085  05/05  G0 U    Letter U                  =>  (self)  */
 86,  /*  086  05/06  G0 V    Letter V                  =>  (self)  */
 87,  /*  087  05/07  G0 W    Letter W                  =>  (self)  */
 88,  /*  088  05/08  G0 X    Letter X                  =>  (self)  */
 89,  /*  089  05/09  G0 Y    Letter Y                  =>  (self)  */
 90,  /*  090  05/10  G0 Z    Letter Z                  =>  (self)  */
 91,  /*  091  05/11  G0 [    Left square bracket       =>  (self)  */
 92,  /*  092  05/12  G0 \    Reverse slash             =>  (self)  */
 93,  /*  093  05/13  G0 ]    Right square bracket      =>  (self)  */
 94,  /*  094  05/14  G0 ^    Circumflex accent         =>  (self)  */
 95,  /*  095  05/15  G0 _    Underline, low line       =>  (self)  */
 96,  /*  096  06/00  G0 `    Grave accent              =>  (self)  */
 97,  /*  097  06/01  G0 a    Letter a                  =>  (self)  */
 98,  /*  098  06/02  G0 b    Letter b                  =>  (self)  */
 99,  /*  099  06/03  G0 c    Letter c                  =>  (self)  */
100,  /*  100  06/04  G0 d    Letter d                  =>  (self)  */
101,  /*  101  06/05  G0 e    Letter e                  =>  (self)  */
102,  /*  102  06/06  G0 f    Letter f                  =>  (self)  */
103,  /*  103  06/07  G0 g    Letter g                  =>  (self)  */
104,  /*  104  06/08  G0 h    Letter h                  =>  (self)  */
105,  /*  105  06/09  G0 i    Letter i                  =>  (self)  */
106,  /*  106  06/10  G0 j    Letter j                  =>  (self)  */
107,  /*  107  06/11  G0 k    Letter k                  =>  (self)  */
108,  /*  108  06/12  G0 l    Letter l                  =>  (self)  */
109,  /*  109  06/13  G0 m    Letter m                  =>  (self)  */
110,  /*  110  06/14  G0 n    Letter n                  =>  (self)  */
111,  /*  111  06/15  G0 o    Letter o                  =>  (self)  */
112,  /*  112  07/00  G0 p    Letter p                  =>  (self)  */
113,  /*  113  07/01  G0 q    Letter q                  =>  (self)  */
114,  /*  114  07/02  G0 r    Letter r                  =>  (self)  */
115,  /*  115  07/03  G0 s    Letter s                  =>  (self)  */
116,  /*  116  07/04  G0 t    Letter t                  =>  (self)  */
117,  /*  117  07/05  G0 u    Letter u                  =>  (self)  */
118,  /*  118  07/06  G0 v    Letter v                  =>  (self)  */
119,  /*  119  07/07  G0 w    Letter w                  =>  (self)  */
120,  /*  120  07/08  G0 x    Letter x                  =>  (self)  */
121,  /*  121  07/09  G0 y    Letter y                  =>  (self)  */
122,  /*  122  07/10  G0 z    Letter z                  =>  (self)  */
123,  /*  123  07/11  G0 {    Left curly bracket        =>  (self)  */
124,  /*  124  07/12  G0 |    Vertical bar              =>  (self)  */
125,  /*  125  07/13  G0 }    Right curly bracket       =>  (self)  */
126,  /*  126  07/14  G0 ~    Tilde                     =>  (self)  */
127,  /*  127  07/15     DEL  Delete, Rubout            =>  (self)  */
UNK,  /*  128  08/00  C1                                =>  UNK     */
UNK,  /*  129  08/01  C1                                =>  UNK     */
UNK,  /*  130  08/02  C1                                =>  UNK     */
UNK,  /*  131  08/03  C1                                =>  UNK     */
UNK,  /*  132  08/04  C1 IND                            =>  UNK     */
UNK,  /*  133  08/05  C1 NEL                            =>  UNK     */
UNK,  /*  134  08/06  C1 SSA                            =>  UNK     */
UNK,  /*  135  08/07  C1 ESA                            =>  UNK     */
UNK,  /*  136  08/08  C1 HTS                            =>  UNK     */
UNK,  /*  137  08/09  C1                                =>  UNK     */
UNK,  /*  138  08/10  C1                                =>  UNK     */
UNK,  /*  139  08/11  C1                                =>  UNK     */
UNK,  /*  140  08/12  C1                                =>  UNK     */
UNK,  /*  141  08/13  C1 RI                             =>  UNK     */
UNK,  /*  142  08/14  C1 SS2                            =>  UNK     */
UNK,  /*  143  08/15  C1 SS3                            =>  UNK     */
UNK,  /*  144  09/00  C1 DCS                            =>  UNK     */
UNK,  /*  145  09/01  C1                                =>  UNK     */
UNK,  /*  146  09/02  C1                                =>  UNK     */
UNK,  /*  147  09/03  C1 STS                            =>  UNK     */
UNK,  /*  148  09/04  C1                                =>  UNK     */
UNK,  /*  149  09/05  C1                                =>  UNK     */
UNK,  /*  150  09/06  C1 SPA                            =>  UNK     */
UNK,  /*  151  09/07  C1 EPA                            =>  UNK     */
UNK,  /*  152  09/08  C1                                =>  UNK     */
UNK,  /*  153  09/09  C1                                =>  UNK     */
UNK,  /*  154  09/10  C1                                =>  UNK     */
UNK,  /*  155  09/11  C1 CSI                            =>  UNK     */
UNK,  /*  156  09/12  C1 ST                             =>  UNK     */
UNK,  /*  157  09/13  C1 OSC                            =>  UNK     */
UNK,  /*  158  09/14  C1 PM                             =>  UNK     */
UNK,  /*  159  09/15  C1 APC                            =>  UNK     */
 32,  /*  160  10/00  G1      No-break space            =>  SP      */
 33,  /*  161  10/01  G1      Inverted exclamation      =>  !       */
 99,  /*  162  10/02  G1      Cent sign                 =>  c       */
 35,  /*  163  10/03  G1      Pound sign                =>  #       */
 36,  /*  164  10/04  G1      Currency sign             =>  $       */
 89,  /*  165  10/05  G1      Yen sign                  =>  Y       */
124,  /*  166  10/06  G1      Broken bar                =>  |       */
 80,  /*  167  10/07  G1      Paragraph sign            =>  P       */
 34,  /*  168  10/08  G1      Diaeresis                 =>  "       */
 67,  /*  169  10/09  G1      Copyright sign            =>  C       */
 97,  /*  170  10/10  G1      Feminine ordinal          =>  a       */
 34,  /*  171  10/11  G1      Left angle quotation      =>  "       */
126,  /*  172  10/12  G1      Not sign                  =>  ~       */
 45,  /*  173  10/13  G1      Soft hyphen               =>  -       */
 82,  /*  174  10/14  G1      Registered trade mark     =>  R       */
 95,  /*  175  10/15  G1      Macron                    =>  _       */
111,  /*  176  11/00  G1      Degree sign, ring above   =>  o       */
UNK,  /*  177  11/01  G1      Plus-minus sign           =>  UNK     */
 50,  /*  178  11/02  G1      Superscript two           =>  2       */
 51,  /*  179  11/03  G1      Superscript three         =>  3       */
 39,  /*  180  11/04  G1      Acute accent              =>  '       */
117,  /*  181  11/05  G1      Micro sign                =>  u       */
 45,  /*  182  11/06  G1      Pilcrow sign              =>  -       */
 45,  /*  183  11/07  G1      Middle dot                =>  -       */
 44,  /*  184  11/08  G1      Cedilla                   =>  ,       */
 49,  /*  185  11/09  G1      Superscript one           =>  1       */
111,  /*  186  11/10  G1      Masculine ordinal         =>  o       */
 34,  /*  187  11/11  G1      Right angle quotation     =>  "       */
UNK,  /*  188  11/12  G1      One quarter               =>  UNK     */
UNK,  /*  189  11/13  G1      One half                  =>  UNK     */
UNK,  /*  190  11/14  G1      Three quarters            =>  UNK     */
 63,  /*  191  11/15  G1      Inverted question mark    =>  ?       */
 65,  /*  192  12/00  G1      A grave                   =>  A       */
 65,  /*  193  12/01  G1      A acute                   =>  A       */
 65,  /*  194  12/02  G1      A circumflex              =>  A       */
 65,  /*  195  12/03  G1      A tilde                   =>  A       */
 65,  /*  196  12/04  G1      A diaeresis               =>  A       */
 65,  /*  197  12/05  G1      A ring above              =>  A       */
 65,  /*  198  12/06  G1      A with E                  =>  A       */
 67,  /*  199  12/07  G1      C Cedilla                 =>  C       */
 69,  /*  200  12/08  G1      E grave                   =>  E       */
 69,  /*  201  12/09  G1      E acute                   =>  E       */
 69,  /*  202  12/10  G1      E circumflex              =>  E       */
 69,  /*  203  12/11  G1      E diaeresis               =>  E       */
 73,  /*  204  12/12  G1      I grave                   =>  I       */
 73,  /*  205  12/13  G1      I acute                   =>  I       */
 73,  /*  206  12/14  G1      I circumflex              =>  I       */
 73,  /*  207  12/15  G1      I diaeresis               =>  I       */
 68,  /*  208  13/00  G1      Icelandic Eth             =>  D       */
 78,  /*  209  13/01  G1      N tilde                   =>  N       */
 79,  /*  210  13/02  G1      O grave                   =>  O       */
 79,  /*  211  13/03  G1      O acute                   =>  O       */
 79,  /*  212  13/04  G1      O circumflex              =>  O       */
 79,  /*  213  13/05  G1      O tilde                   =>  O       */
 79,  /*  214  13/06  G1      O diaeresis               =>  O       */
120,  /*  215  13/07  G1      Multiplication sign       =>  x       */
 79,  /*  216  13/08  G1      O oblique stroke          =>  O       */
 85,  /*  217  13/09  G1      U grave                   =>  U       */
 85,  /*  218  13/10  G1      U acute                   =>  U       */
 85,  /*  219  13/11  G1      U circumflex              =>  U       */
 85,  /*  220  13/12  G1      U diaeresis               =>  U       */
 89,  /*  221  13/13  G1      Y acute                   =>  Y       */
 84,  /*  222  13/14  G1      Icelandic Thorn           =>  T       */
115,  /*  223  13/15  G1      German sharp s            =>  s       */
 97,  /*  224  14/00  G1      a grave                   =>  a       */
 97,  /*  225  14/01  G1      a acute                   =>  a       */
 97,  /*  226  14/02  G1      a circumflex              =>  a       */
 97,  /*  227  14/03  G1      a tilde                   =>  a       */
 97,  /*  228  14/04  G1      a diaeresis               =>  a       */
 97,  /*  229  14/05  G1      a ring above              =>  a       */
 97,  /*  230  14/06  G1      a with e                  =>  a       */
 99,  /*  231  14/07  G1      c cedilla                 =>  c       */
101,  /*  232  14/08  G1      e grave                   =>  e       */
101,  /*  233  14/09  G1      e acute                   =>  e       */
101,  /*  234  14/10  G1      e circumflex              =>  e       */
101,  /*  235  14/11  G1      e diaeresis               =>  e       */
105,  /*  236  14/12  G1      i grave                   =>  i       */
105,  /*  237  14/13  G1      i acute                   =>  i       */
105,  /*  238  14/14  G1      i circumflex              =>  i       */
105,  /*  239  14/15  G1      i diaeresis               =>  i       */
100,  /*  240  15/00  G1      Icelandic eth             =>  d       */
110,  /*  241  15/01  G1      n tilde                   =>  n       */
111,  /*  242  15/02  G1      o grave                   =>  o       */
111,  /*  243  15/03  G1      o acute                   =>  o       */
111,  /*  244  15/04  G1      o circumflex              =>  o       */
111,  /*  245  15/05  G1      o tilde                   =>  o       */
111,  /*  246  15/06  G1      o diaeresis               =>  o       */
 47,  /*  247  15/07  G1      Division sign             =>  /       */
111,  /*  248  15/08  G1      o oblique stroke          =>  o       */
117,  /*  249  15/09  G1      u grave                   =>  u       */
117,  /*  250  15/10  G1      u acute                   =>  u       */
117,  /*  251  15/11  G1      u circumflex              =>  u       */
117,  /*  252  15/12  G1      u diaeresis               =>  u       */
121,  /*  253  15/13  G1      y acute                   =>  y       */
116,  /*  254  15/14  G1      Icelandic thorn           =>  t       */
121   /*  255  15/15  G1      y diaeresis               =>  y       */
};


/* Translation tables for ISO Latin Alphabet 1 to local file character sets */

/*
  Most of the remaining tables are not annotated like the one above, because
  the size of the resulting source file would be ridiculous.  Each row in the
  following tables corresponds to a column of ISO 8859-1.
*/

CHAR
yl185[] = {  /* ISO 8859-1 Latin Alphabet 1 (Latin-1) to IBM Code Page 850 */
/*
  This is IBM's official invertible translation.  Reference: IBM Character
  Data Representation Architecture (CDRA), Level 1, Registry, SC09-1291-00
  (1990), p.152.  (Note: Latin-1 is IBM Code Page 00819.)  Note the bizarre
  rearrangement of C0 controls and DEL.
*/
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
186, 205, 201, 187, 200, 188, 204, 185, 203, 202, 206, 223, 220, 219, 254, 242,
179, 196, 218, 191, 192, 217, 195, 180, 194, 193, 197, 176, 177, 178, 213, 159,
255, 173, 189, 156, 207, 190, 221, 245, 249, 184, 166, 174, 170, 240, 169, 238,
248, 241, 253, 252, 239, 230, 244, 250, 247, 251, 167, 175, 172, 171, 243, 168,
183, 181, 182, 199, 142, 143, 146, 128, 212, 144, 210, 211, 222, 214, 215, 216,
209, 165, 227, 224, 226, 229, 153, 158, 157, 235, 233, 234, 154, 237, 232, 225,
133, 160, 131, 198, 132, 134, 145, 135, 138, 130, 136, 137, 141, 161, 140, 139,
208, 164, 149, 162, 147, 228, 148, 246, 155, 151, 163, 150, 129, 236, 231, 152
};

CHAR
y85l1[] = {  /* IBM Code Page 850 to Latin-1 */
/*
  This is from IBM CDRA page 153.  It is the inverse of yl185[].
  As of edit 183, this table is no longer pure CDRA.  The translations 
  involving C0 controls have been removed.
*/
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
199, 252, 233, 226, 228, 224, 229, 231, 234, 235, 232, 239, 238, 236, 196, 197,
201, 230, 198, 244, 246, 242, 251, 249, 255, 214, 220, 248, 163, 216, 215, 159,
225, 237, 243, 250, 241, 209, 170, 186, 191, 174, 172, 189, 188, 161, 171, 187,
155, 156, 157, 144, 151, 193, 194, 192, 169, 135, 128, 131, 133, 162, 165, 147,
148, 153, 152, 150, 145, 154, 227, 195, 132, 130, 137, 136, 134, 129, 138, 164,
240, 208, 202, 203, 200, 158, 205, 206, 207, 149, 146, 141, 140, 166, 204, 139,
211, 223, 212, 210, 245, 213, 181, 254, 222, 218, 219, 217, 253, 221, 175, 180,
173, 177, 143, 190, 182, 167, 247, 184, 176, 168, 183, 185, 179, 178, 142, 160
};

CHAR
yl143[] = {  /* Latin-1 to IBM Code Page 437 */
/*
  Although the IBM CDRA does not include an official translation between CP437
  and ISO Latin Alphabet 1, it does include an official, invertible
  translation between CP437 and CP850 (page 196), and another from CP850 to
  Latin-1 (CP819) (page 153).  This translation was obtained with a two-step
  process based on those tables.
  As of edit 183, the translation is modified to leave C0 controls alone.
*/
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
186, 205, 201, 187, 200, 188, 204, 185, 203, 202, 206, 223, 220, 219, 254, 242,
179, 196, 218, 191, 192, 217, 195, 180, 194, 193, 197, 176, 177, 178, 213, 159,
255, 173, 155, 156, 207, 157, 221, 245, 249, 184, 166, 174, 170, 240, 169, 238,
248, 241, 253, 252, 239, 230, 244, 250, 247, 251, 167, 175, 172, 171, 243, 168,
183, 181, 182, 199, 142, 143, 146, 128, 212, 144, 210, 211, 222, 214, 215, 216,
209, 165, 227, 224, 226, 229, 153, 158, 190, 235, 233, 234, 154, 237, 232, 225,
133, 160, 131, 198, 132, 134, 145, 135, 138, 130, 136, 137, 141, 161, 140, 139,
208, 164, 149, 162, 147, 228, 148, 246, 189, 151, 163, 150, 129, 236, 231, 152
};

CHAR
y43l1[] = {  /* IBM Code Page 437 to Latin-1 */
/*
  This table is the inverse of yl143[].
*/
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
199, 252, 233, 226, 228, 224, 229, 231, 234, 235, 232, 239, 238, 236, 196, 197,
201, 230, 198, 244, 246, 242, 251, 249, 255, 214, 220, 162, 163, 165, 215, 159,
225, 237, 243, 250, 241, 209, 170, 186, 191, 174, 172, 189, 188, 161, 171, 187,
155, 156, 157, 144, 151, 193, 194, 192, 169, 135, 128, 131, 133, 248, 216, 147,
148, 153, 152, 150, 145, 154, 227, 195, 132, 130, 137, 136, 134, 129, 138, 164,
240, 208, 202, 203, 200, 158, 205, 206, 207, 149, 146, 141, 140, 166, 204, 139,
211, 223, 212, 210, 245, 213, 181, 254, 222, 218, 219, 217, 253, 221, 175, 180,
173, 177, 143, 190, 182, 167, 247, 184, 176, 168, 183, 185, 179, 178, 142, 160
};

CHAR
yl1aq[] = {  /* Latin-1 to Extended Mac Latin (based on Apple QuickDraw) */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
182, 183, 184, 185, 189, 196, 197, 198, 206, 207, 210, 211, 217, 218, 195, 212,
209, 215, 213, 226, 227, 228, 240, 245, 246, 247, 249, 250, 251, 253, 254, 255,
202, 193, 162, 163, 219, 180, 201, 164, 172, 169, 187, 199, 194, 208, 168, 248,
161, 177, 170, 173, 171, 181, 166, 225, 252, 176, 188, 200, 178, 179, 186, 192,
203, 231, 229, 204, 128, 129, 174, 130, 233, 131, 230, 232, 237, 234, 235, 236,
220, 132, 241, 238, 239, 205, 133, 165, 175, 244, 242, 243, 134, 160, 222, 167,
136, 135, 137, 139, 138, 140, 190, 141, 143, 142, 144, 145, 147, 146, 148, 149,
221, 150, 152, 151, 153, 155, 154, 214, 191, 157, 156, 158, 159, 224, 223, 216
};

CHAR
yl1du[] = {  /* Latin-1 to Dutch ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, UNK,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK,  39, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK,  35, 124, UNK, UNK,  93, 123,  67, UNK,  34, UNK,  45,  82, UNK,
 91, UNK, UNK, UNK, 126, 117, UNK, UNK,  44, UNK, UNK,  34, 125,  92,  64,  63,
 65,  65,  65,  65,  91,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
 97,  97,  97,  97,  97,  97,  97,  99, 101, 101, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 111,  47, 111, 117, 117, 117, 117, 121, UNK,  91
};

CHAR
yl1fi[] = {  /* Latin-1 to Finnish ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK, UNK,  95,
UNK,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, UNK,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  91,  93,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  92, 120,  79,  85,  85,  85,  94,  89, UNK, 115,
 97,  97,  97,  97, 123, 125,  97,  99, 101,  96, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 124,  47, 111, 117, 117, 117, 126, 121, UNK, 121
};

CHAR
yl1fr[] = {  /* Latin-1 to French ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, UNK,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK,  35, UNK, UNK, UNK,  93,  34,  67, UNK,  34, UNK,  45,  82, UNK,
 91, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  65,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
 64,  97,  97,  97,  97,  97,  97,  92, 125, 123, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 111,  47, 111, 124, 117, 117, 117, 121, UNK, 121
};

CHAR
yl1fc[] = {  /* Latin-1 to French-Canadian ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK, UNK,  95,
UNK,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  65,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
 64,  97,  91,  97,  97,  97,  97,  92, 125, 123,  93, 101, 105, 105,  94, 105,
UNK, 110, 111, 111,  96, 111, 111,  47, 111, 124, 117, 126, 117, 121, UNK, 121
};

CHAR
yl1ge[] = {  /* Latin-1 to German ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK,  64,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  91,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  92, 120,  79,  85,  85,  85,  93,  89, UNK, 126,
 97,  97,  97,  97, 123,  97,  97,  99, 101, 101, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 124,  47, 111, 117, 117, 117, 125, 121, UNK, 121
};

CHAR
yl1hu[] = {  /* Latin-1 to Hungarian ISO-646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK,  36, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK,  64, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  65,  65,  65,  67,  69,  91,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  92, 120,  79,  85,  85,  85,  93,  89, UNK, 115,
 97,  96,  97,  97,  97,  97,  97,  99, 101, 123, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 124,  47, 111, 117, 117, 117, 125, 121, UNK, 121
};

CHAR
yl1it[] = {  /* Latin-1 to Italian ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, UNK,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
UNK,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK,  35, UNK, UNK, UNK,  64,  34,  67, UNK,  34, UNK,  45,  82, UNK,
 91, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  65,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
123,  97,  97,  97,  97,  97,  97,  92, 125,  93, 101, 101, 126, 105, 105, 105,
UNK, 110, 124, 111, 111, 111, 111,  47, 111,  96, 117, 117, 117, 121, UNK, 121
};

CHAR
yl1ne[] = {  /* Latin-1 to NeXT */
/* NEED TO MAKE THIS ONE INVERTIBLE, LIKE CP850 */
/*
  Which means finding all the graphic characters in the NeXT set that have
  no equivalent in Latin-1 and assigning them to the UNK positions (mostly
  Latin-1 C1 controls).  Then make the ynel1[] table be the inverse of this
  one.  But first we should try to get an official Latin-1/NeXT translation
  table from NeXT, Inc.
*/
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
 32, 161, 162, 163, 168, 165, 181, 167, 200, 160, 227, 171, 190, UNK, 176, 197,
202, 209, 201, 204, 194, 157, 182, 183, 203, 192, 235, 187, 210, 211, 212, 191,
129, 130, 131, 132, 133, 134, 225, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 158, 233, 151, 152, 153, 154, 155, 156, 251,
213, 214, 215, 216, 217, 218, 241, 219, 220, 221, 222, 223, 224, 226, 228, 229,
230, 231, 236, 237, 238, 239, 240, 159, 249, 242, 243, 244, 246, 247, 252, 253
};

CHAR
yl1no[] = {  /* Latin-1 to Norwegian/Danish ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  65,  93,  91,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  79, 120,  92,  85,  85,  85,  85,  89, UNK, 115,
 97,  97,  97,  97,  97, 125, 123,  99, 101, 101, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 111,  47, 124, 117, 117, 117, 117, 121, UNK, 121
};

CHAR
yl1po[] = {  /* Latin-1 to Portuguese ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  91,  65,  65,  65,  92,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  93,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
 97,  97,  97, 123,  97,  97,  97, 124, 101, 101, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 125, 111,  47, 111, 117, 117, 117, 117, 121, UNK, 121
};

CHAR
yl1sp[] = {  /* Latin-1 to Spanish ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, UNK,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,  96, UNK, UNK, 126, 127,
126, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  91, UNK,  35, UNK, UNK, UNK,  64,  34,  67, UNK,  34, UNK,  45,  82, UNK,
123, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  93,
 65,  65,  65,  65,  65,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  92,  79,  79,  79,  79,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
124,  97,  97,  97,  97,  97,  97, 125, 101, 101, 101, 101, 105, 105, 105, 105,
UNK, 124, 111, 111, 111, 111, 111,  47, 111, 117, 117, 117, 117, 121, UNK, 121
};

CHAR
yl1sw[] = {  /* Latin-1 to Swedish ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK, UNK,  95,
UNK,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  91,  93,  65,  67,  69,  64,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  92, 120,  79,  85,  85,  85,  94,  89, UNK, 115,
 97,  97,  97,  97, 123, 125,  97,  99, 101,  96, 101, 101, 105, 105, 105, 105,
UNK, 110, 111, 111, 111, 111, 124,  47, 111, 117, 117, 117, 126, 121, UNK, 121
};

CHAR
yl1ch[] = {  /* Latin-1 to Swiss ISO 646 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, UNK,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
UNK,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, UNK, UNK, UNK, UNK, UNK,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, UNK, UNK, UNK, UNK, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32,  33, UNK, UNK, UNK, UNK, UNK, UNK,  34,  67, UNK,  34, UNK,  45,  82, UNK,
UNK, UNK, UNK, UNK,  39, 117, UNK, UNK,  44, UNK, UNK,  34, UNK, UNK, UNK,  63,
 65,  65,  65,  65,  65,  65,  65,  67,  69,  69,  69,  69,  73,  73,  73,  73,
UNK,  78,  79,  79,  79,  79,  79, 120,  79,  85,  85,  85,  85,  89, UNK, 115,
 64,  97,  97,  97, 123,  97,  97,  92,  95,  91,  93, 101, 105, 105,  94, 105,
UNK, 110, 111, 111,  96, 111, 124,  47, 111,  35, 117, 126, 125, 121, UNK, 121
};

CHAR
yl1dm[] = {  /* Latin-1 to DEC Multinational Character Set */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 32, 161, 162, 163, 168, 165, 124, 167,  34, 169, 170, 171, 126, UNK,  82, UNK,
176, 177, 178, 179,  39, 181, 182, 183,  44, 185, 186, 187, 188, 189, UNK, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
UNK, 209, 210, 211, 212, 213, 214, 120, 216, 217, 218, 219, 220, 221, UNK, 223,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
UNK, 241, 242, 243, 244, 245, 246,  47, 248, 249, 250, 251, 252, UNK, UNK, 253
};

CHAR
yl1dg[] = {  /* Latin-1 to Data General International Character Set */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 171, 167, 168, 166, 181, 191, 187, 189, 173, 169, 177, 161, 255, 174, 175,
188, 182, 164, 165, 190, 163, 178, 185, 186, 179, 170, 176, 223, 162, 220, 172,
193, 192, 194, 196, 195, 197, 198, 199, 201, 200, 202, 203, 205, 204, 206, 207,
184, 208, 210, 209, 211, 213, 212, 215, 214, 217, 216, 218, 219, 221, 222, 252,
225, 224, 226, 228, 227, 229, 230, 231, 233, 232, 234, 235, 237, 236, 238, 239,
183, 240, 242, 241, 243, 245, 244, 247, 246, 249, 248, 250, 251, 180, 254, 253
};


/* Local file character sets to ISO Latin Alphabet 1 */

#ifdef NOTUSED
CHAR
yasl1[] = {  /* ASCII to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127
};
#endif /* NOTUSED */

CHAR
yaql1[] = {  /* Extended Mac Latin (based on Apple Quickdraw) to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
196, 197, 199, 201, 209, 214, 220, 225, 224, 226, 228, 227, 229, 231, 233, 232,
234, 235, 237, 236, 238, 239, 241, 243, 242, 244, 246, 245, 250, 249, 251, 252,
221, 176, 162, 163, 167, 215, 182, 223, 174, 169, 178, 180, 168, 179, 198, 216,
185, 177, 188, 189, 165, 181, 128, 129, 130, 131, 190, 170, 186, 132, 230, 248,
191, 161, 172, 142, 133, 134, 135, 171, 187, 166, 160, 192, 195, 213, 136, 137,
173, 144, 138, 139, 143, 146, 247, 145, 255, 140, 141, 164, 208, 240, 222, 254,
253, 183, 147, 148, 149, 194, 202, 193, 203, 200, 205, 206, 207, 204, 211, 212,
150, 210, 218, 219, 217, 151, 152, 153, 175, 154, 155, 156, 184, 157, 158, 159
};

CHAR
ydul1[] = {  /* Dutch ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, 163,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
190,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 255, 189, 124,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 168, 164, 188,  39, 127
};

CHAR
yfil1[] = {  /* Finnish ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 196, 214, 197, 220,  95,
233,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 228, 246, 229, 252, 127
};

CHAR
yfrl1[] = {  /* French ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, 163,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
224,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 176, 231, 167,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 233, 249, 232, 168, 127
};

CHAR
yfcl1[] = {  /* French-Canadian ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
224,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 226, 231, 234, 238,  95,
244,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 233, 249, 232, 251, 127
};

CHAR
ygel1[] = {  /* German ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
167,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 196, 214, 220,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 228, 246, 252, 223, 127
};

CHAR
yitl1[] = {  /* Italian ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, 163,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
167,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 176, 231, 233,  94,  95,
249,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 224, 242, 232, 236, 127
};

CHAR
ynel1[] = {  /* NeXT to Latin-1 */
/* NEED TO MAKE THIS ONE INVERTIBLE */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
160, 192, 193, 194, 195, 196, 197, 199, 200, 201, 202, 203, 204, 205, 206, 207,
208, 209, 210, 211, 212, 213, 214, 217, 218, 219, 220, 221, 222, 181, 215, 247,
169, 161, 162, 163, UNK, 165, UNK, 167, 164, UNK, UNK, 171, UNK, UNK, UNK, UNK,
174, UNK, UNK, UNK, 183, 166, 182, UNK, UNK, UNK, UNK, 187, UNK, UNK, 172, 191,
185,  96, 180,  94, 126, 175, UNK, UNK, 168, 178, 176, 184, 179, UNK, UNK, UNK,
UNK, 177, 188, 189, 190, 224, 225, 226, 227, 228, 229, 231, 232, 233, 234, 235,
236, 198, 237, 170, 238, 239, 240, 241, UNK, 216, UNK, 186, 242, 243, 244, 245,
246, 230, 249, 250, 251, UNK, 252, 253, UNK, 248, UNK, 223, 254, 255, UNK, UNK
};

CHAR
ynol1[] = {  /* Norwegian/Danish ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 198, 216, 197,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 230, 248, 229, 126, 127
};

CHAR
ypol1[] = {  /* Portuguese ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 195, 199, 213,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 227, 231, 245, 126, 127
};

CHAR
yspl1[] = {  /* Spanish ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, 163,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
167,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 161, 209, 191,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 176, 241, 231, 126, 127
};

CHAR
yswl1[] = {  /* Swedish ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
201,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 196, 214, 197, 220,  95,
233,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 228, 246, 229, 252, 127
};

CHAR
ychl1[] = {  /* Swiss ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34, 249,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
224,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 233, 231, 234, 238, 232,
244,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 228, 246, 252, 251, 127
};

CHAR
yhul1[] = {  /* Hungarian ISO 646 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35, 164,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
193,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 201, 214, 220,  94,  95,
225,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 233, 246, 252,  34, 127
};

CHAR
ydml1[] = {  /* DEC Multinational Character Set to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255,
};

CHAR
ydgl1[] = {  /* Data General International to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 172, 189, 181, 178, 179, 164, 162, 163, 170, 186, 161, 191, 169, 174, 175,
187, 171, 182, 185, 253, 165, 177, 240, 208, 183, 184, 167, 176, 168, 180, 166,
193, 192, 194, 196, 195, 197, 198, 199, 201, 200, 202, 203, 205, 204, 206, 207,
209, 211, 210, 212, 214, 213, 216, 215, 218, 217, 219, 220, 190, 221, 222, 188,
225, 224, 226, 228, 227, 229, 230, 231, 233, 232, 234, 235, 237, 236, 238, 239,
241, 243, 242, 244, 246, 245, 248, 247, 250, 249, 251, 252, 223, 255, 254, 173
};


/* Translation tables for Cyrillic character sets */

#ifdef CYRILLIC
CHAR
ylcac[] = {  /* Latin/Cyrillic to CP866 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19, 208, 209,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
196, 179, 192, 217, 191, 218, 195, 193, 180, 194, 197, 176, 177, 178, 211, 216,
205, 186, 200, 188, 187, 201, 204, 202, 185, 203, 206, 223, 220, 219, 254, UNK,
255, 240, 132, 131, 242,  83,  73, 244,  74, 139, 141, 151, 138,  45, 246, 135,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
252, 241, 164, 163, 243, 115, 105, 245, 106, 171, 173, 231, 170,  21, 247, 167
};

CHAR
ylck8[] = {  /* Latin/Cyrillic to Old KOI-8 Cyrillic */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
UNK, 229, UNK, UNK, UNK,  83,  73,  73,  74, UNK, UNK, UNK, 235, UNK, 245, UNK,
225, 226, 247, 231, 228, 229, 246, 250, 233, 234, 235, 236, 237, 238, 239, 240,
242, 243, 244, 245, 230, 232, 227, 254, 251, 253, 255, 249, 248, 252, 224, 241,
193, 194, 215, 199, 196, 197, 214, 218, 201, 202, 203, 204, 205, 206, 207, 208,
210, 211, 212, 213, 198, 200, 195, 222, 219, 221, 223, 217, 216, 220, 192, 209,
UNK, 197, UNK, UNK, UNK, 115, 105, 105, 106, UNK, UNK, UNK, 203, UNK, 213, UNK
};

CHAR
yaclc[] = {  /* CP866 to Latin/Cyrillic */
/* NEED TO MAKE THIS ONE INVERTIBLE */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
161, 241, 164, 244, 167, 247, 174, 254, UNK, UNK, UNK, UNK, 240, UNK, UNK, UNK
};

CHAR
yk8lc[] = {  /* Old KOI-8 Cyrillic to Latin/Cyrillic */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
238, 208, 209, 230, 212, 213, 228, 211, 229, 216, 217, 218, 219, 220, 221, 222,
223, 239, 224, 225, 226, 227, 214, 210, 236, 235, 215, 232, 237, 233, 231, 234,
206, 176, 177, 198, 180, 181, 196, 179, 197, 184, 185, 186, 187, 188, 189, 190,
191, 207, 192, 193, 194, 195, 182, 178, 204, 203, 183, 200, 205, 201, 199, 127
};

CHAR
ylcsk[] = {  /* Latin/Cyrillic to Short KOI */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94, 127,
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32, 101, UNK, UNK, UNK,  83,  73,  73,  74, UNK, UNK, UNK, 107,  45, 117, UNK,
 97,  98, 119, 103, 100, 101, 118, 122, 105, 106, 107, 108, 109, 110, 111, 112,
114, 115, 116, 117, 102, 104,  99, 126, 123, 125,  39, 121, 120, 124,  96, 113,
 97,  98, 119, 103, 100, 101, 118, 122, 105, 106, 107, 108, 109, 110, 111, 112,
114, 115, 116, 117, 102, 104,  99, 126, 123, 125,  39, 121, 120, 124,  96, 113,
UNK, 101, UNK, UNK, UNK,  83,  73,  73,  74, UNK, UNK, UNK, 107, UNK, 117, UNK
};

CHAR yskcy[] = {  /* Short KOI to Latin/Cyrillic */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
206, 176, 177, 198, 180, 181, 196, 179, 197, 184, 185, 186, 187, 188, 189, 190,
191, 207, 192, 193, 194, 195, 182, 178, 204, 203, 183, 200, 205, 201, 199, 127
};
#endif /* CYRILLIC */

#ifdef LATIN2

/* Latin-2 tables */

CHAR
yl252[] = {  /* Latin-2 to Code Page 852 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
174, 175, 176, 177, 178, 179, 180, 185, 186, 187, 188, 191, 192, 193, 194, 195,
196, 197, 200, 201, 202, 203, 204, 205, 206, 217, 218, 219, 220, 223, 240, 254,
255, 164, 244, 157, 207, 149, 151, 245, 249, 230, 184, 155, 141, 170, 166, 189,
248, 165, 242, 136, 239, 150, 152, 243, 247, 231, 173, 156, 171, 241, 167, 190,
232, 181, 182, 198, 142, 145, 143, 128, 172, 144, 168, 211, 183, 214, 215, 210,
209, 227, 213, 224, 226, 138, 153, 158, 252, 222, 233, 235, 154, 237, 221, 225,
234, 160, 131, 199, 132, 146, 134, 135, 159, 130, 169, 137, 216, 161, 140, 212,
208, 228, 229, 162, 147, 139, 148, 246, 253, 133, 163, 251, 129, 236, 238, 250
};

CHAR
y52l2[] = {  /* Code Page 852 to Latin-2 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
199, 252, 233, 226, 228, 249, 230, 231, 179, 235, 213, 245, 238, 172, 196, 198,
201, 197, 229, 244, 246, 165, 181, 166, 182, 214, 220, 171, 187, 163, 215, 232,
225, 237, 243, 250, 161, 177, 174, 190, 202, 234, 173, 188, 200, 186, 128, 129,
130, 131, 132, 133, 134, 193, 194, 204, 170, 135, 136, 137, 138, 175, 191, 139,
140, 141, 142, 143, 144, 145, 195, 227, 146, 147, 148, 149, 150, 151, 152, 164,
240, 208, 207, 203, 239, 210, 205, 206, 236, 153, 154, 155, 156, 222, 217, 157,
211, 223, 212, 209, 241, 242, 169, 185, 192, 218, 224, 219, 253, 221, 254, 180,
158, 189, 178, 183, 162, 167, 247, 184, 176, 168, 255, 251, 216, 248, 159, 160
};

CHAR
yl2l1[] = {  /* Latin-2 to Latin-1 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 'A', UNK, 'L', 164, 'L', 'S', 167, 168, 'S', 'S', 'T', 'Z', 173, 'Z', 'Z',
176, 'a', UNK, 'l', 180, 'l', 's', UNK, 184, 's', 's', 't', 'z', UNK, 'z', 'z',
'R', 193, 194, 'A', 196, 'L', 'C', 199, 'C', 201, 'E', 203, 'E', 205, 'I', 'D',
208, 'N', 'N', 211, 212, 'O', 214, 215, 'R', 'U', 218, 'U', 220, 221, 'T', 's',
'r', 225, 226, 'a', 228, 'l', 'c', 231, 'c', 233, 'e', 235, 'e', 237, 'i', 'd',
240, 'n', 'n', 243, 244, 'o', 246, 247, 'r', 'u', 250, 'u', 252, 253, 't', '.'
};

CHAR
yl1l2[] = {  /* Latin-1 to Latin-2 */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 'A', UNK, 'L', 164, UNK, UNK, 167, 168, 'C', 'a', '<', '>', 173, 'R', UNK,
176, UNK, UNK, UNK, 180, UNK, UNK, UNK, 184, UNK, 'o', '>', UNK, UNK, UNK, UNK,
'A', 193, 194, 'A', 196, 'A', 'A', 199, 'E', 201, 'E', 203, 'I', 205, 'I', 'I',
208, 'N', 'O', 211, 212, 'O', 214, 215, 'O', 'U', 218, 'U', 220, 221, UNK, 223,
'a', 225, 226, 'a', 228, 'a', 'a', 231, 'e', 233, 'e', 235, 'i', 237, 'i', 'i',
240, 'n', 'o', 243, 244, 'o', 246, 247, 'o', 'u', 250, 'u', 252, 253, UNK, 'y'
};

CHAR
yl2as[] = {  /* Latin-2 to ASCII */
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK, UNK,
 32, 'A', UNK, 'L', UNK, 'L', 'S', UNK,  34, 'S', 'S', 'T', 'Z', '-', 'Z', 'Z',
UNK, 'a', UNK, 'l',  39, 'l', 's', UNK,  44, 's', 's', 't', 'z', UNK, 'z', 'z',
'R', 'A', 'A', 'A', 'A', 'L', 'C', 'C', 'C', 'E', 'E', 'E', 'E', 'I', 'I', 'D',
'D', 'N', 'N', 'O', 'O', 'O', 'O', 'x', 'R', 'U', 'U', 'U', 'U', 'Y', 'T', 's',
'r', 'a', 'a', 'a', 'a', 'l', 'c', 'c', 'c', 'e', 'e', 'e', 'e', 'i', 'i', 'd',
'd', 'n', 'n', 'o', 'o', 'o', 'o', '/', 'r', 'u', 'u', 'u', 'u', 'y', 't', '.'
};
#endif /* LATIN2 */

/* Translation functions ... */

CHAR				/* The identity translation function.  */
#ifdef CK_ANSIC
ident(CHAR c)				/* (no longer used) */
#else
ident(c) CHAR c;
#endif /* CK_ANSIC */
{ /* ident */
    return(c);				/* Instead, enter NULL in the  */
}					/* table of functions to avoid */
					/* needless function calls.    */
CHAR
#ifdef CK_ANSIC
xl1as(CHAR c) 
#else
xl1as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1as */ 			/* Latin-1 to US ASCII... */
    switch(langs[language].id) {

      case L_DUTCH:
	if (c == 255) {			/* Dutch umlaut-y */
	    zmstuff('j');		/* becomes ij */
	    return('i');
	} else return(yl1as[c]);	/* all others by the book */

      case L_GERMAN:
	switch (c) {			/* German, special rules. */
	  case 196:			/* umlaut-A -> Ae */
	    zmstuff('e');
	    return('A');
	  case 214:			/* umlaut-O -> Oe */
	    zmstuff('e');
	    return('O');
	  case 220:			/* umlaut-U -> Ue */
	    zmstuff('e');
	    return('U');
	  case 228:			/* umlaut-a -> ae */
	    zmstuff('e');
	    return('a');
	  case 246:			/* umlaut-o -> oe */
	    zmstuff('e');
	    return('o');
	  case 252:			/* umlaut-u -> ue */
	    zmstuff('e');
	    return('u');
	  case 223:			/* ess-zet -> ss */
	    zmstuff('s');
	    return('s');
	  default: return(yl1as[c]);	/* all others by the book */
	}
      case L_DANISH:
      case L_FINNISH:
      case L_NORWEGIAN:
      case L_SWEDISH:
	switch (c) {			/* Scandanavian languages. */
	  case 196:			/* umlaut-A -> Ae */
          case 198:			/* AE ligature also -> Ae */
	    zmstuff('e');
	    return('A');
	  case 214:			/* umlaut-O -> Oe */
	  case 216:			/* O-slash -> Oe */
	    zmstuff('e');
	    return('O');
	  case 220:			/* umlaut-U -> Ue */
	  /*  return('Y'); replaced by "Ue" by popular demand. */
          /*  Y for Umlaut-U is only used in German names. */
	    zmstuff('e');
	    return('U');
	  case 228:			/* umlaut-a -> ae */
          case 230:			/* ditto for ae ligature */
	    zmstuff('e');
	    return('a');
	  case 246:			/* umlaut-o -> oe */
	  case 248:			/* o-slash -> oe */
	    zmstuff('e');
	    return('o');
	  case 252:			/* umlaut-u -> ue */
	  /*  return('y'); replaced by "ue" by popular demand. */
	    zmstuff('e');
	    return('u');
	  case 197:			/* A-ring -> Aa */
	    zmstuff('a');
	    return('A');
          case 229:			/* a-ring -> aa */
	    zmstuff('a');
	    return('a');
	  default: return(yl1as[c]);	/* All others by the book */
	}
      case L_ICELANDIC:			/* Icelandic. */
	switch (c) {	
	  case 198:			/* uppercase AE -> AE */
	    zmstuff('e');
	    return('A');
	  case 208:			/* uppercase Eth -> D */
	    return('D');
	  case 214:			/* uppercase O-diaeresis -> Oe */
	    zmstuff('e');
	    return('O');
	  case 222:			/* uppercase Thorn -> Th */
	    zmstuff('h');
	    return('T');
	  case 230:			/* lowercase ae -> ae */
	    zmstuff('e');
	    return('a');
	  case 240:			/* lowercase Eth -> d */
	    return('d');
	  case 246:			/* lowercase O-diaeresis -> oe */
	    zmstuff('e');
	    return('o');
	  case 254:			/* lowercase Thorn -> th */
	    zmstuff('h');
	    return('t');
	  default: return(yl1as[c]);	/* All others by the book */
	}
      default:
	return(yl1as[c]);		/* None of the above, by the table. */
    }
}

CHAR					/* Latin-1 to German */
#ifdef CK_ANSIC
xl1ge(CHAR c) 
#else
xl1ge(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1ge */
    return(yl1ge[c]);
}

CHAR					/* German to Latin-1 */
#ifdef CK_ANSIC
xgel1(CHAR c) 
#else
xgel1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xgel1 */
    return(ygel1[c]);
}

CHAR
#ifdef CK_ANSIC
xgeas(CHAR c) 
#else
xgeas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xgeas */			/* German ISO 646 to ASCII */
    switch (c) {
      case 91:				/* umlaut-A -> Ae */
	zmstuff('e');
	return('A');
      case 92:				/* umlaut-O -> Oe */
	zmstuff('e');
	return('O');
      case 93:				/* umlaut-U -> Ue */
	zmstuff('e');
	return('U');
      case 123:				/* umlaut-a -> ae */
	zmstuff('e');
	return('a');
      case 124:				/* umlaut-o -> oe */
	zmstuff('e');
	return('o');
      case 125:				/* umlaut-u -> ue */
	zmstuff('e');
	return('u');
      case 126:				/* ess-zet -> ss */
	zmstuff('s');
	return('s');
      default:  return(c);		/* all others stay the same */
    }
}

CHAR
#ifdef CK_ANSIC
xduas(CHAR c) 
#else
xduas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xduas */			/* Dutch ISO 646 to US ASCII */
    switch (c) {
      case 64:  return(UNK);		/* 3/4 */
      case 91:				/* y-diaeresis */
	zmstuff('j');
	return('i');
      case 92:  return(UNK);		/* 1/2 */
      case 93:  return(124);		/* vertical bar */
      case 123: return(34);		/* diaeresis */
      case 124: return(UNK);		/* Florin */
      case 125: return(UNK);		/* 1/4 */
      case 126: return(39);		/* Apostrophe */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xfias(CHAR c) 
#else
xfias(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xfias */			/* Finnish ISO 646 to US ASCII */
    switch (c) {
      case 91:				/* A-diaeresis */
	zmstuff('e');
	return('A');
      case 92:				/* O-diaeresis */
	zmstuff('e');
	return('O');
      case 93:				/* A-ring */
	zmstuff('a');
	return('A');
      case 94:				/* U-diaeresis */
	/* return('Y'); */
	zmstuff('e');
	return('U');
      case 96:				/* e-acute */
	return('e');
      case 123:				/* a-diaeresis */
	zmstuff('e');
	return('a');
      case 124:				/* o-diaeresis */
	zmstuff('e');
	return('o');
      case 125:				/* a-ring */
	zmstuff('a');
	return('a');
      case 126:				/* u-diaeresis */
	/* return('y'); */
	zmstuff('e');
	return('U');
      default:
	return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xfras(CHAR c) 
#else
xfras(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xfras */			/* French ISO 646 to US ASCII */
    switch (c) {
      case 64:  return(97);		/* a grave */
      case 91:  return(UNK);		/* degree sign */
      case 92:  return(99);		/* c cedilla */
      case 93:  return(UNK);		/* paragraph sign */
      case 123: return(101);		/* e acute */
      case 124: return(117);		/* u grave */
      case 125: return(101);		/* e grave */
      case 126: return(34);		/* diaeresis */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xfcas(CHAR c) 
#else
xfcas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xfcas */			/* French Canadian ISO 646 to ASCII */
    switch (c) {
      case 64:  return('a');		/* a grave */
      case 91:  return('a');		/* a circumflex */
      case 92:  return('c');		/* c cedilla */
      case 93:  return('e');		/* e circumflex */
      case 94:  return('i');		/* i circumflex */
      case 96:  return('o');		/* o circumflex */
      case 123: return('e');		/* e acute */
      case 124: return('u');		/* u grave */
      case 125: return('e');		/* e grave */
      case 126: return('u');		/* u circumflex */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xitas(CHAR c) 
#else
xitas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xitas */			/* Italian ISO 646 to ASCII */
    switch (c) {
      case 91:  return(UNK);		/* degree */
      case 92:  return('c');		/* c cedilla */
      case 93:  return('e');		/* e acute */
      case 96:  return('u');		/* u grave */
      case 123: return('a');		/* a grave */
      case 124: return('o');		/* o grave */
      case 125: return('e');		/* e grave */
      case 126: return('i');		/* i grave */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xneas(CHAR c) 
#else
xneas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xneas */			/* NeXT to ASCII */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 234) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 250) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    c = xnel1(c);			/* Convert to Latin-1 */
    return(yl1as[c]);			/* Convert Latin-1 to ASCII */
}

CHAR
#ifdef CK_ANSIC
xnoas(CHAR c) 
#else
xnoas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xnoas */			/* Norge/Danish ISO 646 to ASCII */
    switch (c) {
      case 91:
	zmstuff('E');			/* AE digraph */
	return('A');
      case 92: return('O');		/* O slash */
      case 93:				/* A ring */
	zmstuff('a');
	return('A');
      case 123:				/* ae digraph */
	zmstuff('e');
	return('a');
      case 124: return('o');		/* o slash */
      case 125:				/* a ring */
	zmstuff('a');
	return('a');
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xpoas(CHAR c) 
#else
xpoas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xpoas */			/* Portuguese ISO 646 to ASCII */
    switch (c) {
      case 91:  return('A');		/* A tilde */
      case 92:  return('C');		/* C cedilla */
      case 93:  return('O');		/* O tilde */
      case 123: return('a');		/* a tilde */
      case 124: return('c');		/* c cedilla */
      case 125: return('o');		/* o tilde */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xspas(CHAR c) 
#else
xspas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xspas */			/* Spanish ISO 646 to ASCII */
    switch (c) {
      case 91:  return(33);		/* Inverted exclamation */
      case 92:  return('N');		/* N tilde */
      case 93:  return(63);		/* Inverted question mark */
      case 123: return(UNK);		/* degree */
      case 124: return('n');		/* n tilde */
      case 125: return('c');		/* c cedilla */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xswas(CHAR c) 
#else
xswas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xswas */			/* Swedish ISO 646 to ASCII */
    switch (c) {
      case 64:  return('E');		/* E acute */
      case 91:				/* A diaeresis */
	zmstuff('e');
	return('A');
      case 92:				/* O diaeresis */
	zmstuff('e');
	return('O');
      case 93:				/* A ring */
	zmstuff('a');
	return('A');
      case 94:				/* U diaeresis */
	/* return('Y'); */
	zmstuff('e');
	return('U');
      case 96:  return('e');		/* e acute */
      case 123:				/* a diaeresis */
	zmstuff('e');
	return('a');
      case 124:				/* o diaeresis */
	zmstuff('e');
	return('o');
      case 125:				/* a ring */
	zmstuff('a');
	return('a');
      case 126:				/* u diaeresis */
	/* return('y'); */
	zmstuff('e');
	return('u');
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xchas(CHAR c) 
#else
xchas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xchas */			/* Swiss ISO 646 to ASCII */
    switch (c) {
      case 35:  return('u');		/* u grave */
      case 64:  return('a');		/* a grave */
      case 91:  return('e');		/* e acute */
      case 92:  return('c');		/* c cedilla */
      case 93:  return('e');		/* e circumflex */
      case 94:  return('i');		/* i circumflex */
      case 95:  return('e');		/* e grave */
      case 96:  return('o');		/* o circumflex */
      case 123:				/* a diaeresis */
	zmstuff('e');
	return('a');
      case 124:				/* o diaeresis */
	zmstuff('e');
	return('o');
      case 125:				/* u diaeresis */
	zmstuff('e');
	return('u');
      case 126: return('u');		/* u circumflex */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xhuas(CHAR c) 
#else
xhuas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xhuas */			/* Hungarian ISO 646 to ASCII */
    switch (c) {
      case 64:  return('A');		/* A acute */
      case 91:  return('E');		/* E acute */
      case 92:  return('O');		/* O diaeresis */
      case 93:  return('U');		/* U diaeresis */
      case 96:  return('a');		/* a acute */
      case 123: return('e');		/* e acute */
      case 124: return('o');		/* o acute */
      case 125: return('u');		/* u acute */
      case 126: return(34);		/* double acute accent */
      default:  return(c);
    }
}

CHAR
#ifdef CK_ANSIC
xdmas(CHAR c) 
#else
xdmas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xdmas */			/* DEC MCS to ASCII */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 215) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 247) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    return(yl1as[c]);			/* Otherwise treat like Latin-1 */
}

CHAR
#ifdef CK_ANSIC
xdgas(CHAR c)
#else
xdgas(c) CHAR c;
#endif /* CK_ANSIC */
{ /*  xdgas */			/* Data General to ASCII */
    switch(c) {
      case 180: return('f');		/* Florin */
      case 183: return('<');		/* Less-equal */
      case 184: return('>');		/* Greater-equal */
      case 186: return(96);		/* Grave accent */
      case 191: return('^');		/* Uparrow */
      case 215:
	if (langs[language].id == L_FRENCH) { /* OE digraph */
	    zmstuff('E');
	    return('O');
	} else return('O');
      case 247:
	if (langs[language].id == L_FRENCH) { /* oe digraph */
	    zmstuff('e');
	    return('o');
	} else return('o');
      case 175: case 179: case 220: case 222:
      case 223: case 254: case 255:
	return(UNK);
      default:				/* The rest, convert to Latin-1 */
	return(yl1as[ydgl1[c]]);	/* and from there to ASCII */
    }
}

CHAR
#ifdef CK_ANSIC
xukl1(CHAR c) 
#else
xukl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xukl1 */			/* UK ASCII to Latin-1 */
    if (c == 35)
      return(163);
    else return(c);
}

CHAR
#ifdef CK_ANSIC
xl1uk(CHAR c) 
#else
xl1uk(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1uk */			/* Latin-1 to UK ASCII */
    if (c == 163)
      return(35);
    else return(yl1as[c]);
}

CHAR					/* Latin-1 to French ISO 646 */
#ifdef CK_ANSIC
xl1fr(CHAR c) 
#else
xl1fr(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1fr */
    return(yl1fr[c]);
}


CHAR					/* French ASCII to Latin-1 */
#ifdef CK_ANSIC
xfrl1(CHAR c) 
#else
xfrl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xfrl1 */
    return(yfrl1[c]);
}

CHAR					/* Latin-1 to Dutch ASCII */
#ifdef CK_ANSIC
xl1du(CHAR c) 
#else
xl1du(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1du */
    return(yl1du[c]);
}

CHAR
#ifdef CK_ANSIC
xdul1(CHAR c) 
#else
xdul1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xdul1 */			/* Dutch ISO 646 to Latin-1 */
    return(ydul1[c]);
}

CHAR
#ifdef CK_ANSIC
xfil1(CHAR c) 
#else
xfil1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xfil1 */			/* Finnish ISO 646 to Latin-1 */
    return(yfil1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1fi(CHAR c) 
#else
xl1fi(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1fi */			/* Latin-1 to Finnish ISO 646 */
    return(yl1fi[c]); 
}

CHAR
#ifdef CK_ANSIC
xfcl1(CHAR c) 
#else
xfcl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xfcl1 */			/* French Canadian ISO646 to Latin-1 */
    return(yfcl1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1fc(CHAR c) 
#else
xl1fc(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1fc */			/* Latin-1 to French Canadian ISO646 */
    return(yl1fc[c]); 
}

CHAR
#ifdef CK_ANSIC
xitl1(CHAR c) 
#else
xitl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xitl1 */			/* Italian ISO 646 to Latin-1 */
    return(yitl1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1it(CHAR c) 
#else
xl1it(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1it */			/* Latin-1 to Italian ISO 646 */
    return(yl1it[c]); 
}

CHAR
#ifdef CK_ANSIC
xnel1(CHAR c) 
#else
xnel1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xnel1 */		 /* NeXT to Latin-1 */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 234) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 250) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    return(ynel1[c]);
}

CHAR
#ifdef CK_ANSIC
xl1ne(CHAR c) 
#else
xl1ne(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1ne */		 /* Latin-1 to NeXT */
    return(yl1ne[c]);
}

CHAR
#ifdef CK_ANSIC
xnol1(CHAR c) 
#else
xnol1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xnol1 */		 /* Norwegian and Danish ISO 646 to Latin-1 */
    return(ynol1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1no(CHAR c) 
#else
xl1no(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1no */		 /* Latin-1 to Norwegian and Danish ISO 646 */
    return(yl1no[c]); 
}

CHAR
#ifdef CK_ANSIC
xpol1(CHAR c) 
#else
xpol1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xpol1 */			/* Portuguese ISO 646 to Latin-1 */
    return(ypol1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1po(CHAR c) 
#else
xl1po(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1po */			/* Latin-1 to Portuguese ISO 646 */
    return(yl1po[c]); 
}

CHAR
#ifdef CK_ANSIC
xspl1(CHAR c) 
#else
xspl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xspl1 */			/* Spanish ISO 646 to Latin-1 */
    return(yspl1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1sp(CHAR c) 
#else
xl1sp(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1sp */			/* Latin-1 to Spanish ISO 646 */
    return(yl1sp[c]); 
}

CHAR
#ifdef CK_ANSIC
xswl1(CHAR c) 
#else
xswl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xswl1 */			/* Swedish ISO 646 to Latin-1 */
    return(yswl1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1sw(CHAR c) 
#else
xl1sw(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1sw */			/* Latin-1 to Swedish ISO 646 */
    return(yl1sw[c]); 
}

CHAR
#ifdef CK_ANSIC
xchl1(CHAR c) 
#else
xchl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xchl1 */			/* Swiss ISO 646 to Latin-1 */
    return(ychl1[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1ch(CHAR c) 
#else
xl1ch(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1ch */			/* Latin-1 to Swiss ISO 646 */
    return(yl1ch[c]); 
}

CHAR
#ifdef CK_ANSIC
xhul1(CHAR c) 
#else
xhul1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xhul1 */			/* Hungarian ISO 646 to Latin-1 */
    return(yhul1[c]);
}

CHAR
#ifdef CK_ANSIC
xl1hu(CHAR c) 
#else
xl1hu(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1hu */			/* Latin-1 to Hungarian ISO 646 */
    return(yl1hu[c]);
}

CHAR
#ifdef CK_ANSIC
xl1dm(CHAR c) 
#else
xl1dm(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1dm */ /* Latin-1 to DEC Multinational Character Set (MCS) */
    return(yl1dm[c]); 
}

CHAR
#ifdef CK_ANSIC
xl1dg(CHAR c) 
#else
xl1dg(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1dg */ /* Latin-1 to DG International Character Set (MCS) */
    return(yl1dg[c]); 
}

CHAR
#ifdef CK_ANSIC
xdml1(CHAR c) 
#else
xdml1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xdml1 */ /* DEC Multinational Character Set (MCS) to Latin-1 */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 215) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 247) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    return(ydml1[c]); 
}

CHAR
#ifdef CK_ANSIC
xdgl1(CHAR c) 
#else
xdgl1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xdgl1 */ /* DG International Character Set (MCS) to Latin-1 */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 215) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 247) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    return(ydgl1[c]); 
}

/* Translation functions for receiving files and translating them into ASCII */

CHAR
#ifdef CK_ANSIC
zl1as(CHAR c) 
#else
zl1as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* zl1as */
    switch(langs[language].id) {

      case L_DUTCH:
	if (c == 255) {			/* Dutch umlaut-y */
	    zdstuff('j');		/* becomes ij */
	    return('i');
	} else return(yl1as[c]);	/* all others by the book */

      case L_GERMAN:
	switch (c) {			/* German, special rules. */
	  case 196:			/* umlaut-A -> Ae */
	    zdstuff('e');
	    return('A');
	  case 214:			/* umlaut-O -> Oe */
	    zdstuff('e');
	    return('O');
	  case 220:			/* umlaut-U -> Ue */
	    zdstuff('e');
	    return('U');
	  case 228:			/* umlaut-a -> ae */
	    zdstuff('e');
	    return('a');
	  case 246:			/* umlaut-o -> oe */
	    zdstuff('e');
	    return('o');
	  case 252:			/* umlaut-u -> ue */
	    zdstuff('e');
	    return('u');
	  case 223:			/* ess-zet -> ss */
	    zdstuff('s');
	    return('s');
	  default: return(yl1as[c]);	/* all others by the book */
	}
      case L_DANISH:
      case L_FINNISH:
      case L_NORWEGIAN:
      case L_SWEDISH:
	switch (c) {			/* Scandanavian languages. */
	  case 196:			/* umlaut-A -> Ae */
	    zdstuff('e');
	    return('A');
	  case 214:			/* umlaut-O -> Oe */
	  case 216:			/* O-slash -> Oe */
	    zdstuff('e');
	    return('O');
	  case 220:			/* umlaut-U -> Y */
	    /* return('Y'); */
	    zdstuff('e');
	    return('U');
	  case 228:			/* umlaut-a -> ae */
	    zdstuff('e');
	    return('a');
	  case 246:			/* umlaut-o -> oe */
	  case 248:			/* o-slash -> oe */
	    zdstuff('e');
	    return('o');
	  case 252:			/* umlaut-u -> y */
	    /* return('y'); */
	    zdstuff('e');
	    return('u');
	  case 197:			/* A-ring -> Aa */
	    zdstuff('a');
	    return('A');
          case 229:			/* a-ring -> aa */
	    zdstuff('a');
	    return('a');
	  default: return(yl1as[c]);	/* All others by the book */
	}
      default:
	return(yl1as[c]);		/* Not German, by the table. */
    }
}

CHAR					/* IBM CP437 to Latin-1 */
#ifdef CK_ANSIC
x43l1(CHAR c) 
#else
x43l1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* x43l1 */
    return(y43l1[c]);
}

CHAR					/* IBM CP850 to Latin-1 */
#ifdef CK_ANSIC
x85l1(CHAR c) 
#else
x85l1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* x85l1 */
    return(y85l1[c]);
}

CHAR					/* Latin-1 to IBM CP437 */
#ifdef CK_ANSIC
xl143(CHAR c) 
#else
xl143(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl143 */
    return(yl143[c]);
}

CHAR					/* Latin-1 to CP850 */
#ifdef CK_ANSIC
xl185(CHAR c) 
#else
xl185(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl185 */
    return(yl185[c]);
}

CHAR
#ifdef CK_ANSIC
x43as(CHAR c) 
#else
x43as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* x43as */				/* CP437 to ASCII */
    c = y43l1[c];			/* Translate to Latin-1 */
    return(xl143(c));			/* and from Latin-1 to ASCII. */
}

CHAR
#ifdef CK_ANSIC
x85as(CHAR c) 
#else
x85as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* x85as */				/* CP850 to ASCII */
    c = y85l1[c];			/* Translate to Latin-1 */
    return(xl1as(c));			/* and from Latin-1 to ASCII. */
}

CHAR					/* Macintosh Latin to Latin-1 */
#ifdef CK_ANSIC
xaql1(CHAR c) 
#else
xaql1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xaql1 */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 206) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 207) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    return(yaql1[c]);
}

CHAR					/* Macintosh Latin to ASCII */
#ifdef CK_ANSIC
xaqas(CHAR c) 
#else
xaqas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xaqas */
    if (langs[language].id == L_FRENCH) { /* If SET LANGUAGE FRENCH */
	if (c == 206) {			/* handle OE digraph. */
	    zmstuff('E');
	    return('O');
	} else if (c == 207) {		/* Also lowercase oe. */
	    zmstuff('e');
	    return('o');
	}
    }
    c = yaql1[c];			/* Translate to Latin-1 */
    return(xl1as(c));			/* then to ASCII. */
}

CHAR					/* Latin-1 to Macintosh Latin */
#ifdef CK_ANSIC
xl1aq(CHAR c) 
#else
xl1aq(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1aq */
    return(yl1aq[c]);
}

#ifdef LATIN2

/* Translation functions for Latin Alphabet 2 */

CHAR					/* Latin-2 to Latin-1 */
#ifdef CK_ANSIC
xl2l1(CHAR c) 
#else
xl2l1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2l1 */
    return(yl2l1[c]);
}

CHAR					/* Latin-1 to Latin-2 */
#ifdef CK_ANSIC
xl1l2(CHAR c) 
#else
xl1l2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll1l2 */
    return(yl1l2[c]);
}

CHAR					/* Latin-2 to ASCII */
#ifdef CK_ANSIC
xl2as(CHAR c) 
#else
xl2as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2as */
    return(yl2as[c]);
}

CHAR					/* Latin-2 to CP852 */
#ifdef CK_ANSIC
xl252(CHAR c) 
#else
xl252(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll252 */
    return(yl252[c]);
}

CHAR					/* CP852 to Latin-2 */
#ifdef CK_ANSIC
x52l2(CHAR c) 
#else
x52l2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* x52l2 */
    return(y52l2[c]);
}

CHAR					/* CP852 to ASCII */
#ifdef CK_ANSIC
x52as(CHAR c) 
#else
x52as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl52as */
    return(yl2as[y52l2[c]]);		/* CP852 -> Latin-2 -> ASCII */
}

CHAR					/* CP852 to Latin-1 */
#ifdef CK_ANSIC
x52l1(CHAR c) 
#else
x52l1(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl52l1 */
    return(yl2l1[y52l2[c]]);		/* CP852 -> Latin-2 -> Latin-1 */
}

CHAR					/* Latin-1 to CP852 */
#ifdef CK_ANSIC
xl152(CHAR c) 
#else
xl152(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll152 */
    return(yl252[yl1l2[c]]);		/* Latin-1 -> Latin-2 -> CP852 */
}

CHAR					/* Latin-2 to NeXT */
#ifdef CK_ANSIC
xl2ne(CHAR c) 
#else
xl2ne(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2ne */
    switch(c) {
      case 162: return(198);		/* Breve */
      case 163: return(232);		/* L with stroke */
      case 178: return(206);		/* Ogonek */
      case 179: return(248);		/* l with stroke */
      case 183: return(207);		/* Caron */
      case 189: return(205);		/* Double acute */
      case 208: return(144);		/* D stroke = Eth */
      case 240: return(230);		/* d stroke = eth */
      case 255: return(199);		/* Dot above */
      default:  return(yl1ne[yl2l1[c]]);
    }
}

CHAR					/* Latin-2 to CP437 */
#ifdef CK_ANSIC
xl243(CHAR c) 
#else
xl243(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll243 */
    return(yl1l2[y43l1[c]]);
}

CHAR					/* Latin-2 to CP850 */
#ifdef CK_ANSIC
xl285(CHAR c) 
#else
xl285(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll285 */
    return(yl1l2[y85l1[c]]);
}

CHAR					/* Latin-2 to Apple */
#ifdef CK_ANSIC
xl2aq(CHAR c) 
#else
xl2aq(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl2aq */
    return(yl1aq[yl2l1[c]]);		/* Could do more... */
}

CHAR					/* Latin-2 to DGI */
#ifdef CK_ANSIC
xl2dg(CHAR c) 
#else
xl2dg(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2dg */
    return(yl1l2[ydgl1[c]]);
}

CHAR					/* Latin-2 to Short KOI */
#ifdef CK_ANSIC
xl2sk(CHAR c) 
#else
xl2sk(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2sk */
    return(islower(c) ? toupper(c) : c);
}

CHAR					/* NeXT to Latin-2 */
#ifdef CK_ANSIC
xnel2(CHAR c) 
#else
xnel2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xnel2 */
    switch (c) {
      case 144: return(208);		/* D stroke = Eth */
      case 198: return(162);		/* Breve */
      case 199: return(255);		/* Dot above */
      case 205: return(189);		/* Double acute */
      case 206: return(178);		/* Ogonek */
      case 207: return(183);		/* Caron */
      case 230: return(240);		/* d stroke = eth */
      case 232: return(163);		/* L with stroke */
      case 248: return(179);		/* l with stroke */
      default:  return(yl1l2[ynel1[c]]); /* Others, go thru Latin-1 */
    }
}

CHAR					/* CP437 to Latin-2 */
#ifdef CK_ANSIC
x43l2(CHAR c) 
#else
x43l2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl43l2 */
    return(yl1l2[y43l1[c]]);
}

CHAR					/* CP850 to Latin-2 */
#ifdef CK_ANSIC
x85l2(CHAR c) 
#else
x85l2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl85l2 */
    return(yl1l2[y85l1[c]]);
}

CHAR					/* Apple to Latin-2 */
#ifdef CK_ANSIC
xaql2(CHAR c) 
#else
xaql2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlaql2 */
    switch (c) {
      case 249: return(162);		/* Breve accent */
      case 250: return(255);		/* Dot accent */
      case 253: return(189);		/* Double acute */
      default: return(yl1l2[yaql1[c]]);
    }
}

CHAR					/* DGI to Latin-2 */
#ifdef CK_ANSIC
xdgl2(CHAR c) 
#else
xdgl2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xldgl2 */
    return(yl1l2[ydgl1[c]]);		/* (for now) */
}

CHAR					/* Short KOI to Latin-2 */
#ifdef CK_ANSIC
xskl2(CHAR c) 
#else
xskl2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlskl2 */
    return(islower(c) ? toupper(c) : c);
}

CHAR					/* Latin-2 to German */
#ifdef CK_ANSIC
xl2ge(CHAR c) 
#else
xl2ge(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2ge */
    switch(c) {
      case 167: return(64);		/* Paragraph sign */
      case 196: return(91);		/* A-diaeresis */
      case 214: return(92);		/* O-diaeresis */
      case 220: return(93);		/* U-diaeresis */
      case 223: return(126);		/* double-s */
      case 228: return(123);		/* a-diaeresis */
      case 246: return(124);		/* o-diaeresis */
      case 252: return(125);		/* u-diaeresis */
      default:  return(yl2as[c]);	/* Others */
    }
}

CHAR					/* German to Latin-2 */
#ifdef CK_ANSIC
xgel2(CHAR c) 
#else
xgel2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlgel2 */
    switch(c) {
      case 64:  return(167);		/* Paragraph sign */
      case 91:  return(196);		/* A-diaeresis */
      case 92:  return(214);		/* O-diaeresis */
      case 93:  return(220);		/* U-diaeresis */
      case 123: return(228);		/* a-diaeresis */
      case 126: return(223);		/* double-s */
      case 124: return(246);		/* o-diaeresis */
      case 125: return(252);		/* u-diaeresis */
      default:  return(c);		/* Others */
    }
}

CHAR					/* Latin-2 to Hungarian */
#ifdef CK_ANSIC
xl2hu(CHAR c) 
#else
xl2hu(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xll2hu */
    switch(c) {
      case 164: return(36);		/* Currency symbol */
      case 189: return(126);		/* Double acute accent */
      case 193: return(64);		/* A-acute */
      case 201: return(91);		/* E-acute */
      case 214: return(92);		/* O-diaeresis */
      case 220: return(93);		/* U-diaeresis */
      case 225: return(96);		/* a-acute */
      case 233: return(123);		/* e-acute */
      case 246: return(124);		/* o-diaeresis */
      case 252: return(125);		/* u-diaeresis */
      default:  return(yl2as[c]);	/* Others */
    }
}

CHAR					/* Hungarian to Latin-2 */
#ifdef CK_ANSIC
xhul2(CHAR c) 
#else
xhul2(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlhul2 */
    switch(c) {
      case 36:  return(164);		/* Currency symbol */
      case 64:  return(193);		/* A-acute */
      case 91:  return(201);		/* E-acute */
      case 92:  return(214);		/* O-diaeresis */
      case 93:  return(220);		/* U-diaeresis */
      case 96:  return(225);		/* a-acute */
      case 123: return(233);		/* e-acute */
      case 124: return(246);		/* o-diaeresis */
      case 125: return(252);		/* u-diaeresis */
      case 126: return(189);		/* Double acute accent */
      default:  return(c);		/* Others */
    }
}
#else /* NOLATIN2 */
#define xl2l1 NULL
#define xl1l2 NULL
#define xl2as NULL
#define xl252 NULL
#define x52l2 NULL
#define x52as NULL
#define x52l1 NULL
#define xl152 NULL
#define xl2ne NULL
#define xl243 NULL
#define xl285 NULL
#define xl2aq NULL
#define xl2dg NULL
#define xl2sk NULL
#define xnel2 NULL
#define x43l2 NULL
#define x85l2 NULL
#define xaql2 NULL
#define xdgl2 NULL
#define xskl2 NULL
#define xl2ge NULL
#define xgel2 NULL
#define xl2hu NULL
#define xhul2 NULL
#endif /* LATIN2 */

#ifdef CYRILLIC
/* Translation functions for Cyrillic character sets */

CHAR					/* Latin/Cyrillic to */
#ifdef CK_ANSIC
xlcac(CHAR c) 
#else
xlcac(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlcac */			/* Microsoft Code Page 866 */
    return(ylcac[c]);
}

CHAR					/* Latin/Cyrillic to Old KOI-8 */
#ifdef CK_ANSIC
xlck8(CHAR c) 
#else
xlck8(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlck8 */
    return(ylck8[c]);
}

CHAR
#ifdef CK_ANSIC
xlcsk(CHAR c) 
#else
xlcsk(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlcsk */			/* Latin/Cyrillic to Short KOI */
    return(ylcsk[c]);
}

CHAR
#ifdef CK_ANSIC
xlcas(CHAR c) 
#else
xlcas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xlcas */			/* Latin/Cyrillic to ASCII */
    if (langs[language].id == L_RUSSIAN)
      return(ylcsk[c]);
    else
      return((c > 127) ? '?' : c);
}

CHAR					/* CP866 */
#ifdef CK_ANSIC
xaclc(CHAR c) 
#else
xaclc(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xaclc */			/* to Latin/Cyrillic */
    return(yaclc[c]);
}

CHAR					/* Old KOI-8 to Latin/Cyrillic */
#ifdef CK_ANSIC
xk8lc(CHAR c) 
#else
xk8lc(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xk8lc */
    return(yk8lc[c]);
}

CHAR
#ifdef CK_ANSIC
xskcy(CHAR c) 
#else
xskcy(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xskcy */			/* Short KOI to Latin/Cyrillic */
    return(yskcy[c & 0x7f]);
}

CHAR
#ifdef CK_ANSIC
xascy(CHAR c) 
#else
xascy(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xascy */			/* ASCII to Latin/Cyrillic */
    if (langs[language].id == L_RUSSIAN) { /* If LANGUAGE == RUSSIAN  */
	return(yskcy[c & 0x7f]);	/* treat ASCII as Short KOI */
    } else return((c > 127) ? '?' : c);
}

CHAR
#ifdef CK_ANSIC
xacas(CHAR c) 
#else
xacas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xacas */			/* CP866 to ASCII */
    if (langs[language].id == L_RUSSIAN) {
	c = yaclc[c];			/* First to Latin/Cyrillic */
	return(ylcsk[c]);		/* Then to Short KOI */
    } else return((c > 127) ? '?' : c);
}

CHAR
#ifdef CK_ANSIC
xskas(CHAR c) 
#else
xskas(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xskas */			/* Short KOI to ASCII */
    return((c > 95) ? '?' : c);
}

CHAR
#ifdef CK_ANSIC
xk8as(CHAR c) 
#else
xk8as(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xk8as */			/* Old KOI-8 Cyrillic to ASCII */
    if (langs[language].id == L_RUSSIAN) {
	c = yk8lc[c];			/* First to Latin/Cyrillic */
	return(ylcsk[c]);		/* Then to Short KOI */
    } else return((c > 127) ? '?' : c);
}

CHAR
#ifdef CK_ANSIC
xassk(CHAR c) 
#else
xassk(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xassk */			/* ASCII to Short KOI */
    c &= 0x77;				/* Force it to be ASCII */
    return((c > 95) ? (c - 32) : c);	/* Fold columns 6-7 to 4-5 */
}

CHAR
#ifdef CK_ANSIC
xl1sk(CHAR c) 
#else
xl1sk(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xl1sk */			/* Latin-1 to Short KOI */
    c = zl1as(c);			/* Convert to ASCII */
    return(c = xassk(c));		/* Convert ASCII to Short KOI */
}

CHAR
#ifdef CK_ANSIC
xaslc(CHAR c) 
#else
xaslc(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xaslc */			/* ASCII to Latin/Cyrillic */
    if (langs[language].id == L_RUSSIAN)
      return(yskcy[c & 0x7f]);
    else return(c & 0x7f);
}

CHAR
#ifdef CK_ANSIC
xasac(CHAR c) 
#else
xasac(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xasac */			/* ASCII to CP866 */
    if (langs[language].id == L_RUSSIAN) { /* Use Short KOI */
	c = xskcy(c);			/* Translate to Latin/Cyrillic */
	return(ylcac[c]);		/* Then to CP866 */
    } else return(c & 0x7f);
}

CHAR
#ifdef CK_ANSIC
xask8(CHAR c) 
#else
xask8(c) CHAR c; 
#endif /* CK_ANSIC */
{ /* xask8 */			/* ASCII to KOI-8 */
    if (langs[language].id == L_RUSSIAN) { /* Use Short KOI */
	c = xskcy(c);			/* Translate to Latin/Cyrillic */
	return(ylck8[c]);		/* Then to KOI-8 */
    } else return(c & 0x7f);
}
#else
#define xacas NULL
#define xaclc NULL
#define xasac NULL
#define xascy NULL
#define xask8 NULL
#define xaslc NULL
#define xassk NULL
#define xk8as NULL
#define xk8lc NULL
#define xl1sk NULL
#define xlcac NULL
#define xlcas NULL
#define xlck8 NULL
#define xlcsk NULL
#define xskas NULL
#define xskcy NULL
#endif /* CYRILLIC */

/* Translation functions for Japanese Kanji character sets */

#ifdef KANJI
/*
  Translate Kanji Transfer Character Set (EUC) to local file character set,
  contributed by Dr. Hirofumi Fujii, Japan High Energy Research Laboratory
  (KEK), Tokyo, Japan.

  a is a byte to be translated, which may be a single-byte character,
  the Katakana prefix, the first byte of a two-byte Kanji character, or the
  second byte of 2-byte Kanji character.

  fn is the output function.

  Returns 0 on success, -1 on failure.
*/

_PROTOTYP(static int jpnxas, (int, int[]) );
_PROTOTYP(static int jpnxkt, (int, int[]) );
_PROTOTYP(static int jpnxkn, (int[], int[]) );

static int jpncnt;       /* byte count for Japanese */
static int jpnlst;       /* last status (for JIS7) */

static int
jpnxas(a, obuf) int a; int obuf[]; { /* Translate ASCII to local file code */
    int r;

    r = 0;
    if (fcharset == FC_JIS7) {
	switch (jpnlst) {
	  case 1:
	    obuf[0] = 0x0f;
	    obuf[1] = a;
	    r = 2;
	    break;
	  case 2:
	    obuf[0] = 0x1b;
	    obuf[1] = 0x28;
	    obuf[2] = 0x4a;
	    obuf[3] = a;
	    r = 4;
	    break;
	  default:
	    obuf[0] = a;
	    r = 1;
	    break;
	}
    } else {
	obuf[0] = a;
	r = 1;
    }
    return(r);
}

static int
jpnxkt(a, obuf) int a; int obuf[]; { 
/* Translate JIS X 201 Katakana to local code */

    int r;
  
    r = 0;
    if (fcharset == FC_JIS7) {
	switch (jpnlst) {
	  case 2:				/* from Kanji */
	    obuf[r++] = 0x1b;
	    obuf[r++] = 0x28;
	    obuf[r++] = 0x4a;
	  case 0:				/* from Roman */
	    obuf[r++] = 0x0e;
	  default:
	    obuf[r++] = (a & 0x7f);
	  break;
	}
    } else {
	if (fcharset == FC_JEUC)
	  obuf[r++] = 0x8e;
	obuf[r++] = (a | 0x80);
    }
    return(r);
}

static int
jpnxkn(ibuf, obuf) int ibuf[], obuf[]; {
    /* Translate JIS X 0208 Kanji to local code */
    int c1, c2;
    int r;

    c1 = ibuf[0] & 0x7f;
    c2 = ibuf[1] & 0x7f;

    if (fcharset == FC_SHJIS) {
	if (c1 & 1)
	  c2 += 0x1f;
	else
	  c2 += 0x7d;

        if (c2 >= 0x7f) c2++;

        c1 = ((c1 - 0x21) >> 1) + 0x81;
        if (c1 > 0x9f) c1 += 0x40;

        obuf[0] = c1;
        obuf[1] = c2;
        r = 2;
    } else if (fcharset == FC_JIS7) {
        r = 0;
        switch (jpnlst) {
  	  case 1:
	    obuf[r++] = 0x0f; /* From Katakana */
  	  case 0:
	    obuf[r++] = 0x1b;
	    obuf[r++] = 0x24;
	    obuf[r++] = 0x42;
	  default:
	    obuf[r++] = c1;
	    obuf[r++] = c2;
	    break;
	}
    } else {
        obuf[0] = (c1 | 0x80);
        obuf[1] = (c2 | 0x80);
        r = 2;
    }
    return(r);
}

int
xkanjf() {
/* Initialize parameters for xkanji */
/* This function should be called when F/X-packet is received */
    jpncnt = jpnlst = 0;
    return(0);
}

int
#ifdef CK_ANSIC
xkanjz( int (*fn)(char) )
#else
xkanjz( fn ) int (*fn)();
#endif /* CK_ANSIC */
{ /* xkanjz */
/*
  Terminate xkanji
  This function must be called when Z-packet is received
  (before closing the file).
*/
    static int obuf[6];
    int r, i, c;
  
    if (fcharset == FC_JIS7) {
        c = 'A';			/* Dummy Roman character */
        r = jpnxas(c, obuf) - 1;	/* -1 removes Dummy character */
        if (r > 0) {
	    for (i = 0; i < r; i++)
	      if ( ((*fn)((char) obuf[i])) < 0 )
		return( -1 );
	}
    }
    return( 0 );
}

int
#ifdef CK_ANSIC
xkanji(int a, int (*fn)(char))
#else
xkanji(a, fn) int a; int (*fn)();
#endif /* CK_ANSIC */
{ /* xkanji */
    static int xbuf[2];
    static int obuf[8];

    int i, r;
    int c7;
    int state;

    r = 0;
    if (jpncnt == 0) {
	/* 1st byte */
	if ( (a & 0x80) == 0 ) {
	    /* 8th bit is 0, i.e., single-byte code */
	    r = jpnxas(a, obuf);
	    state = 0;
	} else {
	    /* 8th bit is 1, check the range */
	    c7 = a & 0x7f;
	    if ( ((c7 > 0x20) && (c7 < 0x7f)) || (c7 == 0x0e) ) {
	        /* double byte code */
	        xbuf[jpncnt++] = a;
	    } else {
	        /* single byte code */
	        r = jpnxas(a, obuf);
	        state = 0;
	    }
	}
    } else {
	/* not the 1st byte */
	xbuf[jpncnt++] = a;
	if (xbuf[0] == 0x8e) {
	    r = jpnxkt( xbuf[1], obuf );
	    state = 1;
	} else {
	    r = jpnxkn(xbuf, obuf);
	    state = 2;
	}
    }
    if (r > 0) {
        for (i = 0; i < r; i++ )
	  if ( ((*fn)((char) obuf[i])) < 0 )
	    return( -1 );
        jpnlst = state;
        jpncnt = 0;
    }
    return( 0 );
}

/*
  Function for translating from Japanese file character set
  to Japanese EUC transfer character set.
  Returns a pointer to a string containing 0, 1, or 2 bytes.
*/

/* zkanji */
static int jpnstz;			/* status for JIS-7 */
static int jpnpnd;			/* number of pending bytes */
static int jpnpnt;			/* pending buffer index */
static int jpnpbf[8];			/* pending buffer */

int
zkanjf() {				/* Initialize */
    jpnstz = jpnpnd = jpnpnt = 0;
    return(0);
}

int
zkanjz() {
    return( 0 );
}

int
#ifdef CK_ANSIC
zkanji( int (*fn)(void) )
#else
zkanji( fn ) int (*fn)();
#endif /* CK_ANSIC */
{ /* zkanji */
    /* Read Japanese local code and translate to Japanese EUC */
    int a;
    int sc[3];
    
    /* No pending characters */
    if (fcharset == FC_SHJIS) {		/* Translating from Shift-JIS */
        if (jpnpnd) {
            jpnpnd--;
            return( jpnpbf[jpnpnt++] );
        }
        
        a = (*fn)();
	jpnpnd = jpnpnt = 0;
	if (((a >= 0x81) && (a <= 0x9f)) ||
	    ((a >= 0xe0) && (a <= 0xfc))) { /* 2-byte Kanji code */
	    sc[0] = a;
	    if ((sc[1] = (*fn)()) < 0)	/* Get second byte */
	      return( sc[1] );
	    if (sc[0] <= 0x9f)
	      sc[0] -= 0x71;
	    else
	      sc[0] -= 0xb1;
	    sc[0] = sc[0] * 2 + 1;
	    if (sc[1] > 0x7f)
	      sc[1]--;
	    if (sc[1] >= 0x9e) {
	        sc[1] -= 0x7d;
	        sc[0]++;
	    } else {
	        sc[1] -= 0x1f;
	    }
	    a = (sc[0] | 0x80);
	    jpnpbf[0] = (sc[1] | 0x80);
	    jpnpnd = 1;
	    jpnpnt = 0;
	} else if ((a >= 0xa1) && (a <= 0xdf)) { /* Katakana */
	    jpnpbf[0] = a;
	    jpnpnd = 1;
	    jpnpnt = 0;
	    a = 0x8e;
	}
	return(a);
    } else if (fcharset == FC_JIS7 ) {	/* 7-bit JIS X 0208 */
        if (jpnpnd) {
            a = jpnpbf[jpnpnt++];
	    jpnpnd--;
            return(a);
        }
        jpnpnt = 0;
        if ((a = (*fn)()) < 0)
	  return(a);
        while (jpnpnd == 0) {
            if ((a > 0x20) && (a < 0x7f)) {
                switch (jpnstz) {
		  case 1:
		    jpnpbf[jpnpnd++] = 0x80; /* Katakana */
		    jpnpbf[jpnpnd++] = (a | 0x80);
		    break;
		  case 2:
		    jpnpbf[jpnpnd++] = (a | 0x80); /* Kanji */
		    if ((a = (*fn)()) < 0)
		      return(a);
		    jpnpbf[jpnpnd++] = (a | 0x80);
		    break;
		  default:
		    jpnpbf[jpnpnd++] = a; /* Single byte */
		    break;
                }
            } else if (a == 0x0e) {
                jpnstz = 1;
                if ((a = (*fn)()) < 0)
		  return(a);
            } else if (a == 0x0f) {
                jpnstz = 0;
                if ((a = (*fn)()) < 0)
		  return(a);
            } else if (a == 0x1b) {
                jpnpbf[jpnpnd++] = a;	/* Escape */
                if ((a = (*fn)()) < 0)
		  return(a);
                jpnpbf[jpnpnd++] = a;
                if (a == '$') {
                    if ((a = (*fn)()) < 0)
		      return(a);
                    jpnpbf[jpnpnd++] = a;
                    if ((a == '@') || (a == 'B')) {
                        jpnstz = 2;
			jpnpnt = jpnpnd = 0;
                        if ((a = (*fn)()) < 0)
			  return(a);
                    }
                } else if (a == '(') {
                    if ((a = (*fn)()) < 0)
		      return( a );
                    jpnpbf[jpnpnd++] = a;
                    if ((a == 'B') || (a == 'J')) {
                        jpnstz = 0;
			jpnpnt = jpnpnd = 0;
                        if ((a = (*fn)()) < 0)
			  return(a);
                    }
                } else if (a == 0x1b) {
                    jpnpnt = jpnpnd = 0;
                    if ((a = (*fn)()) < 0)
		      return(a);
                }
            } else {
                jpnpbf[jpnpnd++] = a;
            }
        }
        jpnpnt = 0;
        a = jpnpbf[jpnpnt++];
	jpnpnd--;
        return(a);
    } else {
        a = (*fn)();
        return(a);
    }
}
#endif /* KANJI */


/*  TABLES OF TRANSLATION FUNCTIONS */

/*
  First, the table of translation functions for RECEIVING files.
  That is, *from* the TRANSFER character set *to* the FILE character set,
  an array of pointers to functions.  The first index is the
  transfer syntax character set number, the second index is the file 
  character set number.

  These arrays must be fully populated, even if (as is the case with
  Kanji character sets), all the entries are NULL.  Otherwise, 
  subscript calculations will be wrong and we'll use the wrong functions.
*/

#ifdef CK_ANSIC
CHAR (*xlr[MAXTCSETS+1][MAXFCSETS+1])(CHAR) = 
#else
CHAR (*xlr[MAXTCSETS+1][MAXFCSETS+1])() = 
#endif /* CK_ANSIC */
{
    NULL,			/* 0,0 transparent to us ascii */
    NULL,			/* 0,1 transparent to uk ascii */
    NULL,			/* 0,2 transparent to dutch nrc */
    NULL,			/* 0,3 transparent to finnish nrc */
    NULL,			/* 0,4 transparent to french nrc */
    NULL,			/* 0,5 transparent to fr-canadian nrc */
    NULL,			/* 0,6 transparent to german nrc */
    NULL,			/* 0,7 transparent to hungarian nrc */
    NULL,			/* 0,8 transparent to italian nrc */
    NULL,			/* 0,9 transparent to norge/danish nrc */
    NULL,			/* 0,10 transparent to portuguese nrc */
    NULL,			/* 0,11 transparent to spanish nrc */
    NULL,			/* 0,12 transparent to swedish nrc */
    NULL,			/* 0,13 transparent to swiss nrc */
    NULL,			/* 0,14 transparent to latin-1 */
    NULL,			/* 0,15 transparent to latin-2 */
    NULL,			/* 0,16 transparent to DEC MCS */
    NULL,			/* 0,17 transparent to NeXT */
    NULL,			/* 0,18 transparent to CP437 */
    NULL,			/* 0,19 transparent to CP850 */
    NULL,			/* 0,20 transparent to CP852 */
    NULL,			/* 0,21 transparent to Macintosh Latin */
    NULL,			/* 0,22 transparent to DGI */
    NULL,			/* 0,23 transparent to Latin/Cyrillic */
    NULL,                       /* 0,24 transparent to CP866 */
    NULL,			/* 0,25 transparent to Short KOI-7 */
    NULL,                       /* 0,26 transparent to Old KOI-8 Cyrillic */
    NULL,			/* 0,27 transparent to JIS-7 */
    NULL,			/* 0,28 transparent to Shift-JIS */
    NULL,			/* 0,29 transparent to J-EUC */
    NULL,			/* 0,30 transparent to DEC Kanji */
    NULL,			/* 1,0 ascii to us ascii */
    NULL,			/* 1,1 ascii to uk ascii */
    NULL,			/* 1,2 ascii to dutch nrc */
    NULL,			/* 1,3 ascii to finnish nrc */
    NULL,			/* 1,4 ascii to french nrc */
    NULL,			/* 1,5 ascii to fr-canadian nrc */
    NULL,			/* 1,6 ascii to german nrc */
    NULL,			/* 1,7 ascii to hungarian nrc */
    NULL,			/* 1,8 ascii to italian nrc */
    NULL,			/* 1,9 ascii to norge/danish nrc */
    NULL,			/* 1,10 ascii to portuguese nrc */
    NULL,			/* 1,11 ascii to spanish nrc */
    NULL,			/* 1,12 ascii to swedish nrc */
    NULL,			/* 1,13 ascii to swiss nrc */
    NULL,			/* 1,14 ascii to latin-1 */
    NULL,			/* 1,15 ascii to latin-2 */
    NULL,			/* 1,16 ascii to DEC MCS */
    NULL,			/* 1,17 ascii to NeXT */
    NULL,			/* 1,18 ascii to CP437 */
    NULL,			/* 1,19 ascii to CP850 */
    NULL,			/* 1,20 ascii to CP852 */
    NULL,			/* 1,21 ascii to Macintosh Latin */
    NULL,			/* 1,22 ascii to DGI */
    xaslc,                      /* 1,23 ascii to Latin/Cyrillic */
    xasac,                      /* 1,24 ascii to CP866 */
    xassk,                      /* 1,25 ascii to Short KOI */
    xask8,                      /* 1,26 ascii to Old KOI-8 Cyrillic */
    NULL,			/* 1,27 ascii to JIS-7 */
    NULL,			/* 1,28 ascii to Shift-JIS */
    NULL,			/* 1,29 ascii to J-EUC */
    NULL,			/* 1,30 ascii to DEC Kanji */
    zl1as,			/* 2,0 latin-1 to us ascii */
    xl1uk,			/* 2,1 latin-1 to uk ascii */
    xl1du,			/* 2,2 latin-1 to dutch nrc */
    xl1fi,			/* 2,3 latin-1 to finnish nrc */
    xl1fr,			/* 2,4 latin-1 to french nrc */
    xl1fc,			/* 2,5 latin-1 to fr-canadian nrc */
    xl1ge,			/* 2,6 latin-1 to german nrc */
    xl1it,			/* 2,7 latin-1 to italian nrc */
    xl1hu,			/* 2,8 latin-1 to hungarian nrc */
    xl1no,			/* 2,9 latin-1 to norge/danish nrc */
    xl1po,			/* 2,10 latin-1 to portuguese nrc */
    xl1sp,			/* 2,11 latin-1 to spanish nrc */
    xl1sw,			/* 2,12 latin-1 to swedish nrc */
    xl1ch,			/* 2,13 latin-1 to swiss nrc */
    NULL,			/* 2,14 latin-1 to latin-1 */
    xl1l2,			/* 2,15 latin-1 to latin-2 */
    xl1dm,			/* 2,16 latin-1 to DEC MCS */
    xl1ne,			/* 2,17 latin-1 to NeXT */
    xl143,			/* 2,18 latin-1 to CP437 */
    xl185,			/* 2,19 latin-1 to CP850 */
    xl152,			/* 2,20 latin-1 to CP852 */
    xl1aq,			/* 2,21 latin-1 to Macintosh Latin */
    xl1dg,			/* 2,22 latin-1 to DGI */
    zl1as,			/* 2,23 latin-1 to Latin/Cyrillic */
    zl1as,                      /* 2,24 latin-1 to CP866 */
    xl1sk,                      /* 2,25 latin-1 to Short KOI */
    zl1as,		       	/* 2,26 latin-1 to Old KOI-8 Cyrillic */
    NULL,			/* 2,27 latin-1 to JIS-7 */
    NULL,			/* 2,28 latin-1 to Shift-JIS */
    NULL,			/* 2,29 latin-1 to J-EUC */
    NULL,			/* 2,30 latin-1 to DEC Kanji */
    xl2as,			/* 3,0 latin-2 to us ascii */
    xl2as,			/* 3,1 latin-2 to uk ascii */
    xl2as,			/* 3,2 latin-2 to dutch nrc */
    xl2as,			/* 3,3 latin-2 to finnish nrc */
    xl2as,			/* 3,4 latin-2 to french nrc */
    xl2as,			/* 3,5 latin-2 to fr-canadian nrc */
    xl2as,			/* 3,6 latin-2 to german nrc */
    xl2as,			/* 3,7 latin-2 to italian nrc */
    xl2as,			/* 3,8 latin-2 to hungarian nrc */
    xl2as,			/* 3,9 latin-2 to norge/danish nrc */
    xl2as,			/* 3,10 latin-2 to portuguese nrc */
    xl2as,			/* 3,11 latin-2 to spanish nrc */
    xl2as,			/* 3,12 latin-2 to swedish nrc */
    xl2as,			/* 3,13 latin-2 to swiss nrc */
    xl2l1,			/* 3,14 latin-2 to latin-1 */
    NULL,			/* 3,15 latin-2 to latin-2 */
    xl2l1,			/* 3,16 latin-2 to DEC MCS */
    xl2ne,			/* 3,17 latin-2 to NeXT */
    xl243,			/* 3,18 latin-2 to CP437 */
    xl285,			/* 3,19 latin-2 to CP850 */
    xl252,			/* 3,20 latin-2 to CP852 */
    xl2aq,			/* 3,21 latin-2 to Macintosh Latin */
    xl2dg,			/* 3,22 latin-2 to DGI */
    xl2as,			/* 3,23 latin-2 to Latin/Cyrillic */
    xl2as,                      /* 3,24 latin-2 to CP866 */
    xl2sk,                      /* 3,25 latin-2 to Short KOI */
    xl2as,		       	/* 3,26 latin-2 to Old KOI-8 Cyrillic */
    NULL,			/* 3,27 latin-2 to JIS-7 */
    NULL,			/* 3,28 latin-2 to Shift-JIS */
    NULL,			/* 3,29 latin-2 to J-EUC */
    NULL,			/* 3,30 latin-2 to DEC Kanji */
    xlcas,			/* 4,0 latin/cyrillic to us ascii */
    xlcas,			/* 4,1 latin/cyrillic to uk ascii */
    xlcas, 		        /* 4,2 latin/cyrillic to dutch nrc */
    xlcas,			/* 4,3 latin/cyrillic to finnish ascii */
    xlcas,			/* 4,4 latin/cyrillic to french nrc */
    xlcas,			/* 4,5 latin/cyrillic to fr-canadian nrc */
    xlcas,			/* 4,6 latin/cyrillic to german nrc */
    xlcas,			/* 4,7 latin/cyrillic to italian nrc */
    xlcas,			/* 4,8 latin/cyrillic to hungarian nrc */
    xlcas,			/* 4,9 latin/cyrillic to norge/danish nrc */
    xlcas,			/* 4,10 latin/cyrillic to portuguese nrc */
    xlcas,			/* 4,11 latin/cyrillic to spanish nrc */
    xlcas,			/* 4,12 latin/cyrillic to swedish nrc */
    xlcas,			/* 4,13 latin/cyrillic to swiss nrc */
    xlcas,			/* 4,14 latin/cyrillic to latin-1 */
    xlcas,			/* 4,15 latin/cyrillic to latin-2 */
    xlcas,			/* 4,16 latin/cyrillic to DEC MCS */
    xlcas,			/* 4,17 latin/cyrillic to NeXT */
    xlcas,			/* 4,18 latin/cyrillic to CP437 */
    xlcas,			/* 4,19 latin/cyrillic to CP850 */
    xlcas,			/* 4,20 latin/cyrillic to CP852 */
    xlcas,			/* 4,21 latin/cyrillic to Macintosh Latin */
    xlcas,			/* 4,22 latin/cyrillic to DGI */
    NULL,                       /* 4,23 latin/cyrillic to Latin/Cyrillic */
    xlcac,                      /* 4,24 latin/cyrillic to CP866 */
    xlcsk,                      /* 4,25 latin/cyrillic to Short KOI */
    xlck8,		       	/* 4,26 latin/cyrillic to Old KOI-8 Cyrillic */
    NULL,			/* 4,27 latin/cyril to JIS-7 */
    NULL,			/* 4,28 latin/cyril to Shift-JIS */
    NULL,			/* 4,29 latin/cyril to J-EUC */
    NULL,			/* 4,30 latin/cyril to DEC Kanji */

/* Kanji to others ... */

    NULL,			/* 5,00 */
    NULL,			/* 5,01 */
    NULL,			/* 5,02 */
    NULL,			/* 5,03 */
    NULL,			/* 5,04 */
    NULL,			/* 5,05 */
    NULL,			/* 5,06 */
    NULL,			/* 5,07 */
    NULL,			/* 5,08 */
    NULL,			/* 5,09 */
    NULL,			/* 5,10 */
    NULL,			/* 5,11 */
    NULL,			/* 5,12 */
    NULL,			/* 5,13 */
    NULL,			/* 5,14 */
    NULL,			/* 5,15 */
    NULL,			/* 5,16 */
    NULL,			/* 5,17 */
    NULL,			/* 5,18 */
    NULL,			/* 5,19 */
    NULL,			/* 5,20 */
    NULL,			/* 5,21 */
    NULL,			/* 5,22 */
    NULL,			/* 5,23 */
    NULL,			/* 5,24 */
    NULL,			/* 5,25 */
    NULL,			/* 5,26 */
    NULL,			/* 5,27 */
    NULL,			/* 5,28 */
    NULL,			/* 5,29 */
    NULL			/* 5,30 */
};

/*
  Translation function table for sending files.
  Array of pointers to functions for translating from the local file
  character set to the transfer syntax character set.  Indexed in the same
  way as the xlr array above.
*/
#ifdef CK_ANSIC
CHAR (*xls[MAXTCSETS+1][MAXFCSETS+1])(CHAR) =
#else
CHAR (*xls[MAXTCSETS+1][MAXFCSETS+1])() =
#endif /* CK_ANSIC */
{
    NULL,			/* 0,0 us ascii to transparent */
    NULL,			/* 0,1 uk ascii to transparent */
    NULL,			/* 0,2 dutch nrc to transparent */
    NULL,			/* 0,3 finnish nrc to transparent */
    NULL,			/* 0,4 french nrc to transparent */
    NULL,			/* 0,5 fr-canadian nrc to transparent */
    NULL,			/* 0,6 german nrc to transparent */
    NULL,			/* 0,7 hungarian nrc to transparent */
    NULL,			/* 0,8 italian nrc to transparent */
    NULL,			/* 0,9 norge/danish nrc to transparent */
    NULL,			/* 0,10 portuguese nrc to transparent */
    NULL,			/* 0,11 spanish nrc to transparent */
    NULL,			/* 0,12 swedish nrc to transparent */
    NULL,			/* 0,13 swiss nrc to transparent */
    NULL,			/* 0,14 latin-1 to transparent */
    NULL,			/* 0,15 latin-2 to transparent */
    NULL,			/* 0,16 DEC MCS to transparent */
    NULL,			/* 0,17 NeXT to transparent */
    NULL,			/* 0,18 CP437 to transparent */
    NULL,			/* 0,19 CP850 to transparent */
    NULL,			/* 0,20 CP852 to transparent */
    NULL,			/* 0,21 Macintosh Latin to transparent */
    NULL,			/* 0,22 DGI to transparent */
    NULL,			/* 0,23 Latin/Cyrillic to transparent */
    NULL,                       /* 0,24 CP866 to transparent */
    NULL,                       /* 0,25 Short KOI to transparent */
    NULL,                       /* 0,26 Old KOI-8 to transparent */
    NULL,			/* 0,27 JIS-7 to transparent */
    NULL,			/* 0,28 Shift JIS to transparent */
    NULL,			/* 0,29 Japanese EUC to transparent */
    NULL,			/* 0,30 DEC Kanji to transparent */
    NULL,			/* 1,0 us ascii to ascii */
    NULL,			/* 1,1 uk ascii to ascii */
    xduas,			/* 1,2 dutch nrc to ascii */
    xfias,			/* 1,3 finnish nrc to ascii */
    xfras,			/* 1,4 french nrc to ascii */
    xfcas,			/* 1,5 french canadian nrc to ascii */
    xgeas,			/* 1,6 german nrc to ascii */
    xhuas,			/* 1,7 hungarian nrc to ascii */
    xitas,			/* 1,8 italian nrc to ascii */
    xnoas,			/* 1,9 norwegian/danish nrc to ascii */
    xpoas,			/* 1,10 portuguese nrc to ascii */
    xspas,			/* 1,11 spanish nrc to ascii */
    xswas,			/* 1,12 swedish nrc to ascii */
    xchas,			/* 1,13 swiss nrc to ascii */
    xl1as,			/* 1,14 latin-1 to ascii */
    xl2as,			/* 1,15 latin-2 to ascii */
    xdmas,			/* 1,16 dec mcs to ascii */
    xneas,			/* 1,17 NeXT to ascii */
    x43as,			/* 1,18 CP437 to ascii */
    x85as,			/* 1,19 CP850 to ascii */
    x52as,			/* 1,20 CP850 to ascii */
    xaqas,			/* 1,21 Macintosh Latin to ascii */
    xdgas,			/* 1,22 DGI to ascii */
    xlcas,			/* 1,23 Latin/Cyrillic to ASCII */
    xacas,                      /* 1,24 CP866 to ASCII */
    xskas,			/* 1,25 Short KOI to ASCII */
    xk8as,                      /* 1,26 Old KOI-8 Cyrillic to ASCII */
    NULL,			/* 1,27 */
    NULL,			/* 1,28 */
    NULL,			/* 1,29 */
    NULL,			/* 1,30 */
    NULL,			/* 2,0 us ascii to latin-1 */
    xukl1,			/* 2,1 uk ascii to latin-1 */
    xdul1,			/* 2,2 dutch nrc to latin-1 */
    xfil1,			/* 2,3 finnish nrc to latin-1 */
    xfrl1,			/* 2,4 french nrc to latin-1 */
    xfcl1,			/* 2,5 french canadian nrc to latin-1 */
    xgel1,			/* 2,6 german nrc to latin-1 */
    xhul1,			/* 2,7 hungarian nrc to latin-1 */
    xitl1,			/* 2,8 italian nrc to latin-1 */
    xnol1,			/* 2,9 norwegian/danish nrc to latin-1 */
    xpol1,			/* 2,10 portuguese nrc to latin-1 */
    xspl1,			/* 2,11 spanish nrc to latin-1 */
    xswl1,			/* 2,12 swedish nrc to latin-1 */
    xchl1,			/* 2,13 swiss nrc to latin-1 */
    NULL,			/* 2,14 latin-1 to latin-1 */
    xl2l1,			/* 2,15 latin-2 to latin-1 */
    xdml1,			/* 2,16 dec mcs to latin-1 */
    xnel1,                      /* 2,17 NeXT to Latin-1 */
    x43l1,                      /* 2,18 CP437 to Latin-1 */
    x85l1,                      /* 2,19 CP850 to Latin-1 */
    x52l1,                      /* 2,20 CP852 to Latin-1 */
    xaql1,                      /* 2,21 Macintosh Latin to Latin-1 */
    xdgl1,                      /* 2,22 DGI to Latin-1 */
    xlcas,                      /* 2,23 Latin/Cyrillic to Latin-1 */
    xacas,                      /* 2,24 CP866 to Latin-1 */
    xskas,                      /* 2,25 Short KOI to Latin-1 */
    xk8as,                      /* 2,26 Old KOI-8 Cyrillic to Latin-1 */
    NULL,			/* 2,27 Kanji ... */
    NULL,			/* 2,28 */
    NULL,			/* 2,29 */
    NULL,			/* 2,30 */
    NULL,			/* 3,0 us ascii to latin-2 */
    NULL,			/* 3,1 uk ascii to latin-2 */
    xduas,			/* 3,2 dutch nrc to latin-2 */
    xfias,			/* 3,3 finnish nrc to latin-2 */
    xfras,			/* 3,4 french nrc to latin-2 */
    xfcas,			/* 3,5 french canadian nrc to latin-2 */
    xgel2,			/* 3,6 german nrc to latin-2 */
    xhul2,			/* 3,7 hungarian nrc to latin-2 */
    xitas,			/* 3,8 italian nrc to latin-2 */
    xnoas,			/* 3,9 norwegian/danish nrc to latin-2 */
    xpoas,			/* 3,10 portuguese nrc to latin-2 */
    xspas,			/* 3,11 spanish nrc to latin-2 */
    xswas,			/* 3,12 swedish nrc to latin-2 */
    xchas,			/* 3,13 swiss nrc to latin-2 */
    xl1l2,			/* 3,14 latin-1 to latin-2 */
    NULL,			/* 3,15 latin-2 to latin-2 */
    xl1l2,			/* 3,16 dec mcs to latin-2 */
    xnel2,                      /* 3,17 NeXT to Latin-2 */
    x43l2,                      /* 3,18 CP437 to Latin-2 */
    x85l2,                      /* 3,19 CP850 to Latin-2 */
    x52l2,                      /* 3,20 CP852 to Latin-2 */
    xaql2,                      /* 3,21 Macintosh Latin to Latin-2 */
    xdgl2,                      /* 3,22 DGI to Latin-2 */
    xlcas,                      /* 3,23 Latin/Cyrillic to Latin-2 */
    xacas,                      /* 3,24 CP866 to Latin-2 */
    xskas,                      /* 3,25 Short KOI to Latin-2 */
    xk8as,                      /* 3,26 Old KOI-8 Cyrillic to Latin-2 */
    NULL,			/* 3,27 Kanji ... */
    NULL,			/* 3,28 */
    NULL,			/* 3,29 */
    NULL,			/* 3,30 */
    xaslc,			/* 4,0 us ascii to latin/cyrillic */
    xaslc,			/* 4,1 uk ascii to latin/cyrillic */
    xduas,			/* 4,2 dutch nrc to latin/cyrillic */
    xfias,			/* 4,3 finnish nrc to latin/cyrillic */
    xfras,			/* 4,4 french nrc to latin/cyrillic */
    xfcas,			/* 4,5 french canadian nrc to latin/cyrillic */
    xgeas,			/* 4,6 german nrc to latin/cyrillic */
    xhuas,			/* 4,7 hungarian nrc to latin/cyrillic */
    xitas,			/* 4,8 italian nrc to latin/cyrillic */
    xnoas,			/* 4,9 norge/danish nrc to latin/cyrillic */
    xpoas,			/* 4,10 portuguese nrc to latin/cyrillic */
    xspas,			/* 4,11 spanish nrc to latin/cyrillic */
    xswas,			/* 4,12 swedish nrc to latin/cyrillic */
    xchas,			/* 4,13 swiss nrc to latin/cyrillic */
    xl1as,			/* 4,14 latin-1 to latin/cyrillic */
    xl2as,			/* 4,15 latin-2 to latin/cyrillic */
    xdmas,			/* 4,16 dec mcs to latin/cyrillic */
    xneas,			/* 4,17 NeXT to latin/cyrillic */
    x43as,			/* 4,18 CP437 to latin/cyrillic */
    x85as,			/* 4,19 CP850 to latin/cyrillic */
    x52as,			/* 4,20 CP852 to latin/cyrillic */
    xaqas,			/* 4,21 Macintosh Latin to latin/cyrillic */
    xdgas,			/* 4,22 DGI to Latin/Cyrillic */
    NULL,                       /* 4,23 Latin/Cyrillic to Latin/Cyrillic */
    xaclc,                      /* 4,24 CP866 to Latin/Cyrillic */
    xskcy,                      /* 4,25 Short KOI to Latin/Cyrillic */
    xk8lc,                      /* 4,26 Old KOI-8 Cyrillic to Latin/Cyrillic */
    NULL,			/* 4,27 Kanji... */
    NULL,			/* 4,28 */
    NULL,			/* 4,29 */
    NULL,			/* 4,30 */
    NULL,			/* 5,00 */
    NULL,			/* 5,01 */
    NULL,			/* 5,02 */
    NULL,			/* 5,03 */
    NULL,			/* 5,04 */
    NULL,			/* 5,05 */
    NULL,			/* 5,06 */
    NULL,			/* 4.07 */
    NULL,			/* 5,08 */
    NULL,			/* 5,09 */
    NULL,			/* 5,10 */
    NULL,			/* 5,11 */
    NULL,			/* 5,12 */
    NULL,			/* 5,13 */
    NULL,			/* 5,14 */
    NULL,			/* 5,15 */
    NULL,			/* 5,16 */
    NULL,			/* 5,17 */
    NULL,			/* 5,18 */
    NULL,			/* 5,19 */
    NULL,			/* 5,20 */
    NULL,			/* 5,21 */
    NULL,			/* 5,22 */
    NULL,			/* 5,23 */
    NULL,			/* 5,24 */
    NULL,			/* 5,25 */
    NULL,			/* 5,26 */
    NULL,			/* 5,27 */
    NULL,			/* 5,28 */
    NULL,			/* 5,29 */
    NULL,			/* 5,30 */
};
#endif /* NOCSETS */
