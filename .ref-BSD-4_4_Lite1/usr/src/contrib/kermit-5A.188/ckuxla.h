/*
  File CKUXLA.H
  Language and Character Set Support for UNIX and VAX/VMS.

  This file should be used as a template for the language support files
  for other C-Kermit implementations -- Macintosh, OS/2, Amiga, etc.
*/
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

#ifndef CKUXLA_H
#define CKUXLA_H

/* Codes for local file character sets */

/* ISO 646 and other ISO-646-like 7-bit sets */

#define FC_USASCII 0   /* US ASCII */
#define FC_UKASCII 1   /* United Kingdom ASCII */
#define FC_DUASCII 2   /* Dutch ISO 646 NRC */
#define FC_FIASCII 3   /* Finnish ISO 646 NRC */
#define FC_FRASCII 4   /* French ISO 646 NRC */
#define FC_FCASCII 5   /* French Canadian ISO 646 NRC */
#define FC_GEASCII 6   /* German ISO 646 NRC */
#define FC_HUASCII 7   /* Hungarian ISO 646 NRC */
#define FC_ITASCII 8   /* Italian *ISO 646 NRC */
#define FC_NOASCII 9   /* Norwegian and Danish ISO 646 NRC */
#define FC_POASCII 10  /* Portuguese ISO 646 NRC */
#define FC_SPASCII 11  /* Spanish ISO 646 NRC */
#define FC_SWASCII 12  /* Swedish ISO 646 NRC */
#define FC_CHASCII 13  /* Swiss ISO 646 NRC */

/* 8-bit Roman character sets */

#define FC_1LATIN  14  /* ISO 8859-1 Latin Alphabet 1 */
#define FC_2LATIN  15  /* ISO 8859-2 Latin Alphabet 2 */
#define FC_DECMCS  16  /* DEC Multinational Character Set */
#define FC_NEXT    17  /* NeXT workstation character set */
#define FC_CP437   18  /* IBM PC Code Page 437 */
#define FC_CP850   19  /* IBM PC Code Page 850 */
#define FC_CP852   20  /* IBM PC Code Page 852 */
#define FC_APPQD   22  /* Apple Quickdraw */
#define FC_DGMCS   22  /* Data General International Character Set */

/* Cyrillic sets */

#define FC_CYRILL  23  /* ISO 8859-5 Latin/Cyrillic */
#define FC_CP866   24  /* Microsoft CP866 Cyrillic */
#define FC_KOI7    25  /* KOI-7 = Short KOI */
#define FC_KOI8    26  /* KOI-8 */

/* Japanese sets */

#define FC_JIS7    27  /* JIS-7 */
#define FC_SHJIS   28  /* Shifted JIS = CP932 */
#define FC_JEUC    29  /* Japanese EUC (JAE) */
#define FC_JDEC    30  /* Japanese DEC Kanji */

#define MAXFCSETS  30  /* Highest file character set number */

#endif /* CKUXLA_H */
