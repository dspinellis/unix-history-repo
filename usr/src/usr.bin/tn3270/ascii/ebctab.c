/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)ebctab.c	1.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * ebcdic to ascii translation tables
 */

#include "ascebc.h"

unsigned char	ebcasc[NEBCASC][NEBC] = {
/* 00 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 08 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 10 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 18 */   ' ',  ' ',  ' ',  ' ',  '*',  ' ',  ';',  ' ',
/* 20 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 28 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 30 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 38 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 40 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',

/* 48 */   ' ',  ' ', 
#if	!defined(MSDOS)
        /* 4A */       '\\',
#else	/* !defined(MSDOS) */
        /* 4A */       '\233',		/* PC cent sign */
#endif	/* !defined(MSDOS) */
        /* 4B */              '.',  '<',  '(',  '+',  '|',

/* 50 */   '&',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 58 */   ' ',  ' ',  '!',  '$',  '*',  ')',  ';',  '^',
/* 60 */   '-',  '/',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 68 */   ' ',  ' ',  '|',  ',',  '%',  '_',  '>',  '?',
/* 70 */   ' ',  '^',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 78 */   ' ',  '`',  ':',  '#',  '@', '\'',  '=',  '"',
/* 80 */   ' ',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
/* 88 */   'h',  'i',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 90 */   ' ',  'j',  'k',  'l',  'm',  'n',  'o',  'p',
/* 98 */   'q',  'r',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* A0 */   ' ',  '~',  's',  't',  'u',  'v',  'w',  'x',
/* A8 */   'y',  'z',  ' ',  ' ',  ' ',  '[',  ' ',  ' ',
/* B0 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* B8 */   ' ',  ' ',  ' ',  ' ',  ' ',  ']',  ' ',  ' ',
/* C0 */   '{',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
/* C8 */   'H',  'I',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* D0 */   '}',  'J',  'K',  'L',  'M',  'N',  'O',  'P',
/* D8 */   'Q',  'R',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* E0 */  '\\',  ' ',  'S',  'T',  'U',  'V',  'W',  'X',
/* E8 */   'Y',  'Z',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* F0 */   '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
/* F8 */   '8',  '9',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
};
