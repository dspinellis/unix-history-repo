/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */


/*
 * ebcdic to ascii translation tables
 */

#include "ascebc.h"

#ifndef	lint
static	char	sccsid[] = "@(#)ebctab.c	2.3";
#endif	/* ndef lint */

char	ebcasc[NEBCASC][NEBC] = {
/* 00 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 08 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 10 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 18 */   ' ',  ' ',  ' ',  ' ',  '*',  ' ',  ';',  ' ',
/* 20 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 28 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 30 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 38 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 40 */   ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 48 */   ' ',  ' ',  '\\',  '.',  '<',  '(',  '+',  '|',
/* 50 */   '&',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 58 */   ' ',  ' ',  '!',  '$',  '*',  ')',  ';',  '^',
/* 60 */   '-',  '/',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
/* 68 */   ' ',  ' ',  ' ',  ',',  '%',  '_',  '>',  '?',
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
