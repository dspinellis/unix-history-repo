/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"

const unsigned char	*SyntaxTable = CharTable[FUNDAMENTAL];	/* Current table to use. */

#define	cU	C_UPPER	/* Upper case */
#define	cL	C_LOWER	/* Lower case */
#define	cN	C_DIGIT	/* Numeric */
#define	cP	C_PUNCT	/* Punctuation */
#define	cC	C_CTRL	/* Control */
#define	cW	C_WORD	/* Word */
#define	cOp	C_BRA	/* Open Parenthesis */
#define	cCl	C_KET	/* Close Parenthesis */

const unsigned char CharTable[NMAJORS][NCHARS] = {
	/* FUNDAMENTAL mode */
    {
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cP,	cP,	cP,	cP,	cP,	cP,	cP,	cP,
	cOp|cP,	cCl|cP,	cP,	cP,	cP,	cP,	cP,	cP,
	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,
	cW|cN,	cW|cN,	cP,	cP,	cP,	cP,	cP,	cP,
	cP,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cOp|cP,	cP,	cCl|cP,	cP,	cP,
	cP,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cOp|cP,	cP,	cCl|cP,	cP,	cC,
#ifdef	IBMPC
	0, cW|cL, 0, 0, cW|cL, 0, 0, 0, 0, 0, 0, 0, 0, 0, cW|cU, 0,
	0, 0, 0, 0, cW|cL, 0, 0, 0, 0, cW|cU, cW|cU, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#endif	/* IBMPC */
#ifdef	MAC	/* See Inside Macintosh Vol One p. 247 */
	cW|cU, cW|cU, cW|cU, cW|cU, cW|cU, cW|cU, cW|cU, cW|cL,
	cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL,
	cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL,
	cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL,
	cP, cP, cP, cP, cP, cP, cP, cP,
	cP, cP, cP, cP, cP, cP, cW|cU, cW|cU,
	cP, cP, cP, cP, cP, cW|cU, cW|cL, cW|cU,
	cW|cU, cW|cL, cP, cP, cP, cW|cU, cW|cL, cW|cL,
	cP, cP, cP, cP, cP, cP, cW|cU, cP,
	cP, cP, cP, cW|cU, cW|cU, cW|cU, cW|cU, cW|cU,
	cP, cP, cP, cP, cP, cP, cP, cP,
	cW|cU, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#endif	/* MAC */
    },

	/* TEXT mode */
    {
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cP,	cP,	cP,	cP,	cP,	cP,	cP,	cP|cW,
	cOp|cP,	cCl|cP,	cP,	cP,	cP,	cP,	cP,	cP,
	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,
	cW|cN,	cW|cN,	cP,	cP,	cP,	cP,	cP,	cP,
	cP,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cOp|cP,	cP,	cCl|cP,	cP,	cP,
	cP,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cOp|cP,	cP,	cCl|cP,	cP,	cC,
#ifdef	IBMPC
	0, cW|cL, 0, 0, cW|cL, 0, 0, 0, 0, 0, 0, 0, 0, 0, cW|cU, 0,
	0, 0, 0, 0, cW|cL, 0, 0, 0, 0, cW|cU, cW|cU, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#endif	/* IBMPC */
#ifdef	MAC	/* See Inside Macintosh Vol One p. 247 */
	cW|cU, cW|cU, cW|cU, cW|cU, cW|cU, cW|cU, cW|cU, cW|cL,
	cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL,
	cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL,
	cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL, cW|cL,
	cP, cP, cP, cP, cP, cP, cP, cP,
	cP, cP, cP, cP, cP, cP, cW|cU, cW|cU,
	cP, cP, cP, cP, cP, cW|cU, cW|cL, cW|cU,
	cW|cU, cW|cL, cP, cP, cP, cW|cU, cW|cL, cW|cL,
	cP, cP, cP, cP, cP, cP, cW|cU, cP,
	cP, cP, cP, cW|cU, cW|cU, cW|cU, cW|cU, cW|cU,
	cP, cP, cP, cP, cP, cP, cP, cP,
	cW|cU, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#endif	/* MAC */
    },

	/* CMODE */
    {
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cP,	cP,	cP,	cP,	cP|cW,	cP,	cP,	cP,
	cOp|cP,	cCl|cP,	cP,	cP,	cP,	cP,	cP,	cP,
	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,
	cW|cN,	cW|cN,	cP,	cP,	cP,	cP,	cP,	cP,
	cP,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cOp|cP,	cP,	cCl|cP,	cP,	cP|cW,
	cP,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cOp|cP,	cP,	cCl|cP,	cP,	cC,
#ifndef	ASCII7
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#endif	/* !ASCII7 */
    },

	/* LISP mode */
#ifdef	LISP
    {
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cC,	cC,	cC,	cC,	cC,	cC,	cC,	cC,
	cP,	cW|cP,	cP,	cP,	cW|cP,	cW|cP,	cW|cP,	cP,
	cOp|cP,	cCl|cP,	cW|cP,	cW|cP,	cP,	cW|cP,	cP,	cW,
	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,	cW|cN,
	cW|cN,	cW|cN,	cW|cP,	cP,	cW|cP,	cW|cP,	cW|cP,	cW|cP,
	cW|cP,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,	cW|cU,
	cW|cU,	cW|cU,	cW|cU,	cOp|cP,	cP,	cCl|cP,	cW|cP,	cW|cP,
	cP,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,	cW|cL,
	cW|cL,	cW|cL,	cW|cL,	cOp|cW|cP,	cW|cP,	cCl|cW|cP,	cW|cP,	cW|cC,
#ifndef	ASCII7
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#endif	/* !ASCII7 */
    },
#endif	/* LISP */
};

#undef	cU
#undef	cL
#undef	cN
#undef	cP
#undef	cC
#undef	cW
#undef	cOp
#undef	cCl

int
ismword(c)
int	c;
{
	return ((CharTable[curbuf->b_major])[c]&(C_WORD));
}

/* Map lower case characters to upper case and the rest to themselves. */

const char	RaiseTable[NCHARS] = {
	'\000',	'\001',	'\002',	'\003',	'\004',	'\005',	'\006',	'\007',
	'\010',	'\011',	'\012',	'\013',	'\014',	'\015',	'\016',	'\017',
	'\020',	'\021',	'\022',	'\023',	'\024',	'\025',	'\026',	'\027',
	'\030',	'\031',	'\032',	'\033',	'\034',	'\035',	'\036',	'\037',
	'\040',	'!',	'"',	'#',	'$',	'%',	'&',	'\'',
	'(',	')',	'*',	'+',	',',	'-',	'.',	'/',
	'0',	'1',	'2',	'3',	'4',	'5',	'6',	'7',
	'8',	'9',	':',	';',	'<',	'=',	'>',	'?',
	'@',	'A',	'B',	'C',	'D',	'E',	'F',	'G',
	'H',	'I',	'J',	'K',	'L',	'M',	'N',	'O',
	'P',	'Q',	'R',	'S',	'T',	'U',	'V',	'W',
	'X',	'Y',	'Z',	'[',	'\\',	']',	'^',	'_',
	'`',	'A',	'B',	'C',	'D',	'E',	'F',	'G',
	'H',	'I',	'J',	'K',	'L',	'M',	'N',	'O',
	'P',	'Q',	'R',	'S',	'T',	'U',	'V',	'W',
	'X',	'Y',	'Z',	'{',	'|',	'}',	'~',	'\177',
#ifdef	IBMPC
	/* Only lower case codes are Umlauted u, a, and o (indented) */
	0x80,	 0x9A,	0x82,	0x83,	 0x8E,	0x85,	0x86,	0x87,
	0x88,	0x89,	0x8A,	0x8B,	0x8C,	0x8D,	0x8E,	0x8F,
	0x90,	0x91,	0x92,	0x93,	 0x99,	0x95,	0x96,	0x97,
	0x98,	0x99,	0x9A,	0x9B,	0x9C,	0x9D,	0x9E,	0x9F,
	0xA0,	0xA1,	0xA2,	0xA3,	0xA4,	0xA5,	0xA6,	0xA7,
	0xA8,	0xA9,	0xAA,	0xAB,	0xAC,	0xAD,	0xAE,	0xAF,
	0xB0,	0xB1,	0xB2,	0xB3,	0xB4,	0xB5,	0xB6,	0xB7,
	0xB8,	0xB9,	0xBA,	0xBB,	0xBC,	0xBD,	0xBE,	0xBF,
	0xC0,	0xC1,	0xC2,	0xC3,	0xC4,	0xC5,	0xC6,	0xC7,
	0xC8,	0xC9,	0xCA,	0xCB,	0xCC,	0xCD,	0xCE,	0xCF,
	0xD0,	0xD1,	0xD2,	0xD3,	0xD4,	0xD5,	0xD6,	0xD7,
	0xD8,	0xD9,	0xDA,	0xDB,	0xDC,	0xDD,	0xDE,	0xDF,
	0xE0,	0xE1,	0xE2,	0xE3,	0xE4,	0xE5,	0xE6,	0xE7,
	0xE8,	0xE9,	0xEA,	0xEB,	0xEC,	0xED,	0xEE,	0xEF,
	0xF0,	0xF1,	0xF2,	0xF3,	0xF4,	0xF5,	0xF6,	0xF7,
	0xF8,	0xF9,	0xFA,	0xFB,	0xFC,	0xFD,	0xFE,	0xFF,
#endif	/*!IBMPC*/
#ifdef	MAC
	0x80,	0x81,	0x82,	0x83,	0x84,	0x85,	0x86,	0x87,
	 0xCB,	0x89,	 0x80,	 0xCC,	 0x81,	 0x82,	 0x83,	0x8F,
	0x90,	0x91,	0x92,	0x93,	0x94,	0x95,	 0x84,	0x97,
	0x98,	0x99,	 0x85,	 0xCD,	0x9C,	0x9D,	0x9E,	 0x86,
	0xA0,	0xA1,	0xA2,	0xA3,	0xA4,	0xA5,	0xA6,	0xA7,
	0xA8,	0xA9,	0xAA,	0xAB,	0xAC,	0xAD,	0xAE,	0xAF,
	0xB0,	0xB1,	0xB2,	0xB3,	0xB4,	0xB5,	0xC6,	0xB7,
	0xB8,	 0xB8,	0xBA,	0xBB,	0xBC,	0xBD,	0xAE,	0xAF,
	0xC0,	0xC1,	0xC2,	0xC3,	0xC4,	0xC5,	0xC6,	0xC7,
	0xC8,	0xC9,	0xCA,	0xCB,	0xCC,	0xCD,	0xCE,	 0xCE,
	0xD0,	0xD1,	0xD2,	0xD3,	0xD4,	0xD5,	0xD6,	0xD7,
	0xD8,	0xD9,	0xDA,	0xDB,	0xDC,	0xDD,	0xDE,	0xDF,
	0xE0,	0xE1,	0xE2,	0xE3,	0xE4,	0xE5,	0xE6,	0xE7,
	0xE8,	0xE9,	0xEA,	0xEB,	0xEC,	0xED,	0xEE,	0xEF,
	0xF0,	0xF1,	0xF2,	0xF3,	0xF4,	0xF5,	0xF6,	0xF7,
	0xF8,	0xF9,	0xFA,	0xFB,	0xFC,	0xFD,	0xFE,	0xFF,
#endif	/*MAC*/
};
