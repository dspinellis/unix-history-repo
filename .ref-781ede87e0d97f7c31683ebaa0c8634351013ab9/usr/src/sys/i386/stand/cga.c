/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)cga.c	7.1 (Berkeley) %G%
 */

typedef unsigned short u_short;
typedef unsigned char u_char;

#define	CRT_TXTADDR	((u_short *)0xb8000)
#define	COL		80
#define	ROW		25
#define	CHR		2

u_short	*crtat;
u_char	color = 0xe ;
int row;

sput(c) u_char c; {

	if (crtat == 0) {
		crtat = CRT_TXTADDR; bzero (crtat,COL*ROW*CHR);
	}
	if (crtat >= (CRT_TXTADDR+COL*ROW*CHR)) {
		crtat = CRT_TXTADDR+COL*(ROW-1); row = 0;
	}
	switch(c) {

	case '\t':
		do {
			*crtat++ = (color<<8)| ' '; row++ ;
		} while (row %8);
		break;

	case '\010':
		crtat--; row--;
		break;

	case '\r':
		bzero (crtat,(COL-row)*CHR) ; crtat -= row ; row = 0;
		break;

	case '\n':
		if (crtat >= CRT_TXTADDR+COL*(ROW-1)) { /* scroll */
			bcopy(CRT_TXTADDR+COL, CRT_TXTADDR,COL*(ROW-1)*CHR);
			bzero (CRT_TXTADDR+COL*(ROW-1),COL*CHR) ;
			crtat -= COL ;
		}
		crtat += COL ;
		break;

	default:
		*crtat++ = (color<<8)| c; row++ ;
		break ;
	}
}
