/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)char.h	3.9 (Berkeley) 6/6/90
 */

/*
 * Macros and things to deal with control characters.
 *
 * Unctrl() is just like the standard function, except we don't want
 * to include curses.
 * Isctrl() returns true for all characters less than space and
 * greater than or equal to delete.
 * Isprt() is tab and all characters not isctrl().  It's used
 * by wwwrite().
 * Isunctrl() includes all characters that should be expanded
 * using unctrl() by wwwrite() if ww_unctrl is set.
 */

extern char *_unctrl[];
extern char _cmap[];
#define ctrl(c)		(c & 0x1f)
#define unctrl(c)	(_unctrl[(unsigned char) (c)])
#define _C		0x01
#define _P		0x02
#define _U		0x04
#define isctrl(c)	(_cmap[(unsigned char) (c)] & _C)
#define isprt(c)	(_cmap[(unsigned char) (c)] & _P)
#define isunctrl(c)	(_cmap[(unsigned char) (c)] & _U)
