/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Mark Nudleman and the University of California, Berkeley.  The
 * name of Mark Nudleman or the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)cmd.h	5.3 (Berkeley) %G%
 */

#define	MAX_USERCMD		200
#define	MAX_CMDLEN		16

#define	A_AGAIN_SEARCH		1
#define	A_B_LINE		2
#define	A_B_SCREEN		3
#define	A_B_SCROLL		4
#define	A_B_SEARCH		5
#define	A_DIGIT			6
#define	A_DISP_OPTION		7
#define	A_DEBUG			8
#define	A_EXAMINE		9
#define	A_FIRSTCMD		10
#define	A_FREPAINT		11
#define	A_F_LINE		12
#define	A_F_SCREEN		13
#define	A_F_SCROLL		14
#define	A_F_SEARCH		15
#define	A_GOEND			16
#define	A_GOLINE		17
#define	A_GOMARK		18
#define	A_HELP			19
#define	A_NEXT_FILE		20
#define	A_PERCENT		21
#define	A_PREFIX		22
#define	A_PREV_FILE		23
#define	A_QUIT			24
#define	A_REPAINT		25
#define	A_SETMARK		26
#define	A_SHELL			27
#define	A_STAT			28

#define	A_TOGGLE_OPTION		30
#define	A_VISUAL		32

#define	A_INVALID		100
#define	A_NOACTION		101
