/*
 * Copyright (c) 1981, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)curses.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <curses.h>

/* Private. */
int	__echoit = 1;			/* If stty indicates ECHO. */
int	__pfast;
int	__rawmode = 0;			/* If stty indicates RAW mode. */
int	__noqch = 0;			/* 
					 * If terminal doesn't have 
					 * insert/delete line capabilities 
					 * for quick change on refresh.
					 */
char	AM, BS, CA, DA, EO, HC, IN, MI, MS, NC, NS, OS, PC,
	UL, XB, XN, XT, XS, XX;
char	*AL, *BC, *BT, *CD, *CE, *CL, *CM, *CR, *CS, *DC, *DL,
	*DM, *DO, *ED, *EI, *K0, *K1, *K2, *K3, *K4, *K5, *K6,
	*K7, *K8, *K9, *HO, *IC, *IM, *IP, *KD, *KE, *KH, *KL,
	*KR, *KS, *KU, *LL, *MA, *ND, *NL, *RC, *SC, *SE, *SF,
	*SO, *SR, *TA, *TE, *TI, *UC, *UE, *UP, *US, *VB, *VS,
	*VE, *al, *dl, *sf, *sr,
	*AL_PARM, *DL_PARM, *UP_PARM, *DOWN_PARM, *LEFT_PARM,
	*RIGHT_PARM;
/*
 * Public.
 *
 * XXX
 * UPPERCASE isn't used by libcurses, and is left for backward
 * compatibility only.
 */
WINDOW	*curscr;			/* Current screen. */
WINDOW	*stdscr;			/* Standard screen. */
int	 COLS;				/* Columns on the screen. */
int	 LINES;				/* Lines on the screen. */
int	 My_term = 0;			/* Use Def_term regardless. */
char	*Def_term = "unknown";		/* Default terminal type. */
char	 GT;				/* Gtty indicates tabs. */
char	 NONL;				/* Term can't hack LF doing a CR. */
char	 UPPERCASE;			/* Terminal is uppercase only. */
