/*
 * Copyright (c) 1980 Regents of the University of California.
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
static char sccsid[] = "@(#)init.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sgtty.h>

/*
 * variable initialization.
 */

				/* name of executable object programs */
char	EXEC[] = "/usr/games/backgammon";
char	TEACH[] = "/usr/games/teachgammon";

int	pnum	= 2;		/* color of player:
					-1 = white
					 1 = red
					 0 = both
					 2 = not yet init'ed */
int	acnt	= 0;		/* length of args */
int	aflag	= 1;		/* flag to ask for rules or instructions */
int	bflag	= 0;		/* flag for automatic board printing */
int	cflag	= 0;		/* case conversion flag */
int	hflag	= 1;		/* flag for cleaning screen */
int	mflag	= 0;		/* backgammon flag */
int	raflag	= 0;		/* 'roll again' flag for recovered game */
int	rflag	= 0;		/* recovered game flag */
int	tflag	= 0;		/* cursor addressing flag */
int	iroll	= 0;		/* special flag for inputting rolls */
int	rfl	= 0;

char	*color[] = {"White","Red","white","red"};
