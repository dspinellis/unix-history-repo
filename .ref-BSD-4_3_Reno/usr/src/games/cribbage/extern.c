/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)extern.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	<curses.h>
# include	"deck.h"
# include	"cribbage.h"

bool	explain		= FALSE;	/* player mistakes explained */
bool	iwon		= FALSE;	/* if comp won last game */
bool	quiet		= FALSE;	/* if suppress random mess */
bool	rflag		= FALSE;	/* if all cuts random */

char	expl[128];			/* explanation */

int	cgames		= 0;		/* number games comp won */
int	cscore		= 0;		/* comp score in this game */
int	gamecount	= 0;		/* number games played */
int	glimit		= LGAME;	/* game playe to glimit */
int	knownum		= 0;		/* number of cards we know */
int	pgames		= 0;		/* number games player won */
int	pscore		= 0;		/* player score in this game */

CARD	chand[FULLHAND];		/* computer's hand */
CARD	crib[CINHAND];			/* the crib */
CARD	deck[CARDS];			/* a deck */
CARD	known[CARDS];			/* cards we have seen */
CARD	phand[FULLHAND];		/* player's hand */
CARD	turnover;			/* the starter */

WINDOW	*Compwin;			/* computer's hand window */
WINDOW	*Msgwin;			/* messages for the player */
WINDOW	*Playwin;			/* player's hand window */
WINDOW	*Tablewin;			/* table window */
