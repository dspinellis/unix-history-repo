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
 *
 *	@(#)cribcur.h	5.2 (Berkeley) %G%
 */

# define	PLAY_Y		15	/* size of player's hand window */
# define	PLAY_X		12
# define	TABLE_Y		21	/* size of table window */
# define	TABLE_X		14
# define	COMP_Y		15	/* size of computer's hand window */
# define	COMP_X		12
# define	Y_SCORE_SZ	9	/* Y size of score board */
# define	X_SCORE_SZ	41	/* X size of score board */
# define	SCORE_Y		0	/* starting position of scoring board */
# define	SCORE_X	 	(PLAY_X + TABLE_X + COMP_X)
# define	CRIB_Y		17	/* position of crib (cut card) */
# define	CRIB_X		(PLAY_X + TABLE_X)
# define	MSG_Y		(LINES - (Y_SCORE_SZ + 1))
# define	MSG_X		(COLS - SCORE_X - 1)
# define	Y_MSG_START	(Y_SCORE_SZ + 1)

# define	PEG	'*'	/* what a peg looks like on the board */

extern	WINDOW		*Compwin;		/* computer's hand window */
extern	WINDOW		*Msgwin;		/* message window */
extern	WINDOW		*Playwin;		/* player's hand window */
extern	WINDOW		*Tablewin;		/* table window */
