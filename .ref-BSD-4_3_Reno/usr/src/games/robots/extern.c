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

# include	"robots.h"

bool	Dead;			/* Player is now dead */
bool	Full_clear = TRUE;	/* Lots of junk for init_field to clear */
bool	Jump = FALSE;		/* Jump while running, counting, or waiting */
bool	Newscore;		/* There was a new score added */
#ifdef	FANCY
bool	Pattern_roll = FALSE;	/* Auto play for YHBJNLUK pattern */
#endif
bool	Real_time = FALSE;	/* Play in real time? */
bool	Running = FALSE;	/* Currently in the middle of a run */
#ifdef	FANCY
bool	Stand_still = FALSE;	/* Auto play for standing still pattern */
#endif
bool	Teleport = FALSE;	/* Teleport automatically when player must */
bool	Waiting;		/* Player is waiting for end */
bool	Was_bonus = FALSE;	/* Was a bonus last level */

char	Cnt_move;		/* Command which has preceded the count */
char	Field[Y_FIELDSIZE][X_FIELDSIZE];	/* the playing field itslef */
char	*Next_move;		/* Next move to be used in the pattern */
char	*Move_list = "YHBJNLUK";/* List of moves in the pattern */
char	Run_ch;			/* Character for the direction we are running */

int	Count = 0;		/* Command count */
int	Level;			/* Current level */
int	Num_robots;		/* Number of robots left */
int	Num_scores;		/* Number of scores posted */
int	Score;			/* Current score */
int	Start_level = 1;	/* Level on which to start */
int	Wait_bonus;		/* bonus for waiting */

COORD	Max;			/* Max area robots take up */
COORD	Min;			/* Min area robots take up */
COORD	My_pos;			/* Player's current position */
COORD	Robots[MAXROBOTS];	/* Robots' current positions */

jmp_buf	End_move;		/* Jump to on Real_time */
