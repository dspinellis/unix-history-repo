/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)extern.c	5.3 (Berkeley) 6/18/88";
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
