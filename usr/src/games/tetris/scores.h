/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scores.h	5.1 (Berkeley) %G%
 */

/*
 * Tetris scores.
 */
struct highscore {
	char	hs_name[20];	/* login name */
	int	hs_score;	/* raw score */
	int	hs_level;	/* play level */
	time_t	hs_time;	/* time at game end */
};

#define MAXHISCORES	80
#define MAXSCORES	9	/* maximum high score entries per person */
#define	EXPIRATION	(5L * 365 * 24 * 60 * 60)

void	savescore __P((int));
void	showscores __P((int));
