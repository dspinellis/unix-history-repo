/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)robots.h	5.1 (Berkeley) %G%
 */

# include	<curses.h>
# include	<setjmp.h>

/*
 * miscellaneous constants
 */

# define	Y_FIELDSIZE	23
# define	X_FIELDSIZE	60
# define	Y_SIZE		24
# define	X_SIZE		80
# define	MAXLEVELS	4
# define	MAXROBOTS	(MAXLEVELS * 10)
# define	ROB_SCORE	10
# define	S_BONUS		(60 * ROB_SCORE)
# define	Y_SCORE		21
# define	X_SCORE		(X_FIELDSIZE + 9)
# define	Y_PROMPT	(Y_FIELDSIZE - 1)
# define	X_PROMPT	(X_FIELDSIZE + 2)
# define	MAXSCORES	(Y_SIZE - 2)
# define	MAXNAME		16
# define	MS_NAME		"Ten"
# define	SCOREFILE	"/usr/games/lib/robots_roll"

/*
 * characters on screen
 */

# define	ROBOT	'+'
# define	HEAP	'*'
# define	PLAYER	'@'

/*
 * pseudo functions
 */

# undef		CTRL
# define	CTRL(X)	('X' - 'A' + 1)

/*
 * type definitions
 */

typedef struct {
	int	y, x;
} COORD;

/*
 * global variables
 */

extern bool	Dead, Full_clear, Jump, Newscore, Real_time, Running,
		Teleport, Waiting, Was_bonus;

#ifdef	FANCY
extern bool	Pattern_roll, Stand_still;
#endif

extern char	Cnt_move, Field[Y_FIELDSIZE][X_FIELDSIZE], *Next_move,
		*Move_list, Run_ch;

extern int	Count, Level, Num_robots, Num_scores, Score,
		Start_level, Wait_bonus;

extern COORD	Max, Min, My_pos, Robots[];

extern jmp_buf	End_move;

/*
 * functions types
 */

int	quit(), cmp_sc(), move_robots();

COORD	*rnd_pos();
