/*
 *	back.h	4.1	82/05/11
 */

#include <sgtty.h>

#define rnum(r)	(rand(0)/(32767/r+1))
#define D0	dice[0]
#define D1	dice[1]
#define swap	{D0 ^= D1; D1 ^= D0; D0 ^= D1; d0 = 1-d0;}

/*
 *
 * Some numerical conventions:
 *
 *	Arrays have white's value in [0], red in [1].
 *	Numeric values which are one color or the other use
 *	-1 for white, 1 for red.
 *	Hence, white will be negative values, red positive one.
 *	This makes a lot of sense since white is going in decending
 *	order around the board, and red is ascending.
 *
 */

char	EXEC[];			/* object for main program */
char	TEACH[];		/* object for tutorial program */

int	pnum;			/* color of player:
					-1 = white
					 1 = red
					 0 = both
					 2 = not yet init'ed */
char	args[100];		/* args passed to teachgammon and back */
int	acnt;			/* length of args */
int	aflag;			/* flag to ask for rules or instructions */
int	bflag;			/* flag for automatic board printing */
int	cflag;			/* case conversion flag */
int	hflag;			/* flag for cleaning screen */
int	mflag;			/* backgammon flag */
int	raflag;			/* 'roll again' flag for recovered game */
int	rflag;			/* recovered game flag */
int	tflag;			/* cursor addressing flag */
int	rfl;			/* saved value of rflag */
int	iroll;			/* special flag for inputting rolls */
int	board[26];		/* board:  negative values are white,
				   positive are red */
int	dice[2];		/* value of dice */
int	mvlim;			/* 'move limit':  max. number of moves */
int	mvl;			/* working copy of mvlim */
int	p[5];			/* starting position of moves */
int	g[5];			/* ending position of moves (goals) */
int	h[4];			/* flag for each move if a man was hit */
int	cturn;			/* whose turn it currently is:
					-1 = white
					 1 = red
					 0 = just quitted
					-2 = white just lost
					 2 = red just lost */
int	d0;			/* flag if dice have been reversed from
				   original position */
int	table[6][6];		/* odds table for possible rolls */
int	rscore;			/* red's score */
int	wscore;			/* white's score */
int	gvalue;			/* value of game (64 max.) */
int	dlast;			/* who doubled last (0 = neither) */
int	bar;			/* position of bar for current player */
int	home;			/* position of home for current player */
int	off[2];			/* number of men off board */
int	*offptr;		/* pointer to off for current player */
int	*offopp;		/* pointer to off for opponent */
int	in[2];			/* number of men in inner table */
int	*inptr;			/* pointer to in for current player */
int	*inopp;			/* pointer to in for opponent */

int	ncin;			/* number of characters in cin */
char	cin[100];		/* input line of current move
				   (used for reconstructing input after
				   a backspace) */

char	*color[];
				/* colors as strings */
char	**colorptr;		/* color of current player */
char	**Colorptr;		/* color of current player, capitalized */
int	colen;			/* length of color of current player */

struct sgttyb	tty;		/* tty information buffer */
int		old;		/* original tty status */
int		noech;		/* original tty status without echo */
int		raw;		/* raw tty status, no echo */

int	curr;			/* row position of cursor */
int	curc;			/* column position of cursor */
int	begscr;			/* 'beginning' of screen
				   (not including board) */

int	getout();		/* function to exit backgammon cleanly */
