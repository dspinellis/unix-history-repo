/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek and Darren F. Provine.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tetris.h	8.1 (Berkeley) %G%
 */

/*
 * Definitions for Tetris.
 */

/*
 * The display (`board') is composed of 23 rows of 12 columns of characters
 * (numbered 0..22 and 0..11), stored in a single array for convenience.
 * Columns 1 to 10 of rows 1 to 20 are the actual playing area, where
 * shapes appear.  Columns 0 and 11 are always occupied, as are all
 * columns of rows 21 and 22.  Rows 0 and 22 exist as boundary areas
 * so that regions `outside' the visible area can be examined without
 * worrying about addressing problems.
 */

	/* the board */
#define	B_COLS	12
#define	B_ROWS	23
#define	B_SIZE	(B_ROWS * B_COLS)

typedef unsigned char cell;
cell	board[B_SIZE];		/* 1 => occupied, 0 => empty */

	/* the displayed area (rows) */
#define	D_FIRST	1
#define	D_LAST	22

	/* the active area (rows) */
#define	A_FIRST	1
#define	A_LAST	21

/*
 * Minimum display size.
 */
#define	MINROWS	23
#define	MINCOLS	40

int	Rows, Cols;		/* current screen size */

/*
 * Translations from board coordinates to display coordinates.
 * As with board coordinates, display coordiates are zero origin.
 */
#define	RTOD(x)	((x) - 1)
#define	CTOD(x)	((x) * 2 + (((Cols - 2 * B_COLS) >> 1) - 1))

/*
 * A `shape' is the fundamental thing that makes up the game.  There
 * are 7 basic shapes, each consisting of four `blots':
 *
 *	X.X	  X.X		X.X
 *	  X.X	X.X	X.X.X	X.X	X.X.X	X.X.X	X.X.X.X
 *			  X		X	    X
 *
 *	  0	  1	  2	  3	  4	  5	  6
 *
 * Except for 3 and 6, the center of each shape is one of the blots.
 * This blot is designated (0,0).  The other three blots can then be
 * described as offsets from the center.  Shape 3 is the same under
 * rotation, so its center is effectively irrelevant; it has been chosen
 * so that it `sticks out' upward and leftward.  Except for shape 6,
 * all the blots are contained in a box going from (-1,-1) to (+1,+1);
 * shape 6's center `wobbles' as it rotates, so that while it `sticks out'
 * rightward, its rotation---a vertical line---`sticks out' downward.
 * The containment box has to include the offset (2,0), making the overall
 * containment box range from offset (-1,-1) to (+2,+1).  (This is why
 * there is only one row above, but two rows below, the display area.)
 *
 * The game works by choosing one of these shapes at random and putting
 * its center at the middle of the first display row (row 1, column 5).
 * The shape is moved steadily downward until it collides with something:
 * either  another shape, or the bottom of the board.  When the shape can
 * no longer be moved downwards, it is merged into the current board.
 * At this time, any completely filled rows are elided, and blots above
 * these rows move down to make more room.  A new random shape is again
 * introduced at the top of the board, and the whole process repeats.
 * The game ends when the new shape will not fit at (1,5).
 *
 * While the shapes are falling, the user can rotate them counterclockwise
 * 90 degrees (in addition to moving them left or right), provided that the
 * rotation puts the blots in empty spaces.  The table of shapes is set up
 * so that each shape contains the index of the new shape obtained by
 * rotating the current shape.  Due to symmetry, each shape has exactly
 * 1, 2, or 4 rotations total; the first 7 entries in the table represent
 * the primary shapes, and the remaining 12 represent their various
 * rotated forms.
 */
struct shape {
	int	rot;	/* index of rotated version of this shape */
	int	off[3];	/* offsets to other blots if center is at (0,0) */
};

extern struct shape shapes[];
#define	randshape() (&shapes[random() % 7])

/*
 * Shapes fall at a rate faster than once per second.
 *
 * The initial rate is determined by dividing 1 million microseconds
 * by the game `level'.  (This is at most 1 million, or one second.)
 * Each time the fall-rate is used, it is decreased a little bit,
 * depending on its current value, via the `faster' macro below.
 * The value eventually reaches a limit, and things stop going faster,
 * but by then the game is utterly impossible.
 */
long	fallrate;		/* less than 1 million; smaller => faster */
#define	faster() (fallrate -= fallrate / 3000)

/*
 * Game level must be between 1 and 9.  This controls the initial fall rate
 * and affects scoring.
 */
#define	MINLEVEL	1
#define	MAXLEVEL	9

/*
 * Scoring is as follows:
 *
 * When the shape comes to rest, and is integrated into the board,
 * we score one point.  If the shape is high up (at a low-numbered row),
 * and the user hits the space bar, the shape plummets all the way down,
 * and we score a point for each row it falls (plus one more as soon as
 * we find that it is at rest and integrate it---until then, it can
 * still be moved or rotated).
 */
int	score;			/* the obvious thing */

char	key_msg[100];

int	fits_in __P((struct shape *, int));
void	place __P((struct shape *, int, int));
void	stop __P((char *));
