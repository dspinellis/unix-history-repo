/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)gomoku.h	8.1 (Berkeley) %G%
 */

#include <sys/types.h>

/* board dimensions */
#define BSZ	19
#define BSZ1	(BSZ+1)
#define BSZ2	(BSZ+2)
#define BAREA	(BSZ2*BSZ1+1)

/* frame dimentions (based on 5 in a row) */
#define FSZ1	BSZ
#define FSZ2	(BSZ-4)
#define FAREA	(FSZ1*FSZ2 + FSZ2*FSZ2 + FSZ1*FSZ2 + FSZ2*FSZ2)

#define MUP	(BSZ1)
#define MDOWN	(-BSZ1)
#define MLEFT	(-1)
#define MRIGHT	(1)

/* values for s_occ */
#define BLACK	0
#define WHITE	1
#define EMPTY	2
#define BORDER	3

/* return values for makemove() */
#define MOVEOK	0
#define RESIGN	1
#define ILLEGAL	2
#define WIN	3
#define TIE	4
#define SAVE	5

#define A 1
#define B 2
#define C 3
#define D 4
#define E 5
#define F 6
#define G 7
#define H 8
#define J 9
#define K 10
#define L 11
#define M 12
#define N 13
#define O 14
#define P 15
#define Q 16
#define R 17
#define S 18
#define T 19

#define PT(x,y)		((x) + BSZ1 * (y))

/*
 * A 'frame' is a group of five or six contiguous board locations.
 * An open ended frame is one with spaces on both ends; otherwise, its closed.
 * A 'combo' is a group of intersecting frames and consists of two numbers:
 * 'A' is the number of moves to make the combo non-blockable.
 * 'B' is the minimum number of moves needed to win once it can't be blocked.
 * A 'force' is a combo that is one move away from being non-blockable
 *
 * Single frame combo values:
 *     <A,B>	board values
 *	5,0	. . . . . O
 *	4,1	. . . . . .
 *	4,0	. . . . X O
 *	3,1	. . . . X .
 *	3,0	. . . X X O
 *	2,1	. . . X X .
 *	2,0	. . X X X O
 *	1,1	. . X X X .
 *	1,0	. X X X X O
 *	0,1	. X X X X .
 *	0,0	X X X X X O
 *
 * The rule for combining two combos (<A1,B1> <A2,B2>)
 * with V valid intersection points, is:
 *	A' = A1 + A2 - 2 - V
 *	B' = MIN(A1 + B1 - 1, A2 + B2 - 1)
 * Each time a frame is added to the combo, the number of moves to complete
 * the force is the number of moves needed to 'fill' the frame plus one at
 * the intersection point. The number of moves to win is the number of moves
 * to complete the best frame minus the last move to complete the force.
 * Note that it doesn't make sense to combine a <1,x> with anything since
 * it is already a force. Also, the frames have to be independent so a
 * single move doesn't affect more than one frame making up the combo.
 *
 * Rules for comparing which of two combos (<A1,B1> <A2,B2>) is better:
 * Both the same color:
 *	<A',B'> = (A1 < A2 || A1 == A2 && B1 <= B2) ? <A1,B1> : <A2,B2>
 *	We want to complete the force first, then the combo with the
 *	fewest moves to win.
 * Different colors, <A1,B1> is the combo for the player with the next move:
 *	<A',B'> = A2 <= 1 && (A1 > 1 || A2 + B2 < A1 + B1) ? <A2,B2> : <A1,B1>
 *	We want to block only if we have to (i.e., if they are one move away
 *	from completing a force and we don't have a force that we can
 *	complete which takes fewer or the same number of moves to win).
 */

#define MAXA		6
#define MAXB		2
#define MAXCOMBO	0x600
#define MAXDEPTH	5

union	combo {
	struct {
#if BYTE_ORDER == BIG_ENDIAN
		u_char	a;	/* # moves to complete force */
		u_char	b;	/* # moves to win */
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
		u_char	b;	/* # moves to win */
		u_char	a;	/* # moves to complete force */
#endif
	} c;
	u_short	s;
};

/*
 * This structure is used to record combinations of two more frames.
 * This is so we can do it incrementally in makemove() and because
 * we don't want to combine frames with <1,x> combos.
 */
struct combostr {
	struct combostr	*c_next;	/* list of combos at the same level */
	struct combostr	*c_prev;	/* list of combos at the same level */
	struct combostr	*c_link[2];	/* previous level or NULL if level 1 */
	union combo	c_combo;	/* combo value for this level */
	u_short		c_vertex;	/* intersection or frame head */
	u_char		c_nframes;	/* number of frames in the combo */
	u_char		c_dir;		/* which direction */
	u_char		c_flg;		/* combo flags */
	u_char		c_refcnt;	/* # higher levels that point to us */
};

/* flag values for s_flg */
#define C_LOOP		0x01		/* link[1] intersects previous frame */

struct	elist {
	struct elist	*e_next;	/* list of combos */
	struct combostr	*e_combo;	/* the combo */
	struct combostr	*e_frame;	/* the 1st level combo that connects */
};

/*
 * One spot structure for each location on the board.
 * A frame consists of the combination for the current spot plus the five spots
 * 0: right, 1: right & down, 2: down, 3: down & left.
 */
struct	spotstr {
	short		s_occ;		/* color of occupant */
	short		s_wval;		/* weighted value */
	int		s_flg;		/* flags for graph walks */
	union combo	s_fval[2][4];	/* combo value for [color][frame] */
	union combo	s_combo[2];	/* minimum combo value for BLK & WHT */
	u_char		s_level[2];	/* number of frames in the min combo */
	u_char		s_nforce[2];	/* number of <1,x> combos */
	struct combostr	*s_frame[4];	/* level 1 combo for frame[dir] */
	struct elist	*s_empty[2];	/* level n combo for [color] */
};

/* flag values for s_flg */
#define CFLAG		0x000001	/* frame is part of a combo */
#define CFLAGALL	0x00000F	/* all frame directions marked */
#define IFLAG		0x000010	/* legal intersection point */
#define IFLAGALL	0x0000F0	/* any intersection points? */
#define FFLAG		0x000100	/* frame is part of a <1,x> combo */
#define FFLAGALL	0x000F00	/* all force frames */
#define MFLAG		0x001000	/* frame has already been seen */
#define MFLAGALL	0x00F000	/* all frames seen */
#define BFLAG		0x010000	/* frame intersects border or dead */
#define BFLAGALL	0x0F0000	/* all frames dead */

extern	char	*letters;
extern	char	*color[];
extern	char	fmtbuf[];

extern	int     dd[4];
extern	struct	spotstr	board[BAREA];		/* info for board */
extern	struct	combostr frames[FAREA];		/* storage for all frames */
extern	struct	combostr *sortframes[2];	/* sorted, non-empty frames */
extern	char	overlap[FAREA * FAREA];		/* frame [a][b] overlap */
extern	short	intersect[FAREA * FAREA];	/* frame [a][b] intersection */
extern	int	movelog[BSZ * BSZ];		/* history of moves */
extern	int	movenum;
extern	int	debug;

extern	char    *copy();
extern	char    *stoc();
extern	char    *tail();

#define ASSERT(x)
