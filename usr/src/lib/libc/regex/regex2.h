/*-
 * Copyright (c) 1992 Henry Spencer.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)regex2.h	5.1 (Berkeley) %G%
 */

/*
 * internals of regex_t
 */
#define	MAGIC1	((('r'^0200)<<8) | 'e')

/*
 * The internal representation is a *strip*, a sequence of
 * operators ending with an endmarker.  (Some terminology etc. is a
 * historical relic of earlier versions which used multiple strips.)
 * Certain oddities in the representation are there to permit running
 * the machinery backwards; in particular, any deviation from sequential
 * flow must be marked at both its source and its destination.  Some
 * fine points:
 *
 * - OPLUS_ and O_PLUS are *inside* the loop they create.
 * - OQUEST_ and O_QUEST are *outside* the bypass they create.
 * - OCH_ and O_CH are *outside* the multi-way branch they create, while
 *   OOR1 and OOR2 are respectively the end and the beginning of one of
 *   the branches.  Note that there is an implicit OOR2 following OCH_
 *   and an implicit OOR1 preceding O_CH.
 *
 * In state representations, an operator's bit is on to signify a state
 * immediately *preceding* "execution" of that operator.
 */
typedef unsigned long sop;	/* strip operator */
typedef long sopno;
#define	OPRMASK	0xf8000000
#define	OPDMASK	0x07ffffff
#define	OPSHIFT	27
#define	OP(n)	((n)&OPRMASK)
#define	OPND(n)	((n)&OPDMASK)
#define	SOP(op, opnd)	((op)|(opnd))
/* operators			   meaning	operand			*/
/*						(back, fwd are offsets)	*/
#define	OEND	(1<<OPSHIFT)	/* endmarker	-			*/
#define	OCHAR	(2<<OPSHIFT)	/* character	unsigned char		*/
#define	OBOL	(3<<OPSHIFT)	/* left anchor	-			*/
#define	OEOL	(4<<OPSHIFT)	/* right anchor	-			*/
#define	OANY	(5<<OPSHIFT)	/* .		-			*/
#define	OANYOF	(6<<OPSHIFT)	/* [...]	set number		*/
#define	OBACK_	(7<<OPSHIFT)	/* begin \d	paren number		*/
#define	O_BACK	(8<<OPSHIFT)	/* end \d	paren number		*/
#define	OPLUS_	(9<<OPSHIFT)	/* + prefix	fwd to suffix		*/
#define	O_PLUS	(10<<OPSHIFT)	/* + suffix	back to prefix		*/
#define	OQUEST_	(11<<OPSHIFT)	/* ? prefix	fwd to suffix		*/
#define	O_QUEST	(12<<OPSHIFT)	/* ? suffix	back to prefix		*/
#define	OLPAREN	(13<<OPSHIFT)	/* (		fwd to )		*/
#define	ORPAREN	(14<<OPSHIFT)	/* )		back to (		*/
#define	OCH_	(15<<OPSHIFT)	/* begin choice	fwd to OOR2		*/
#define	OOR1	(16<<OPSHIFT)	/* | pt. 1	back to OOR1 or OCH_	*/
#define	OOR2	(17<<OPSHIFT)	/* | pt. 2	fwd to OOR2 or O_CH	*/
#define	O_CH	(18<<OPSHIFT)	/* end choice	back to OOR1		*/

/*
 * Structure for [] character-set representation.  Character sets are
 * done as bit vectors, grouped 8 to a byte vector for compactness.
 * The individual set therefore has both a pointer to the byte vector
 * and a mask to pick out the relevant bit of each byte.  A hash code
 * simplifies testing whether two sets could be identical.
 *
 * This will get trickier for multicharacter collating elements.  As
 * preliminary hooks for dealing with such things, we also carry along
 * a string of multi-character elements, and decide the size of the
 * vectors at run time.
 */
typedef struct {
	uchar *ptr;		/* -> uchar [csetsize] */
	uchar mask;		/* bit within array */
	uchar hash;		/* hash code */
	size_t smultis;
	uchar *multis;		/* -> uchar[smulti]  ab\0cd\0ef\0\0 */
} cset;
/* note that CHadd and CHsub are unsafe, and CHIN doesn't yield 0/1 */
#define	CHadd(cs, c)	((cs)->ptr[(c)] |= (cs)->mask, (cs)->hash += (c))
#define	CHsub(cs, c)	((cs)->ptr[(c)] &= ~(cs)->mask, (cs)->hash -= (c))
#define	CHIN(cs, c)	((cs)->ptr[(c)] & (cs)->mask)
#define	MCadd(cs, cp)	mcadd(p, cs, cp)	/* regcomp() internal fns */
#define	MCsub(cs, cp)	mcsub(p, cs, cp)
#define	MCin(cs, cp)	mcin(p, cs, cp)

/*
 * main compiled-expression structure
 */
struct re_guts {
	int magic;
#		define	MAGIC2	((('R'^0200)<<8)|'E')
	sop *strip;		/* malloced area for strip */
	int csetsize;		/* number of bits in a cset vector */
	int ncsets;		/* number of csets in use */
	cset *sets;		/* -> cset [ncsets] */
	uchar *setbits;		/* -> uchar[csetsize][ncsets/CHAR_BIT] */
	int cflags;		/* copy of regcomp() cflags argument */
	sopno nstates;		/* = number of sops */
	sopno firststate;	/* the initial OEND (normally 0) */
	sopno laststate;	/* the final OEND */
	int iflags;		/* internal flags */
#		define	USEBOL	01	/* used ^ */
#		define	USEEOL	02	/* used $ */
#		define	BAD	04	/* something wrong */
	int ncategories;	/* how many character categories */
	uchar *categories;	/* -> uchar[NUC] */
	char *must;		/* match must contain this string */
	int mlen;		/* length of must */
	size_t nsub;		/* copy of re_nsub */
	int backrefs;		/* does it use back references? */
	sopno nplus;		/* how deep does it nest +s? */
};
