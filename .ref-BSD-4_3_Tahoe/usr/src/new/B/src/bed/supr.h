/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: supr.h,v 2.2 84/07/11 15:20:05 guido Exp $ */

/*
 * B editor -- Superstructure for fine focusing.
 */

/*
 * Interpretation of mode and s1, s2, s3:
 * WHOLE: whole node is the focus;
 * SUBSET: s1/2, s2/2 are first and last child number under focus;
 *         even means fixed text, odd means child node;
 * SUBRANGE: s1/2 is fixed text number; s2, s3 are 1st&last char;
 *         if s1 is odd, ditto for child which must be "text";
 * VHOLE: s1/2 is fixed text number; volatile hole before char s2;
 *         if s1 is odd, ditto for child which must be "text".
 * ATEND: a volatile hole just after the entire node.
 * ATBEGIN: ditto just before it.
 * SUBLIST: s3 indicates how many times downrite() bring us
 *         beyond the focus (i.e., the focus is the subtree below
 *         ep->focus EXCLUDING the subtree reached after s3 times
 *         downrite().  Note s3 > 0.
 * FHOLE: Like VHOLE but in Fixed text.
 *
 * It is assumed that if the focus is a substring of fixed text
 * (SUBRANGE, VHOLE), it does not begin or end with lay-out of spaces.
 */

#define WHOLE	'W'
#define SUBSET	'S'
#define SUBRANGE	'R'
#define VHOLE	'V'
#define ATEND	'E'
#define ATBEGIN	'B'
#define SUBLIST	'L'
#define FHOLE	'F'

typedef struct {
	path focus;
	char mode;
	char /*bool*/ copyflag;
	char /*bool*/ spflag;
	char /*bool*/ changed;
	short /*0..2*MAXCHILD+1*/ s1;
	short s2;
	short s3;
	short highest;
	value copybuffer; /* Actually, a queue */
	value oldmacro; /* A text */
	value newmacro; /* A text, too */
	int generation;
} environ;

#ifdef STRUCTASS
#define Emove(e1, e2) ((e2) = (e1))
#else !STRUCTASS
#define Emove(e1, e2) emove(&(e1), &(e2))
#endif !STRUCTASS
#define Ecopy(e1, e2) ecopy(&(e1), &(e2))
#define Erelease(e) erelease(&(e))
