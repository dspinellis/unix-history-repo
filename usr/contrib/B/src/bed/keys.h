/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: keys.h,v 2.3 85/08/22 16:04:38 timo Exp $ */

/*
 * B editor -- Function key and control character definitions.
 */


#define Ctl(x) ('x'&037)

/*
 * Commands bound to control characters.
 *
 * Not all control characters can be freely used:
 * ^Q and ^S are used by the Unix operating system
 * for output flow control, and ^Z is used by BSD
 * Unix systems for `job control'.
 *
 * Also note that ^H, ^I and ^M (and somtimes ^J) have their
 * own keys on most keyboards and thus usually have a strong
 * intuitive meaning.
 */

#define COPY	Ctl(C)
#define DELETE	Ctl(D)
#define GOTO	Ctl(G)
#define UNDO	Ctl(H)
#define ACCEPT	Ctl(I)		/* TAB */
#define NEWLINE	Ctl(J)
#define REDRAW	Ctl(L)
#define RETURN	Ctl(M)
#define RECORD	Ctl(R)
#define PLAYBACK	Ctl(P)
#define USEMACRO	PLAYBACK
#define SAVEMACRO	RECORD
#define REDO	Ctl(U)
#define EXIT	Ctl(X)


/*
 * Commands bound to ESC sequences.
 *
 * When 'inchar()' in "getc.c" sees an ESC-x sequence, it
 * will return (x&0177)|MASK.
 */

#define MASK 0200 /* Must fit in a character! */

#define WIDEN		('w'|MASK) /* so "\ew" is recognized as WIDEN */
#define NARROW		('f'|MASK) /* FIRST */
#define RNARROW		('l'|MASK) /* LAST */
#define EXTEND		('e'|MASK)

#define UPLINE		('u'|MASK)
#define PREVIOUS	('p'|MASK)
#define NEXT		('n'|MASK)
#define DOWNLINE	('d'|MASK)

#define LEFTARROW	(','|MASK)
#define RITEARROW	('.'|MASK)
#define UPARROW		('U'|MASK)
#define DOWNARROW	('D'|MASK)

#ifdef HELPFUL
#define HELP	('?'|MASK)
#endif HELPFUL
