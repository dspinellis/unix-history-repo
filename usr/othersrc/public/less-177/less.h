/*
 * Standard include file for "less".
 */

/*
 * Include the file of compile-time options.
 */
#include "defines.h"

/*
 * Language details.
 */
#if !VOID
#define	void  int
#endif
#define	public		/* PUBLIC FUNCTION */

/*
 * Special types and constants.
 */
typedef long		POSITION;
/*
 * {{ Warning: if POSITION is changed to other than "long",
 *    you may have to change some of the printfs which use "%ld"
 *    to print a variable of type POSITION. }}
 */

#define	NULL_POSITION	((POSITION)(-1))

/*
 * The type of an interrupt handler.
 */
#define	HANDLER		void

/*
 * An IFILE represents an input file.
 */
#define	IFILE		VOID_POINTER
#define	NULL_IFILE	((IFILE)NULL)

/*
 * The structure used to represent a "screen position".
 * This consists of a file position, and a screen line number.
 * The meaning is that the line starting at the given file
 * position is displayed on the ln-th line of the screen.
 * (Screen lines before ln are empty.)
 */
struct scrpos
{
	POSITION pos;
	int ln;
};

typedef union parg
{
	char *p_string;
	int p_int;
} PARG;

#define	NULL_PARG	((PARG *)NULL)

#define	EOI		(-1)

#ifndef NULL
#define	NULL		(0)
#endif

#define	READ_INTR	(-2)

/* How quiet should we be? */
#define	NOT_QUIET	0	/* Ring bell at eof and for errors */
#define	LITTLE_QUIET	1	/* Ring bell only for errors */
#define	VERY_QUIET	2	/* Never ring bell */

/* How should we prompt? */
#define	PR_SHORT	0	/* Prompt with colon */
#define	PR_MEDIUM	1	/* Prompt with message */
#define	PR_LONG		2	/* Prompt with longer message */

/* How should we handle backspaces? */
#define	BS_SPECIAL	0	/* Do special things for underlining and bold */
#define	BS_NORMAL	1	/* \b treated as normal char; actually output */
#define	BS_CONTROL	2	/* \b treated as control char; prints as ^H */

/* How should we search? */
#define	SRCH_FORW	0	/* Search forward from current position */
#define	SRCH_BACK	1	/* Search backward from current position */
#define	SRCH_NOMATCH	0100	/* Search for non-matching lines */
#define	SRCH_PAST_EOF	0200	/* Search past end-of-file, into next file */
#define	SRCH_FIRST_FILE	0400	/* Search starting at the first file */

#define	SRCH_DIR(t)	((t) & 01)
#define	SRCH_REVERSE(t)	((t) ^ 01)

/* Special chars used to tell put_line() to do something special */
#define	NORMAL		(0)
#define	UNDERLINE	(1)
#define	BOLD		(2)
#define	BLINK		(3)
#define	INVIS		(4)

#define	CONTROL(c)		((c)&037)
#define	ESC			CONTROL('[')

#define	SIGNAL(sig,func)	signal(sig,func)

/* Library function declarations */
offset_t lseek();
#define	BAD_LSEEK	((offset_t)-1)
VOID_POINTER calloc();

#define	ch_zero()	((POSITION)0)
#include "funcs.h"
