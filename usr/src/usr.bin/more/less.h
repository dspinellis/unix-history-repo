/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
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
 *
 *	@(#)less.h	5.1 (Berkeley) %G%
 */

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
 * The type of signal handler functions.
 * Usually int, although it should be void.
 */
typedef	int		HANDLER;


#define	FILENAME	128	/* Max size of a filename */

#define	EOI		(0)
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

/* Special chars used to tell put_line() to do something special */
#define	UL_CHAR		'\201'	/* Enter underline mode */
#define	UE_CHAR		'\202'	/* Exit underline mode */
#define	BO_CHAR		'\203'	/* Enter boldface mode */
#define	BE_CHAR		'\204'	/* Exit boldface mode */

#define	CONTROL(c)		((c)&037)
#define	SIGNAL(sig,func)	signal(sig,func)

/* Library function declarations */
offset_t lseek();
char *calloc();

#include "funcs.h"
