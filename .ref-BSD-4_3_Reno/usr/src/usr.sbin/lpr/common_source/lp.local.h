/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)lp.local.h	5.5 (Berkeley) 6/1/90
 */

/*
 * Possibly, local parameters to the spooling system
 */

/*
 * Magic number mapping for binary files, used by lpr to avoid
 *   printing objects files.
 */

#include <a.out.h>
#include <ar.h>

#ifndef A_MAGIC1	/* must be a VM/UNIX system */
#	define A_MAGIC1	OMAGIC
#	define A_MAGIC2	NMAGIC
#	define A_MAGIC3	ZMAGIC
#	undef ARMAG
#	define ARMAG	0177545
#endif

/*
 * Defaults for line printer capabilities data base
 */
#define	DEFLP		"lp"
#define DEFLOCK		"lock"
#define DEFSTAT		"status"
#define	DEFMX		1000
#define DEFMAXCOPIES	0
#define DEFFF		"\f"
#define DEFWIDTH	132
#define DEFLENGTH	66
#define DEFUID		1

/*
 * When files are created in the spooling area, they are normally
 *   readable only by their owner and the spooling group.  If you
 *   want otherwise, change this mode.
 */
#define FILMOD		0660

/*
 * Printer is assumed to support LINELEN (for block chars)
 *   and background character (blank) is a space
 */
#define LINELEN		132
#define BACKGND		' '

#define HEIGHT	9		/* height of characters */
#define WIDTH	8		/* width of characters */
#define DROP	3		/* offset to drop characters with descenders */

/*
 * Define TERMCAP if the terminal capabilites are to be used for lpq.
 */
#define TERMCAP

/*
 * Maximum number of user and job requests for lpq and lprm.
 */
#define MAXUSERS	50
#define MAXREQUESTS	50
