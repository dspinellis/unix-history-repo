/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)lp.local.h	5.3 (Berkeley) %G%
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
#define	DEFSPOOL	"/usr/spool/lpd"
#define	DEFDAEMON	"/usr/lib/lpd"
#define	DEFLOGF		"/dev/console"
#define	DEFDEVLP	"/dev/lp"
#define DEFRLPR		"/usr/lib/rlpr"
#define DEFBINDIR	"/usr/ucb"
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
 * path name of files created by lpd.
 */
#define MASTERLOCK "/usr/spool/lpd.lock"
#define SOCKETNAME "/dev/printer"

/*
 * Some utilities used by printjob.
 */
#define PR		"/bin/pr"
#define MAIL		"/usr/lib/sendmail"

/*
 * Define TERMCAP if the terminal capabilites are to be used for lpq.
 */
#define TERMCAP

/*
 * Maximum number of user and job requests for lpq and lprm.
 */
#define MAXUSERS	50
#define MAXREQUESTS	50
