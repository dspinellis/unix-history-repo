/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
**
**	@(#)conf.h	5.1 (Berkeley) %G%
*/

/*
**  CONF.H -- All user-configurable parameters for sendmail
*/



/*
**  Table sizes, etc....
**	There shouldn't be much need to change these....
*/

# define MAXLINE	512		/* max line length */
# define MAXNAME	128		/* max length of a name */
# define MAXFIELD	2500		/* max total length of a hdr field */
# define MAXPV		40		/* max # of parms to mailers */
# define MAXHOP		30		/* max value of HopCount */
# define MAXATOM	100		/* max atoms per address */
# define MAXMAILERS	25		/* maximum mailers known to system */
# define MAXRWSETS	30		/* max # of sets of rewriting rules */
# define MAXPRIORITIES	25		/* max values for Precedence: field */
# define MAXTRUST	30		/* maximum number of trusted users */

/*
**  Compilation options.
*/

#define DBM		1	/* use DBM library (requires -ldbm) */
#define DEBUG		1	/* enable debugging */
#define LOG		1	/* enable logging */
#define SMTP		1	/* enable user and server SMTP */
#define QUEUE		1	/* enable queueing */
#define UGLYUUCP	1	/* output ugly UUCP From lines */
#define DAEMON		1	/* include the daemon (requires IPC) */
#define FLOCK		1	/* use flock file locking */
