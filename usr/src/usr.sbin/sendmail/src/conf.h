/*
 * Copyright (c) 1983, 1995 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.h	8.179 (Berkeley) %G%
 */

/*
**  CONF.H -- All user-configurable parameters for sendmail
**
**	Send updates to sendmail@CS.Berkeley.EDU so they will be
**	included in the next release.
*/

struct rusage;	/* forward declaration to get gcc to shut up in wait.h */

# include <sys/param.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/file.h>
# include <sys/wait.h>
# include <fcntl.h>
# include <signal.h>
# include <netdb.h>
# include <pwd.h>

/**********************************************************************
**  Table sizes, etc....
**	There shouldn't be much need to change these....
**********************************************************************/

# define MAXLINE	2048		/* max line length */
# define MAXNAME	256		/* max length of a name */
# define MAXPV		40		/* max # of parms to mailers */
# define MAXHOP		30		/* max value of HopCount */
# define MAXATOM	200		/* max atoms per address */
# define MAXMAILERS	25		/* maximum mailers known to system */
# define MAXRWSETS	200		/* max # of sets of rewriting rules */
# define MAXPRIORITIES	25		/* max values for Precedence: field */
# define MAXMXHOSTS	20		/* max # of MX records */
# define SMTPLINELIM	990		/* maximum SMTP line length */
# define MAXKEY		128		/* maximum size of a database key */
# define MEMCHUNKSIZE	1024		/* chunk size for memory allocation */
# define MAXUSERENVIRON	100		/* max envars saved, must be >= 3 */
# define MAXALIASDB	12		/* max # of alias databases */
# define MAXMAPSTACK	12		/* max # of stacked or sequenced maps */
# define MAXTOCLASS	8		/* max # of message timeout classes */
# define MAXMIMEARGS	20		/* max args in Content-Type: */
# define MAXMIMENESTING	20		/* max MIME multipart nesting */
# define QUEUESEGSIZE	1000		/* increment for queue size */

/**********************************************************************
**  Compilation options.
**
**	#define these if they are available; comment them out otherwise.
**********************************************************************/

