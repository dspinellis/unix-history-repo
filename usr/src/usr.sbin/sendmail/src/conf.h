/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.h	6.4 (Berkeley) %G%
 */

/*
**  CONF.H -- All user-configurable parameters for sendmail
*/

# include <sys/param.h>

/*
**  Table sizes, etc....
**	There shouldn't be much need to change these....
*/

# define MAXLINE	1024		/* max line length */
# define MAXNAME	256		/* max length of a name */
# define MAXPV		40		/* max # of parms to mailers */
# define MAXATOM	100		/* max atoms per address */
# define MAXMAILERS	25		/* maximum mailers known to system */
# define MAXRWSETS	100		/* max # of sets of rewriting rules */
# define MAXPRIORITIES	25		/* max values for Precedence: field */
# define MAXTRUST	30		/* maximum number of trusted users */
# define MAXMXHOSTS	20		/* max # of MX records */
# define SMTPLINELIM	990		/* maximum SMTP line length */
# define MAXKEY		128		/* maximum size of a database key */
# define MEMCHUNKSIZE	1024		/* chunk size for memory allocation */
# define MAXUSERENVIRON	100		/* max envariables saved */

# ifndef QUEUESIZE
# define QUEUESIZE	1000		/* max # of jobs per queue run */
# endif

# ifndef FORK
# define FORK		vfork		/* function to call to fork mailer */
# endif

/*
**  Compilation options.
**
**	#define these if they are available; comment them out otherwise.
*/

# define LOG		1	/* enable logging */
# define SMTP		1	/* enable user and server SMTP */
# define QUEUE		1	/* enable queueing */
# define UGLYUUCP	1	/* output ugly UUCP From lines */
# define DAEMON		1	/* include the daemon (requires IPC & SMTP) */
# define SETPROCTITLE	1	/* munge argv to display current status */
# define NAMED_BIND	1	/* use Berkeley Internet Domain Server */
# define MATCHGECOS	1	/* match user names from gecos field */

# ifdef NEWDB
# define USERDB		1	/* look in user database (requires NEWDB) */
# define BTREE_MAP	1	/* enable BTREE mapping type (requires NEWDB) */
# define HASH_MAP	1	/* enable HASH mapping type (requires NEWDB) */
# endif

# ifdef NDBM
# define DBM_MAP	1	/* enable DBM mapping type (requires NDBM) */
# endif

/*
**  Some general configuration -- you shouldn't have to touch these
*/

# ifdef hpux
# define SYSTEM5	1
# endif

# ifdef SYSTEM5

# define LOCKF		1	/* use System V lockf instead of flock */
# define SYS5TZ		1	/* use System V style timezones */
# define UNAME		1	/* use System V uname system call */

# endif

/*
**  Older systems don't have this error code -- it should be in
**  /usr/include/sysexits.h.
*/

# ifndef EX_CONFIG
# define EX_CONFIG	78	/* configuration error */
# endif

/*
**  Arrange to use either varargs or stdargs
*/

# ifdef __STDC__

# include <stdarg.h>

# define VA_LOCAL_DECL	va_list ap;
# define VA_START(f)	va_start(ap, f)
# define VA_END		va_end(ap)

# else

# include <varargs.h>

# define VA_LOCAL_DECL	va_list ap;
# define VA_START(f)	va_start(ap)
# define VA_END		va_end(ap)

# endif

#ifdef UNAME
# include <sys/utsname.h>
# ifdef newstr
#  undef newstr
# endif
#else /* ! UNAME */
# define NODE_LENGTH 32
struct utsname
{
	char nodename[NODE_LENGTH+1];
};
#endif /* UNAME */

#ifdef sun
# include <vfork.h>
#endif
