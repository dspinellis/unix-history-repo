/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
**
**	@(#)conf.h	5.1 (Berkeley) 6/7/85
*/

/*
**  CONF.H -- definitions of compilation flags needed everywhere.
**
**	This, together with conf.c, should be all the configuration
**	information needed.  This stuff could be in a makefile, but
**	we prefer to keep this file very small because it is different
**	on a number of machines.
**
*/


# define	DEBUG		/* turn on debugging information */

/* # define	NEWFTP		/* use new ftp reply codes */

/* # define	NOTUNIX		/* don't use Unix-style mail headers */

# ifdef INGRES
# define	LOG		/* turn on logging */
				/* this flag requires -lsys in makefile */
# endif INGRES

# ifdef VAX
# define	VFORK		/* use the vfork syscall */
# endif VAX

# ifndef V6
# define	DBM		/* use the dbm library */
				/* this flag requires -ldbm in makefile */
# endif V6
