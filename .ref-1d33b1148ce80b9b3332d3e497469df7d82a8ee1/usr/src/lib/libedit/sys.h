/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sys.h	8.1 (Berkeley) %G%
 */

/*
 * sys.h: Put all the stupid compiler and system dependencies here...
 */
#ifndef _h_sys
#define _h_sys

#ifndef public
# define public		/* Externally visible functions/variables */
#endif

#ifndef private
# define private	static	/* Always hidden internals */
#endif

#ifndef protected
# define protected	/* Redefined from elsewhere to "static" */
			/* When we want to hide everything	*/
#endif

#include <sys/cdefs.h>

#ifndef _PTR_T
# define _PTR_T
# if __STDC__
typedef void* ptr_t;
# else
typedef char* ptr_t;
# endif
#endif

#ifndef _IOCTL_T
# define _IOCTL_T
# if __STDC__
typedef void* ioctl_t;
# else
typedef char* ioctl_t;
# endif
#endif

#include <stdio.h>
#define REGEXP

#ifdef SUNOS
# undef REGEXP
# include <malloc.h>
typedef void (*sig_t)__P((int));
# ifdef __GNUC__
/*
 * Broken hdrs.
 */
extern char    *getenv		__P((const char *));
extern int	fprintf		__P((FILE *, const char *, ...));
extern int	sigsetmask	__P((int));
extern int	sigblock	__P((int));
extern int	ioctl		__P((int, int, void *));
extern int	fputc		__P((int, FILE *));
extern int	fgetc		__P((FILE *));
extern int	fflush		__P((FILE *));
extern int	tolower		__P((int));
extern int	toupper		__P((int));
extern int	errno, sys_nerr;
extern char	*sys_errlist[];
extern void	perror		__P((const char *));
extern int	read		__P((int, const char*, int));
#  include <string.h>
#  define strerror(e)	sys_errlist[e]
# endif
# ifdef SABER
extern ptr_t    memcpy		__P((ptr_t, const ptr_t, size_t));
extern ptr_t    memset		__P((ptr_t, int, size_t));
# endif
extern char    *fgetline	__P((FILE *, int *));
#endif

#endif /* _h_sys */
