/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)useful.h	8.3 (Berkeley) %G%
 */

# include <sys/types.h>

/* support for bool type */
typedef int	bool;
# define TRUE	1
# define FALSE	0

# ifndef NULL
# define NULL	0
# endif /* NULL */

/* bit hacking */
# define bitset(bit, word)	(((word) & (bit)) != 0)

/* some simple functions */
# ifndef max
# define max(a, b)	((a) > (b) ? (a) : (b))
# define min(a, b)	((a) < (b) ? (a) : (b))
# endif

/* assertions */
# ifndef NASSERT
# define ASSERT(expr, msg, parm)\
	if (!(expr))\
	{\
		fprintf(stderr, "assertion botch: %s:%d: ", __FILE__, __LINE__);\
		fprintf(stderr, msg, parm);\
	}
# else /* NASSERT */
# define ASSERT(expr, msg, parm)
# endif /* NASSERT */

/* sccs id's */
# ifndef lint
#  ifdef __STDC__
#   define SCCSID(arg)	static char SccsId[] = #arg;
#  else
#   define SCCSID(arg)	static char SccsId[] = "arg";
#  endif
# else
#  define SCCSID(arg)
# endif
