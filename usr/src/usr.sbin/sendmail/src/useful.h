/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)useful.h	6.5 (Berkeley) %G%
 */

# include <sys/types.h>

/* support for ANSI prototypes (or not, as the case may be) */
#ifndef __P
# if defined(__STDC__) && defined(_FORGIVING_CC_)
#  define __P(protos)	protos
# else
#  define __P(protos)	()
# endif
#endif

/* support for bool type */
typedef char	bool;
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
# define SCCSID(arg)	static char SccsId[] = "arg";
# else
# define SCCSID(arg)
# endif
