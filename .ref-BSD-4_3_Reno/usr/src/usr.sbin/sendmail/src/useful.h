/*
 * Copyright (c) 1988 Regents of the University of California.
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
 *	@(#)useful.h	4.6 (Berkeley) 6/1/90
 */

# include <sys/types.h>

/* support for bool type */
typedef char	bool;
# define TRUE	1
# define FALSE	0

# ifndef NULL
# define NULL	0
# endif NULL

/* bit hacking */
# define bitset(bit, word)	(((word) & (bit)) != 0)

/* some simple functions */
# ifndef max
# define max(a, b)	((a) > (b) ? (a) : (b))
# define min(a, b)	((a) < (b) ? (a) : (b))
# endif max

/* assertions */
# ifndef NASSERT
# define ASSERT(expr, msg, parm)\
	if (!(expr))\
	{\
		fprintf(stderr, "assertion botch: %s:%d: ", __FILE__, __LINE__);\
		fprintf(stderr, msg, parm);\
	}
# else NASSERT
# define ASSERT(expr, msg, parm)
# endif NASSERT

/* sccs id's */
# ifndef lint
# define SCCSID(arg)	static char SccsId[] = "arg";
# else lint
# define SCCSID(arg)
# endif lint

/* define the types of some common functions */
extern char	*strcpy(), *strncpy();
extern char	*strcat(), *strncat();
extern char	*malloc();
extern char	*index(), *rindex();
extern int	errno;
extern time_t	time();
extern char	*ctime();
extern char	*getenv();
