/*
 * Copyright (c) 1988 Regents of the University of California.
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
 *	@(#)useful.h	4.4 (Berkeley) 6/29/88
 */

# ifndef makedev
# include <sys/types.h>
# endif

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
# ifndef V6
extern char	*getenv();
# endif V6
# ifndef VMUNIX
typedef unsigned short	u_short;
typedef long		u_long;
typedef char		u_char;
typedef int		void;
# endif VMUNIX
