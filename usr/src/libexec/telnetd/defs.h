/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)defs.h	5.5 (Berkeley) %G%
 */

/*
 * Telnet server defines
 */
#include <sys/types.h>
#include <sys/param.h>

#ifndef	BSD
# define	BSD 43
#endif

#if BSD > 43
#define	USE_TERMIO
#endif

#ifdef	CRAY
# define	NEWINIT
# define	SYSV_TERMIO
# define	NO_GETTYTAB
# define	signal sigset
#endif	/* CRAY */

#ifdef	SYSV_TERMIO
# define	USE_TERMIO
#endif

#include <sys/socket.h>
#ifndef	CRAY
#include <sys/wait.h>
#endif	/* CRAY */
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#include <netinet/in.h>

#include <arpa/telnet.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <netdb.h>
#include <syslog.h>
#ifndef	LOG_DAEMON
#define	LOG_DAEMON	0
#endif
#ifndef	LOG_ODELAY
#define	LOG_ODELAY	0
#endif
#include <ctype.h>
#include <string.h>

#ifndef	USE_TERMIO
#include <sgtty.h>
typedef unsigned char cc_t;
#else
# ifdef	SYSV_TERMIO
# include <termio.h>
# else
# include <termios.h>
# endif
#endif

#ifdef	CRAY
#include <sys/fcntl.h>
# ifdef	CRAY1
# include <sys/pty.h>
#  ifndef FD_ZERO
# include <sys/select.h>
#  endif /* FD_ZERO */
# endif	/* CRAY1 */

#include <memory.h>
#endif	/* CRAY */

#if	defined(TCSIG) || defined(TIOCPKT_IOCTL)
# define	LINEMODE
# define	KLUDGELINEMODE
#endif

#ifndef	FD_SET
#ifndef	HAVE_fd_set
typedef struct fd_set { int fds_bits[1]; } fd_set;
#endif

#define	FD_SET(n, p)	((p)->fds_bits[0] |= (1<<(n)))
#define	FD_CLR(n, p)	((p)->fds_bits[0] &= ~(1<<(n)))
#define	FD_ISSET(n, p)	((p)->fds_bits[0] & (1<<(n)))
#define FD_ZERO(p)	((p)->fds_bits[0] = 0)
#endif	/* FD_SET */

#define	OPT_NO			0		/* won't do this option */
#define	OPT_YES			1		/* will do this option */
#define	OPT_YES_BUT_ALWAYS_LOOK	2
#define	OPT_NO_BUT_ALWAYS_LOOK	3

/*
 * I/O data buffers defines
 */
#define	NETSLOP	64
#ifdef CRAY
#undef BUFSIZ
#define BUFSIZ  2048
#endif

#define	NIACCUM(c)	{   *netip++ = c; \
			    ncc++; \
			}

/* clock manipulations */
#define	settimer(x)	(clocks.x = ++clocks.system)
#define	sequenceIs(x,y)	(clocks.x < clocks.y)

/*
 * Linemode support states, in decreasing order of importance
 */
#define REAL_LINEMODE	0x02
#define KLUDGE_LINEMODE	0x01
#define NO_LINEMODE	0x00

/*
 * Structures of information for each special character function.
 */
typedef struct {
	unsigned char	flag;		/* the flags for this function */
	cc_t		val;		/* the value of the special character */
} slcent, *Slcent;

typedef struct {
	slcent		defset;		/* the default settings */
	slcent		current;	/* the current settings */
	cc_t		*sptr;		/* a pointer to the char in */
					/* system data structures */
} slcfun, *Slcfun;
