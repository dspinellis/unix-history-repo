/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)auth.h	8.1 (Berkeley) %G%
 */

/*
 * Copyright (C) 1990 by the Massachusetts Institute of Technology
 *
 * Export of this software from the United States of America is assumed
 * to require a specific license from the United States Government.
 * It is the responsibility of any person or organization contemplating
 * export to obtain such a license before exporting.
 *
 * WITHIN THAT CONSTRAINT, permission to use, copy, modify, and
 * distribute this software and its documentation for any purpose and
 * without fee is hereby granted, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of M.I.T. not be used in advertising or publicity pertaining
 * to distribution of the software without specific, written prior
 * permission.  M.I.T. makes no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 */

#ifndef	__AUTH__
#define	__AUTH__

#define	AUTH_REJECT	0	/* Rejected */
#define	AUTH_UNKNOWN	1	/* We don't know who he is, but he's okay */
#define	AUTH_OTHER	2	/* We know him, but not his name */
#define	AUTH_USER	3	/* We know he name */
#define	AUTH_VALID	4	/* We know him, and he needs no password */

#if	!defined(P)
#ifdef	__STDC__
#define P(x)	x
#else
#define P(x)	()
#endif
#endif

typedef struct XauthP {
	int	type;
	int	way;
	int	(*init) P((struct XauthP *, int));
	int	(*send) P((struct XauthP *));
	void	(*is) P((struct XauthP *, unsigned char *, int));
	void	(*reply) P((struct XauthP *, unsigned char *, int));
	int	(*status) P((struct XauthP *, char *, int));
	void	(*printsub) P((unsigned char *, int, unsigned char *, int));
} Authenticator;

#include "auth-proto.h"

extern auth_debug_mode;
#endif
