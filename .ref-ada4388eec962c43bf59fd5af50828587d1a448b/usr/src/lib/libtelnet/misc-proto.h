/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)misc-proto.h	8.1 (Berkeley) %G%
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

#ifndef	__MISC_PROTO__
#define	__MISC_PROTO__

#if	!defined(P)
#ifdef	__STDC__
#define	P(x)	x
#else
#define	P(x)	()
#endif
#endif

void auth_encrypt_init P((char *, char *, char *, int));
void auth_encrypt_connect P((int));
void printd P((unsigned char *, int));

/*
 * These functions are imported from the application
 */
int net_write P((unsigned char *, int));
void net_encrypt P((void));
int telnet_spin P((void));
char *telnet_getenv P((char *));
char *telnet_gets P((char *, char *, int, int));
#endif
