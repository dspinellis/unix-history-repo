/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
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
 *	@(#)xdr_subs.h	7.2 (Berkeley) %G%
 */

/*
 * Macros used for conversion to/from xdr representation by nfs...
 * These use the MACHINE DEPENDENT routines ntohl, htonl
 * As defined by "XDR: External Data Representation Standard" RFC1014
 */
/* From xdr to machine */
#define fxdr_unsigned(t, v)	((t)ntohl((long)(v)))
#define	fxdr_time(f, t)		{((struct timeval *)(t))->tv_sec=ntohl( \
				((struct timeval *)(f))->tv_sec); \
				((struct timeval *)(t))->tv_usec=ntohl( \
				((struct timeval *)(f))->tv_usec);}

/* from machine to xdr */
#define	txdr_unsigned(v)	(htonl((long)(v)))
#define	txdr_time(f, t)		{((struct timeval *)(t))->tv_sec=htonl( \
				((struct timeval *)(f))->tv_sec); \
				((struct timeval *)(t))->tv_usec=htonl( \
				((struct timeval *)(f))->tv_usec);}

