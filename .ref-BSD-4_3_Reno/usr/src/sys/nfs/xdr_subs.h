/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)xdr_subs.h	7.3 (Berkeley) 6/28/90
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

