/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)des_rw.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/param.h>
#include <kerberosIV/des.h>
#include <kerberosIV/krb.h>

extern long		random();
static unsigned char	des_inbuf[10240], storage[10240], *store_ptr;
static bit_64		*key;
static u_char		*key_schedule;

/*
 * NB: These routines will not function properly if NBIO
 * 	is set
 */

/*
 * des_set_key
 *
 * Set des encryption/decryption key for use by the des_read and
 * des_write routines
 *
 * The inkey parameter is actually the DES initial vector,
 * and the insched is the DES Key unwrapped for faster decryption
 */

void
des_set_key(inkey, insched)
	bit_64		*inkey;
	u_char		*insched;
{
	key = inkey;
	key_schedule = insched;
}

void
des_clear_key()
{
	bzero((char *) key, sizeof(C_Block));
	bzero((char *) key_schedule, sizeof(Key_schedule));
}
	

int
des_read(fd, buf, len)
	int fd;
	register char *buf;
	int len;
{
	int nreturned = 0;
	long net_len, rd_len;
	int nstored = 0;

	if (nstored >= len) {
		(void) bcopy(store_ptr, buf, len);
		store_ptr += len;
		nstored -= len;
		return(len);
	} else if (nstored) {
		(void) bcopy(store_ptr, buf, nstored);
		nreturned += nstored;
		buf += nstored;
		len -= nstored;
		nstored = 0;
	}
	
	if (krb_net_read(fd, &net_len, sizeof(net_len)) != sizeof(net_len)) {
		/* XXX can't read enough, pipe
		   must have closed */
		return(0);
	}
	net_len = ntohl(net_len);
	if (net_len <= 0 || net_len > sizeof(des_inbuf)) {
		/* preposterous length; assume out-of-sync; only
		   recourse is to close connection, so return 0 */
		return(0);
	}
	/* the writer tells us how much real data we are getting, but
	   we need to read the pad bytes (8-byte boundary) */
	rd_len = roundup(net_len, 8);
	if (krb_net_read(fd, des_inbuf, rd_len) != rd_len) {
		/* pipe must have closed, return 0 */
		return(0);
	}
	(void) des_pcbc_encrypt(des_inbuf,	/* inbuf */
			    storage,		/* outbuf */
			    net_len,		/* length */
			    key_schedule,	/* DES key */
			    key,		/* IV */
			    DECRYPT);		/* direction */

	if(net_len < 8)
		store_ptr = storage + 8 - net_len;
	else
		store_ptr = storage;

	nstored = net_len;
	if (nstored > len) {
		(void) bcopy(store_ptr, buf, len);
		nreturned += len;
		store_ptr += len;
		nstored -= len;
	} else {
		(void) bcopy(store_ptr, buf, nstored);
		nreturned += nstored;
		nstored = 0;
	}
	
	return(nreturned);
}

static	unsigned char des_outbuf[10240];	/* > longest write */

int
des_write(fd, buf, len)
	int fd;
	char *buf;
	int len;
{
	static	int	seeded = 0;
	static	char	garbage_buf[8];
	long net_len, garbage;

	if(len < 8) {
		if(!seeded) {
			seeded = 1;
			srandom((int) time((long *)0));
		}
		garbage = random();
		/* insert random garbage */
		(void) bcopy(&garbage, garbage_buf, MIN(sizeof(long),8));
		/* this "right-justifies" the data in the buffer */
		(void) bcopy(buf, garbage_buf + 8 - len, len);
	}
	/* pcbc_encrypt outputs in 8-byte (64 bit) increments */

	(void) des_pcbc_encrypt((len < 8) ? garbage_buf : buf,
			    des_outbuf,
			    (len < 8) ? 8 : len,
			    key_schedule,	/* DES key */
			    key,		/* IV */
			    ENCRYPT);

	/* tell the other end the real amount, but send an 8-byte padded
	   packet */
	net_len = htonl(len);
	(void) write(fd, &net_len, sizeof(net_len));
	(void) write(fd, des_outbuf, roundup(len,8));
	return(len);
}
