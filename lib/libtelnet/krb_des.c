/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)krb_des.c	5.1 (Berkeley) 2/28/91";
#endif /* not lint */

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

#if	defined(AUTHENTICATE) && defined(ENCRYPT) && defined(KRBDES_ENCRYPT)
#include <arpa/telnet.h>
#include <stdio.h>
#ifdef	__STDC__
#include <stdlib.h>
#endif

#include "encrypt.h"
#include "key-proto.h"
#include "misc-proto.h"

extern encrypt_debug_mode;

static Block krbdes_key = { 0 };
static Schedule krbdes_sched = { 0 };
static Block input_feed = { 0 };
static Block output_feed = { 0 };
static Block temp_feed = { 0 };

static unsigned char str_feed[64] = { IAC, SB, TELOPT_ENCRYPT,
				      ENCRYPT_IS,
				      ENCTYPE_KRBDES, };

	void
krbdes_init(server)
	int server;
{
	bzero((void *)krbdes_key, sizeof(Block));
	bzero((void *)krbdes_sched, sizeof(Schedule));
	bzero((void *)input_feed, sizeof(Block));
	bzero((void *)output_feed, sizeof(Block));
}

#define	KRBDES_FEED_INIT	1
#define	KRBDES_FEED_VRFY	2
#define	KRBDES_FEED_FAIL	3
#define	KRBDES_FEED_OK		4
#define	KRBDES_FEED_VRFY_FAIL	5

/*
 * Returns:
 *	-1: some error.  Negotiation is done, encryption not ready.
 *	 0: Successful, initial negotiation all done.
 *	 1: successful, negotiation not done yet.
 *	 2: Not yet.  Other things (like getting the key from
 *	    Kerberos) have to happen before we can continue.
 */


#define	NOT_YET		2
#define	IN_PROGRESS	1
#define	SUCCESS		0
#define	FAILED		-1

int state[2] = { NOT_YET, NOT_YET };
#define	ENCRYPT_STATE 0
#define	DECRYPT_STATE 1

/*
 * This is where we keep track of the fact that there has been a
 * call to krbdes_start(), but we haven't gotten a valid key from
 * kerberos, so that when (if) it does come in, we can continue with
 * the initial encryption startup.
 */
static int need_start;

	int
krbdes_start(dir, server)
	int dir;
	int server;
{ 
	Block b;
	int x;
	unsigned char *p;

	switch (dir) {
	case DIR_DECRYPT:
		/*
		 * This is simply a request to have the other side
		 * start output (our input).  He will negotiate a
		 * feed so we need not look for it.
		 */
		if (!VALIDKEY(krbdes_key))
			return(state[DECRYPT_STATE]);
		if (state[DECRYPT_STATE] == NOT_YET)
			state[DECRYPT_STATE] = IN_PROGRESS;
		return(state[DECRYPT_STATE]);

	case DIR_ENCRYPT:
		if (!VALIDKEY(krbdes_key)) {
			need_start = 1;
			return(state[ENCRYPT_STATE]);
		}
		if (state[ENCRYPT_STATE] == IN_PROGRESS)
			return(IN_PROGRESS);
		if (VALIDKEY(output_feed))
			return(SUCCESS);
		if (encrypt_debug_mode)
			printf("Creating new feed\r\n");
		/*
		 * Create a random feed and send it over encrypted.
		 * The other side will decrypt it, xor in 'KRBDES'
		 * and then recrypt it and send it back.
		 */
		des_new_random_key(temp_feed);
		des_ecb_encrypt(temp_feed, b, krbdes_sched, 1);
		str_feed[3] = ENCRYPT_IS;
		p = str_feed + 5;
		*p++ = KRBDES_FEED_INIT;
		for (x = 0; x < sizeof(Block); ++x) {
			if ((*p++ = b[x]) == IAC)
				*p++ = IAC;
		}
		*p++ = IAC;
		*p++ = SE;
		printsub('>', &str_feed[2], p - &str_feed[2]);
		net_write(str_feed, p - str_feed);
		temp_feed[0] ^= 'K';
		temp_feed[1] ^= 'R';
		temp_feed[2] ^= 'B';
		temp_feed[3] ^= 'D';
		temp_feed[4] ^= 'E';
		temp_feed[5] ^= 'S';
		state[ENCRYPT_STATE] = IN_PROGRESS;
		return(IN_PROGRESS);
	default:
		return(FAILED);
	}
}

/*
 * Returns:
 *	-1: some error.  Negotiation is done, encryption not ready.
 *	 0: Successful, initial negotiation all done.
 *	 1: successful, negotiation not done yet.
 */

	int
krbdes_is(data, cnt)
	unsigned char *data;
	int cnt;
{
	int x;
	unsigned char *p;
	unsigned char *why = 0;
	Block b;

	if (cnt-- < 1) {
		why = (unsigned char *)"Bad IS suboption";
		goto failure;
	}

	switch (*data++) {
	case KRBDES_FEED_INIT:
		if (cnt != sizeof(Block)) {
			if (encrypt_debug_mode)
				printf("Use Feed failed on size\r\n");
			why = (unsigned char *)"Bad block size";
			goto failure;
		}
		if (!VALIDKEY(krbdes_key)) {
			if (encrypt_debug_mode)
				printf("Use Feed failed on key\r\n");
			why = (unsigned char *)"Invalid key";
			goto failure;
		}
		if (encrypt_debug_mode)
			printf("Have a Use Feed\r\n");

		bcopy((void *)data, (void *)b, sizeof(Block));
		des_ecb_encrypt(b, input_feed, krbdes_sched, 0);

		input_feed[0] ^= 'K';
		input_feed[1] ^= 'R';
		input_feed[2] ^= 'B';
		input_feed[3] ^= 'D';
		input_feed[4] ^= 'E';
		input_feed[5] ^= 'S';

		des_ecb_encrypt(input_feed, b, krbdes_sched, 1);
		str_feed[3] = ENCRYPT_REPLY;
		p = str_feed + 5;
		*p++ = KRBDES_FEED_VRFY;
		for (x = 0; x < sizeof(Block); ++x) {
			if ((*p++ = b[x]) == IAC)
				*p++ = IAC;
		}
		*p++ = IAC;
		*p++ = SE;
		printsub('>', &str_feed[2], p - &str_feed[2]);
		net_write(str_feed, p - str_feed);
		if (encrypt_debug_mode)
			printf("Initializing Decrypt stream\r\n");
		key_stream_init(krbdes_key, input_feed, DIR_DECRYPT);
		state[DECRYPT_STATE] = IN_PROGRESS;
		return(IN_PROGRESS);

	case KRBDES_FEED_OK:
		state[DECRYPT_STATE] = SUCCESS;
		return(SUCCESS);

	case KRBDES_FEED_FAIL:
		state[DECRYPT_STATE] = FAILED;
		return(FAILED);

	default:
		if (encrypt_debug_mode) {
			printf("Unknown option type: %d\r\n", data[-1]);
			printd(data, cnt);
			printf("\r\n");
		}
		why = (unsigned char *)"Unknown sub-suboption";
	failure:
		/*
		 * We failed.  Send an empty KRBDES_FEED_VRFY option
		 * to the other side so it will know that things
		 * failed.
		 */
		str_feed[3] = ENCRYPT_REPLY;
		p = str_feed + 5;
		*p++ = KRBDES_FEED_VRFY_FAIL;
		if (why) {
			while (*why) {
				if ((*p++ = *why++) == IAC)
					*p++ = IAC;
			}
		}
		*p++ = IAC;
		*p++ = SE;
		printsub('>', &str_feed[2], p - &str_feed[2]);
		net_write(str_feed, p - str_feed);
		state[DECRYPT_STATE] = FAILED;
		return(FAILED);
	}
}

/*
 * Returns:
 *	-1: some error.  Negotiation is done, encryption not ready.
 *	 0: Successful, initial negotiation all done.
 *	 1: successful, negotiation not done yet.
 */

	int
krbdes_reply(data, cnt)
	unsigned char *data;
	int cnt;
{
	int x;
	unsigned char *p;
	unsigned char *why = 0;
	Block b;

	if (cnt-- < 1) {
		why = (unsigned char *)"Bad REPLY suboption";
		goto failure;
	}

	switch (*data++) {
	case KRBDES_FEED_VRFY:
		if (cnt != sizeof(Block)) {
			if (encrypt_debug_mode)
				printf("Have Feed failed on size\r\n");
			why = (unsigned char *)"Bad block size";
			goto failure;
		}
		if (!VALIDKEY(krbdes_key)) {
			if (encrypt_debug_mode)
				printf("Have Feed failed on key\r\n");
			why = (unsigned char *)"Invalid key";
			goto failure;
		}
		if (encrypt_debug_mode)
			printf("Have a Test Feed\r\n");

		bcopy((void *)data, (void *)b, sizeof(Block));
		des_ecb_encrypt(b, output_feed, krbdes_sched, 0);
		if (!SAMEKEY(output_feed, temp_feed)) {
			if (encrypt_debug_mode)
				printf("Have Feed failed on challange\r\n");
			bzero((void *)output_feed, sizeof(Block));
			why = (unsigned char*)"Challange decrypt failed";
			goto failure;
		}
		if (encrypt_debug_mode)
			printf("Initializing Encrypt stream\r\n");
		key_stream_init(krbdes_key, output_feed, DIR_ENCRYPT);

		str_feed[3] = ENCRYPT_IS;
		p = str_feed + 5;
		*p++ = KRBDES_FEED_OK;
		*p++ = IAC;
		*p++ = SE;
		printsub('>', &str_feed[2], p - &str_feed[2]);
		net_write(str_feed, p - str_feed);
		state[ENCRYPT_STATE] = SUCCESS;
		return(SUCCESS);

	default:
		if (encrypt_debug_mode) {
			printf("Unknown option type: %d\r\n", data[-1]);
			printd(data, cnt);
			printf("\r\n");
		}
		why = (unsigned char *)"Unknown sub-suboption";
	failure:
		str_feed[3] = ENCRYPT_IS;
		p = str_feed + 5;
		*p++ = KRBDES_FEED_FAIL;
		if (why) {
			while (*why) {
				if ((*p++ = *why++) == IAC)
					*p++ = IAC;
			}
		}
		*p++ = IAC;
		*p++ = SE;
		printsub('>', &str_feed[2], p - &str_feed[2]);
		net_write(str_feed, p - str_feed);
		state[ENCRYPT_STATE] = FAILED;
		return(FAILED);
	}
}

	void
krbdes_encrypt(s, c)
	unsigned char *s;
	int c;
{
	while (c-- > 0) {
		*s = key_stream(DIR_ENCRYPT, *s);
		++s;
	}
}

	int
krbdes_decrypt(c)
	int c;
{
	return(key_stream(DIR_DECRYPT, c));
#ifdef	ndef

	if (c == -1) {
		++use;
		return(0);
	}
	if (use) {
		use = 0;
	} else {
		last = key_stream(DIR_DECRYPT, c);
	}

	return(last ^ c);
#endif
}

	void
krbdes_session(key, server)
	Session_Key *key;
	int server;
{
	static once = 1;

	if (!key || key->type != SK_DES) {
		if (encrypt_debug_mode)
			printf("Can't set krbdes's session key (%d != %d)\r\n",
				key ? key->type : -1, SK_DES);
		return;
	}
	bcopy((void *)key->data, (void *)krbdes_key, sizeof(Block));
	if (once) {
		des_set_random_generator_seed(krbdes_key);
		once = 0;
	}
	des_key_sched(krbdes_key, krbdes_sched);
	/*
	 * Now look to see if krbdes_start() was was waiting for
	 * the key to show up.  If so, go ahead an call it now
	 * that we have the key.
	 */
	if (need_start) {
		krbdes_start(DIR_ENCRYPT, server);
		need_start = 0;
	}
}

	void
krbdes_printsub(data, cnt, buf, buflen)
	unsigned char *data, *buf;
	int cnt, buflen;
{
	char lbuf[32];
	register int i;
	char *cp;

	buf[buflen-1] = '\0';		/* make sure it's NULL terminated */
	buflen -= 1;

	switch(data[2]) {
	case KRBDES_FEED_OK:
		cp = "FEED_OK";
		goto common1;
	case KRBDES_FEED_FAIL:
		cp = "FEED_FAIL";
	common1:
		for (; (buflen > 0) && (*buf = *cp++); buf++)
			buflen--;
		if (cnt > 3) {
			if (buflen <= 0)
				break;
			*buf++ = ' ';
			if (--buflen <= 0)
				break;
			*buf++ = '"';
			for (i = 3; i < cnt && buflen > 0; i++, buflen--)
				*buf++ = data[i];
			if (buflen <= 0)
				break;
			*buf++ = '"';
			--buflen;
		}
		if (buflen <= 0)
			break;
		*buf++ = '\0';
		break;
		
	case KRBDES_FEED_INIT:
		cp = "FEED_INIT ";
		goto common2;

	case KRBDES_FEED_VRFY:
		cp = "FEED_VRFY ";
		goto common2;

	default:
		sprintf(lbuf, " %d (unknown)", data[2]);
		cp = lbuf;
	common2:
		for (; (buflen > 0) && (*buf = *cp++); buf++)
			buflen--;
		for (i = 3; i < cnt; i++) {
			sprintf(lbuf, " %d", data[i]);
			for (cp = lbuf; (buflen > 0) && (*buf = *cp++); buf++)
				buflen--;
		}
		break;
	}
}

static	Block stream_output[2];
static	Block stream_feed[2];
static	Schedule stream_sched[2];
static	int stream_index[2];

	void
key_stream_init(key, seed, dir)
	Block key;
	Block seed;
	int dir;
{
	des_key_sched(key, stream_sched[--dir]);
	bcopy((void *)seed, (void *)stream_output[dir], sizeof(Block));
	stream_index[dir] = sizeof(Block);
}

	unsigned char
key_stream(dir, data)
	int dir;
	int data;	/* data de/encrypting */
{
	int index;

	if (data == -1) {
		/* Backpeddle */
		if (stream_index[--dir])
			--stream_index[dir];
		return(0);
	}
	if ((index = stream_index[--dir]++) == sizeof(Block)) {
		Block b;
		des_ecb_encrypt(stream_output[dir],
				b,
				stream_sched[dir], 1);
		bcopy((void *)b, (void *)stream_feed[dir], sizeof(Block));
		stream_index[dir] = 1;	/* Next time will be 1 */
		index = 0;		/* But now use 0 */
	}

	/*
	 * On encryption, we store (feed ^ data) which is cypher
	 * On decryption we store (data) which is cypher
	 */
	if (dir == DIR_DECRYPT)
		return(stream_output[dir][index] =
		        (stream_feed[dir][index] ^ data));
	else
		return((stream_output[dir][index] = data) ^
		          stream_feed[dir][index]);
}
#endif
