/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)kerberos.c	5.1 (Berkeley) %G%";
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

#ifdef	KRB4
#include <sys/types.h>
#include <arpa/telnet.h>
#include <stdio.h>
#if	defined(ENCRYPT)
#define	__NEED_ENCRYPT__
#undef	ENCRYPT
#endif
#include <des.h>        /* BSD wont include this in krb.h, so we do it here */
#include <krb.h>
#if	defined(__NEED_ENCRYPT__) && !defined(ENCRYPT)
#define	ENCRYPT
#undef	__NEED_ENCRYPT__
#endif
#ifdef	__STDC__
#include <stdlib.h>
#endif
#ifdef	NO_STRING_H
#include <strings.h>
#else
#include <string.h>
#endif

#include "encrypt.h"
#include "auth.h"
#include "misc.h"

int cksum P((unsigned char *, int));
int krb_mk_req P((KTEXT, char *, char *, char *, u_long));
int krb_rd_req P((KTEXT, char *, char *, u_long, AUTH_DAT *, char *));
int krb_kntoln P((AUTH_DAT *, char *));
int krb_get_cred P((char *, char *, char *, CREDENTIALS *));
int krb_get_lrealm P((char *, int));
int kuserok P((AUTH_DAT *, char *));

extern auth_debug_mode;

static unsigned char str_data[1024] = { IAC, SB, TELOPT_AUTHENTICATION, 0,
			  		AUTHTYPE_KERBEROS_V4, };

#define	KRB_AUTH	0		/* Authentication data follows */
#define	KRB_REJECT	1		/* Rejected (reason might follow) */
#define	KRB_ACCEPT	2		/* Accepted (name might follow) */
#define	KRB_NEWKEY	3		/* A new session key follows */
#define	KRB_NAME	4		/* Name to authenticate for */

static	KTEXT_ST auth;
static	char name[ANAME_SZ];
static	AUTH_DAT adat = { 0 };
#if	defined(ENCRYPT)
static Block	session_key	= { 0 };
#endif

	static int
Data(type, d, c)
	int type;
	void *d;
	int c;
{
        unsigned char *p = str_data + 6;
	unsigned char *cd = (unsigned char *)d;

	if (c == -1)
		c = strlen((char *)cd);

        if (auth_debug_mode) {
                printf("%s:%d: [%d] (%d)",
                        str_data[3] == TELQUAL_IS ? ">>>IS" : ">>>REPLY",
                        str_data[3],
                        type, c);
                printd(d, c);
                printf("\r\n");
        }
	*p++ = type;
        while (c-- > 0) {
                if ((*p++ = *cd++) == IAC)
                        *p++ = IAC;
        }
        *p++ = IAC;
        *p++ = SE;
	if (str_data[3] == TELQUAL_IS)
		printsub('>', &str_data[2], p - (&str_data[2]));
        return(net_write(str_data, p - str_data));
}

	int
kerberos4_init(ap, server)
	Authenticator *ap;
	int server;
{
	if (server)
		str_data[3] = TELQUAL_REPLY;
	else
		str_data[3] = TELQUAL_IS;
	str_data[4] = ap->type;
	str_data[5] = ap->way;
	return(1);
}

char dst_realm_buf[REALM_SZ], *dest_realm = NULL;
int dst_realm_sz = REALM_SZ;

	int
kerberos4_send(ap)
	Authenticator *ap;
{
	KTEXT_ST auth;
	Block enckey;
	char instance[INST_SZ];
	char *realm;
	char *krb_realmofhost();
	char *krb_get_phost();
	CREDENTIALS cred;
#if	defined(ENCRYPT)
	Schedule krb_sched;
#endif
	int r;
	
	if (!UserNameRequested) {
		if (auth_debug_mode) {
			printf("Kerberos V4: no user name supplied\r\n");
		}
		return(0);
	}

	bzero(instance, sizeof(instance));
	if (realm = krb_get_phost(RemoteHostName))
		strncpy(instance, realm, sizeof(instance));
	instance[sizeof(instance)-1] = '\0';

	realm = dest_realm ? dest_realm : krb_realmofhost(RemoteHostName);
	if (!realm) {
		if (auth_debug_mode) {
			printf("Kerberos V4: no realm for %s\r\n", RemoteHostName);
		}
		return(0);
	}
	if (r = krb_mk_req(&auth, "rcmd", instance, realm, 0L)) {
		if (auth_debug_mode) {
			printf("mk_req failed: %s\r\n", krb_err_txt[r]);
		}
		return(0);
	}
	if (r = krb_get_cred("rcmd", instance, realm, &cred)) {
		if (auth_debug_mode) {
			printf("get_cred failed: %s\r\n", krb_err_txt[r]);
		}
		return(0);
	}
	if (!Data(KRB_NAME, (void *)UserNameRequested, -1)) {
		if (auth_debug_mode)
			printf("Not enough room for user name\r\n");
		return(0);
	}
	if (auth_debug_mode)
		printf("Sent %d bytes of authentication data\r\n", auth.length);
	if (!Data(KRB_AUTH, (void *)auth.dat, auth.length)) {
		if (auth_debug_mode)
			printf("Not enough room for authentication data\r\n");
		return(0);
	}
#if	defined(ENCRYPT)
	des_key_sched(cred.session, krb_sched);
	des_set_random_generator_seed(cred.session);
	des_new_random_key(session_key);
	des_ecb_encrypt(session_key, enckey, krb_sched, 1);
	Data(KRB_NEWKEY, (void *)enckey, sizeof(enckey));
#endif
	
	if (auth_debug_mode) {
		printf("CK: %d:", cksum(auth.dat, auth.length));
		printd(auth.dat, auth.length);
		printf("\r\n");
		printf("Sent Kerberos V4 credentials to server\r\n");
	}
	return(1);
}

	void
kerberos4_is(ap, data, cnt)
	Authenticator *ap;
	unsigned char *data;
	int cnt;
{
	Session_Key skey;
	Block datablock;
	char realm[REALM_SZ];
	char instance[INST_SZ];
	Schedule sched;
	int r;

	if (cnt-- < 1)
		return;
	switch (*data++) {
	case KRB_NAME: {
		char user[256];

		if (cnt > 255)
			cnt = 255;
		strncpy(user, (char *)data, cnt);
		user[cnt] = 0;
		auth_encrypt_user(user);
		return;
	    }
	case KRB_AUTH:
		if (krb_get_lrealm(realm, 1) != KSUCCESS) {
			Data(KRB_REJECT, (void *)"No local V4 Realm.", -1);
			auth_finished(ap, AUTH_REJECT);
			if (auth_debug_mode)
				printf("No local realm\r\n");
			return;
		}
		bcopy((void *)data, (void *)auth.dat, auth.length = cnt);
		if (auth_debug_mode) {
			printf("Got %d bytes of authentication data\r\n", cnt);
			printf("CK: %d:", cksum(auth.dat, auth.length));
			printd(auth.dat, auth.length);
			printf("\r\n");
		}
		instance[0] = '*'; instance[1] = 0;
		if (r = krb_rd_req(&auth, "rcmd", instance, 0, &adat, "")) {
			if (auth_debug_mode)
				printf("Kerberos failed him as %s\r\n", name);
			Data(KRB_REJECT, (void *)krb_err_txt[r], -1);
			auth_finished(ap, AUTH_REJECT);
			return;
		}
		bcopy((void *)adat.session, (void *)session_key, sizeof(Block));
		krb_kntoln(&adat, name);
		Data(KRB_ACCEPT, (void *)name, -1);
		auth_finished(ap, AUTH_USER);
		if (auth_debug_mode) {
			printf("Kerberos accepting him as %s\r\n", name);
		}
		return;
	case KRB_NEWKEY:
#if	defined(ENCRYPT)
		if (VALIDKEY(session_key)) {
			des_key_sched(session_key, sched);
			bcopy((void *)data, (void *)datablock, sizeof(Block));
			des_ecb_encrypt(datablock, session_key, sched, 0);
			skey.type = SK_DES;
			skey.length = 8;
			skey.data = session_key;
			encrypt_session_key(&skey, 1);
		}
#endif
		return;
	default:
		if (auth_debug_mode)
			printf("Unknown Kerberos option %d\r\n", data[-1]);
		Data(KRB_REJECT, 0, 0);
		return;
	}
}

	void
kerberos4_reply(ap, data, cnt)
	Authenticator *ap;
	unsigned char *data;
	int cnt;
{
	Session_Key skey;

	if (cnt-- < 1)
		return;
	switch (*data++) {
	case KRB_REJECT:
		if (cnt > 0) {
			printf("[ Kerberos V4 refuses authentication because %.*s ]\r\n",
				cnt, data);
		} else
			printf("[ Kerberos V4 refuses authentication ]\r\n");
		auth_send_retry();
		return;
	case KRB_ACCEPT:
		if (cnt > 0) {
			printf("[ Kerberos V4 accepts you as %.*s ]\n", cnt, data);
		} else
			printf("[ Kerberos V4 accepts you ]\n", cnt, data);
#if	defined(ENCRYPT)
		skey.type = SK_DES;
		skey.length = 8;
		skey.data = session_key;
		encrypt_session_key(&skey, 0);
#endif
		auth_finished(ap, AUTH_USER);
		return;
	default:
		if (auth_debug_mode)
			printf("Unknown Kerberos option %d\r\n", data[-1]);
		return;
	}
}

	int
kerberos4_status(ap, name, level)
	Authenticator *ap;
	char *name;
	int level;
{
	if (level < AUTH_USER)
		return(level);

	if (UserNameRequested && !kuserok(&adat, UserNameRequested)) {
		strcpy(name, UserNameRequested);
		return(AUTH_VALID);
	} else
		return(AUTH_USER);
}

#define	BUMP(buf, len)		while (*(buf)) {++(buf), --(len);}
#define	ADDC(buf, len, c)	if ((len) > 0) {*(buf)++ = (c); --(len);}

	void
kerberos4_printsub(data, cnt, buf, buflen)
	unsigned char *data, *buf;
	int cnt, buflen;
{
	char lbuf[32];
	register int i;

	buf[buflen-1] = '\0';		/* make sure its NULL terminated */
	buflen -= 1;

	switch(data[3]) {
	case KRB_NAME:			/* Name to authenticate for */
		strncpy((char *)buf, " NAME ", buflen);
		goto common;

	case KRB_REJECT:		/* Rejected (reason might follow) */
		strncpy((char *)buf, " REJECT ", buflen);
		goto common;

	case KRB_ACCEPT:		/* Accepted (name might follow) */
		strncpy((char *)buf, " ACCEPT ", buflen);
	common:
		BUMP(buf, buflen);
		if (cnt <= 4)
			break;
		ADDC(buf, buflen, '"');
		for (i = 4; i < cnt; i++)
			ADDC(buf, buflen, data[i]);
		ADDC(buf, buflen, '"');
		ADDC(buf, buflen, '\0');
		break;

	case KRB_AUTH:			/* Authentication data follows */
		strncpy((char *)buf, " AUTH", buflen);
		goto common2;

	case KRB_NEWKEY:		/* A new session key follows */
		strncpy((char *)buf, " NEWKEY", buflen);
		goto common2;

	default:
		sprintf(lbuf, " %d (unknown)", data[3]);
		strncpy((char *)buf, lbuf, buflen);
	common2:
		BUMP(buf, buflen);
		for (i = 4; i < cnt; i++) {
			sprintf(lbuf, " %d", data[i]);
			strncpy((char *)buf, lbuf, buflen);
			BUMP(buf, buflen);
		}
		break;
	}
}

	int
cksum(d, n)
	unsigned char *d;
	int n;
{
	int ck = 0;

	switch (n&03)
	while (n > 0) {
	case 0:
		ck ^= *d++ << 24;
		--n;
	case 3:
		ck ^= *d++ << 16;
		--n;
	case 2:
		ck ^= *d++ << 8;
		--n;
	case 1:
		ck ^= *d++;
		--n;
	}
	return(ck);
}
#endif

#ifdef notdef

prkey(msg, key)
	char *msg;
	unsigned char *key;
{
	register int i;
	printf("%s:", msg);
	for (i = 0; i < 8; i++)
		printf(" %3d", key[i]);
	printf("\r\n");
}
#endif
