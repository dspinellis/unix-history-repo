/*
 *	$Source: /afs/athena.mit.edu/astaff/project/krb5/src/appl/telnet/libtelnet/RCS/kerberos5.c,v $
 *	$Author: jtkohl $
 *	$Id: kerberos5.c,v 1.3 91/07/19 16:37:57 jtkohl Exp Locker: tytso $
 */

#if !defined(lint) && !defined(SABER)
static
#ifdef __STDC__
const
#endif
char rcsid_kerberos5_c[] = "$Id: kerberos5.c,v 1.3 91/07/19 16:37:57 jtkohl Exp Locker: tytso $";
#endif /* lint */

/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)kerberos5.c	8.1 (Berkeley) %G%";
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


#ifdef	KRB5
#include <arpa/telnet.h>
#include <stdio.h>
#include <krb5/krb5.h>
#include <krb5/crc-32.h>
#include <krb5/los-proto.h>
#include <krb5/ext-proto.h>
#include <com_err.h>
#include <netdb.h>
#include <ctype.h>


/* kerberos 5 include files (ext-proto.h) will get an appropriate stdlib.h
   and string.h/strings.h */
 
#include "encrypt.h"
#include "auth.h"
#include "misc.h"

extern auth_debug_mode;


static unsigned char str_data[1024] = { IAC, SB, TELOPT_AUTHENTICATION, 0,
			  		AUTHTYPE_KERBEROS_V5, };
/*static unsigned char str_name[1024] = { IAC, SB, TELOPT_AUTHENTICATION,
					TELQUAL_NAME, };*/

#define	KRB_AUTH	0		/* Authentication data follows */
#define	KRB_REJECT	1		/* Rejected (reason might follow) */
#define	KRB_ACCEPT	2		/* Accepted */
#define	KRB_RESPONSE	3		/* Response for mutual auth. */

static	krb5_data auth;
	/* telnetd gets session key from here */
static	krb5_tkt_authent *authdat = NULL;
/* telnet matches the AP_REQ and AP_REP with this */
static	krb5_authenticator authenticator;

/* some compilers can't hack void *, so we use the Kerberos krb5_pointer,
   which is either void * or char *, depending on the compiler. */

#define Voidptr krb5_pointer

#ifdef	ENCRYPTION
Block	session_key;
#endif	/* ENCRYPTION */
	static int
Data(ap, type, d, c)
	Authenticator *ap;
	int type;
	Voidptr d;
	int c;
{
        unsigned char *p = str_data + 4;
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
	*p++ = ap->type;
	*p++ = ap->way;
	*p++ = type;
        while (c-- > 0) {
                if ((*p++ = *cd++) == IAC)
                        *p++ = IAC;
        }
        *p++ = IAC;
        *p++ = SE;
	if (str_data[3] == TELQUAL_IS)
		printsub('>', &str_data[2], p - &str_data[2]);
        return(net_write(str_data, p - str_data));
}

	int
kerberos5_init(ap, server)
	Authenticator *ap;
	int server;
{
	if (server)
		str_data[3] = TELQUAL_REPLY;
	else
		str_data[3] = TELQUAL_IS;
        krb5_init_ets();
	return(1);
}

	int
kerberos5_send(ap)
	Authenticator *ap;
{
	char **realms;
	char *name;
	char *p1, *p2;
	krb5_checksum ksum;
	krb5_octet sum[CRC32_CKSUM_LENGTH];
 	krb5_principal server;
	krb5_error_code r;
	krb5_ccache ccache;
	krb5_creds creds;		/* telnet gets session key from here */
	extern krb5_flags krb5_kdc_default_options;
	int ap_opts;

#ifdef	ENCRYPTION
	krb5_keyblock *newkey = 0;
#endif	/* ENCRYPTION */

	ksum.checksum_type = CKSUMTYPE_CRC32;
	ksum.contents = sum;
	ksum.length = sizeof(sum);
	bzero((Voidptr )sum, sizeof(sum));
	
        if (!UserNameRequested) {
                if (auth_debug_mode) {
                        printf("Kerberos V5: no user name supplied\r\n");
                }
                return(0);
        }

	if (r = krb5_cc_default(&ccache)) {
		if (auth_debug_mode) {
			printf("Kerberos V5: could not get default ccache\r\n");
		}
		return(0);
	}

	if ((name = malloc(strlen(RemoteHostName)+1)) == NULL) {
		if (auth_debug_mode)
			printf("Out of memory for hostname in Kerberos V5\r\n");
		return(0);
	}

	if (r = krb5_get_host_realm(RemoteHostName, &realms)) {
		if (auth_debug_mode)
			printf("Kerberos V5: no realm for %s\r\n", RemoteHostName);
		free(name);
		return(0);
	}

	p1 = RemoteHostName;
	p2 = name;

	while (*p2 = *p1++) {
		if (isupper(*p2))
			*p2 |= 040;
		++p2;
	}

	if (r = krb5_build_principal_ext(&server,
					 strlen(realms[0]), realms[0],
					 4, "rcmd",
					 p2 - name, name,
					 0)) {
		if (auth_debug_mode) {
			printf("Kerberos V5: failure setting up principal (%s)\r\n",
			       error_message(r));
		}
		free(name);
		krb5_free_host_realm(realms);
		return(0);
	}
					 

	bzero((char *)&creds, sizeof(creds));
	creds.server = server;

	if (r = krb5_cc_get_principal(ccache, &creds.client)) {
		if (auth_debug_mode) {
			printf("Kerberos V5: failure on principal (%s)\r\n",
				error_message(r));
		}
		free(name);
		krb5_free_principal(server);
		krb5_free_host_realm(realms);
		return(0);
	}

	if (r = krb5_get_credentials(krb5_kdc_default_options, ccache, &creds)) {
		if (auth_debug_mode) {
			printf("Kerberos V5: failure on credentials(%d)\r\n",r);
		}
		free(name);
		krb5_free_host_realm(realms);
		krb5_free_principal(server);
		return(0);
	}

	if ((ap->way & AUTH_HOW_MASK) == AUTH_HOW_MUTUAL)
	    ap_opts = AP_OPTS_MUTUAL_REQUIRED;
	else
	    ap_opts = 0;
	    
	r = krb5_mk_req_extended(ap_opts, &ksum, krb5_kdc_default_options, 0,
#ifdef	ENCRYPTION
				 &newkey,
#else	/* ENCRYPTION */
				 0,
#endif	/* ENCRYPTION */
				 ccache, &creds, &authenticator, &auth);
	/* don't let the key get freed if we clean up the authenticator */
	authenticator.subkey = 0;

	free(name);
	krb5_free_host_realm(realms);
	krb5_free_principal(server);
#ifdef	ENCRYPTION
	if (newkey) {
	    /* keep the key in our private storage, but don't use it
	       yet---see kerberos5_reply() below */
	    if (newkey->keytype != KEYTYPE_DES) {
		if (creds.keyblock.keytype == KEYTYPE_DES)
		    /* use the session key in credentials instead */
		    memcpy((char *)session_key,
			   (char *)creds.keyblock.contents, sizeof(Block));
		else
		    /* XXX ? */;
	    } else {
		memcpy((char *)session_key, (char *)newkey->contents,
		       sizeof(Block));
	    }
	    krb5_free_keyblock(newkey);
	}
#endif	/* ENCRYPTION */
	if (r) {
		if (auth_debug_mode) {
			printf("Kerberos V5: mk_req failed (%s)\r\n",
			       error_message(r));
		}
		return(0);
	}

        if (!auth_sendname(UserNameRequested, strlen(UserNameRequested))) {
                if (auth_debug_mode)
                        printf("Not enough room for user name\r\n");
                return(0);
        }
	if (!Data(ap, KRB_AUTH, auth.data, auth.length)) {
		if (auth_debug_mode)
			printf("Not enough room for authentication data\r\n");
		return(0);
	}
	if (auth_debug_mode) {
		printf("Sent Kerberos V5 credentials to server\r\n");
	}
	return(1);
}

	void
kerberos5_is(ap, data, cnt)
	Authenticator *ap;
	unsigned char *data;
	int cnt;
{
	int r;
	struct hostent *hp;
	char *p1, *p2;
	static char *realm = NULL;
	krb5_principal server;
	krb5_ap_rep_enc_part reply;
	krb5_data outbuf;
	Session_Key skey;
	char *name;
	char *getenv();

	if (cnt-- < 1)
		return;
	switch (*data++) {
	case KRB_AUTH:
		auth.data = (char *)data;
		auth.length = cnt;

		if (!(hp = gethostbyname(LocalHostName))) {
			if (auth_debug_mode)
				printf("Cannot resolve local host name\r\n");
			Data(ap, KRB_REJECT, "Unknown local hostname.", -1);
			auth_finished(ap, AUTH_REJECT);
			return;
		}

		if (!realm && (krb5_get_default_realm(&realm))) {
			if (auth_debug_mode)
				printf("Could not get default realm\r\n");
			Data(ap, KRB_REJECT, "Could not get default realm.", -1);
			auth_finished(ap, AUTH_REJECT);
			return;
		}

		if ((name = malloc(strlen(hp->h_name)+1)) == NULL) {
			if (auth_debug_mode)
				printf("Out of memory for hostname in Kerberos V5\r\n");
			Data(ap, KRB_REJECT, "Out of memory.", -1);
			auth_finished(ap, AUTH_REJECT);
			return;
		}

		p1 = hp->h_name;
		p2 = name;

		while (*p2 = *p1++) {
			if (isupper(*p2))
				*p2 |= 040;
			++p2;
		}

		if (authdat)
			krb5_free_tkt_authent(authdat);

	        r = krb5_build_principal_ext(&server,
					     strlen(realm), realm,
					     4, "rcmd",
					     p2 - name, name,
					     0);
		if (!r) {
		    r = krb5_rd_req_simple(&auth, server, 0, &authdat);
		    krb5_free_principal(server);
		}
		if (r) {
			char errbuf[128];

		    errout:
			authdat = 0;
			(void) strcpy(errbuf, "Read req failed: ");
			(void) strcat(errbuf, error_message(r));
			Data(ap, KRB_REJECT, errbuf, -1);
			if (auth_debug_mode)
				printf("%s\r\n", errbuf);
			return;
		}
		free(name);
		if ((ap->way & AUTH_HOW_MASK) == AUTH_HOW_MUTUAL) {
		    /* do ap_rep stuff here */
		    reply.ctime = authdat->authenticator->ctime;
		    reply.cusec = authdat->authenticator->cusec;
		    reply.subkey = 0;	/* use the one he gave us, so don't
					   need to return one here */
		    reply.seq_number = 0; /* we don't do seq #'s. */

		    if (r = krb5_mk_rep(&reply,
					authdat->authenticator->subkey ?
					authdat->authenticator->subkey :
					authdat->ticket->enc_part2->session,
					&outbuf)) {
			goto errout;
		    }
		    Data(ap, KRB_RESPONSE, outbuf.data, outbuf.length);
		} 
		if (krb5_unparse_name(authdat->ticket->enc_part2 ->client,
				      					&name))
			name = 0;
		Data(ap, KRB_ACCEPT, name, name ? -1 : 0);
		if (auth_debug_mode) {
			printf("Kerberos5 identifies him as ``%s''\r\n",
							name ? name : "");
		}
                auth_finished(ap, AUTH_USER);
		
		free(name);
	    	if (authdat->authenticator->subkey &&
		    authdat->authenticator->subkey->keytype == KEYTYPE_DES) {
		    bcopy((Voidptr )authdat->authenticator->subkey->contents,
			  (Voidptr )session_key, sizeof(Block));
		} else if (authdat->ticket->enc_part2->session->keytype ==
			   KEYTYPE_DES) {
		    bcopy((Voidptr )authdat->ticket->enc_part2->session->contents,
			  (Voidptr )session_key, sizeof(Block));
		} else
		    break;

		skey.type = SK_DES;
		skey.length = 8;
		skey.data = session_key;
		encrypt_session_key(&skey, 1);
		break;
	default:
		if (auth_debug_mode)
			printf("Unknown Kerberos option %d\r\n", data[-1]);
		Data(ap, KRB_REJECT, 0, 0);
		break;
	}
}

	void
kerberos5_reply(ap, data, cnt)
	Authenticator *ap;
	unsigned char *data;
	int cnt;
{
        Session_Key skey;
	static int mutual_complete = 0;

	if (cnt-- < 1)
		return;
	switch (*data++) {
	case KRB_REJECT:
		if (cnt > 0) {
			printf("[ Kerberos V5 refuses authentication because %.*s ]\r\n",
				cnt, data);
		} else
			printf("[ Kerberos V5 refuses authentication ]\r\n");
		auth_send_retry();
		return;
	case KRB_ACCEPT:
		if ((ap->way & AUTH_HOW_MASK) == AUTH_HOW_MUTUAL &&
		    !mutual_complete) {
		    printf("[ Kerberos V5 accepted you, but didn't provide mutual authentication! ]\n");
		    auth_send_retry();
		    return;
		}
		if (cnt)
		    printf("[ Kerberos V5 accepts you as ``%.*s'' ]\n", cnt, data);
		else
		    printf("[ Kerberos V5 accepts you ]\n");
		auth_finished(ap, AUTH_USER);
		break;
	case KRB_RESPONSE:
		if ((ap->way & AUTH_HOW_MASK) == AUTH_HOW_MUTUAL) {
		    /* the rest of the reply should contain a krb_ap_rep */
		    krb5_ap_rep_enc_part *reply;
		    krb5_data inbuf;
		    krb5_error_code r;
		    krb5_keyblock tmpkey;

		    inbuf.length = cnt;
		    inbuf.data = (char *)data;

		    tmpkey.keytype = KEYTYPE_DES;
		    tmpkey.contents = session_key;
		    tmpkey.length = sizeof(Block);

		    if (r = krb5_rd_rep(&inbuf, &tmpkey, &reply)) {
			printf("[ Mutual authentication failed: %s ]\n",
			       error_message(r));
			auth_send_retry();
			return;
		    }
		    if (reply->ctime != authenticator.ctime ||
			reply->cusec != authenticator.cusec) {
			printf("[ Mutual authentication failed (mismatched KRB_AP_REP) ]\n");
			auth_send_retry();
			return;
		    }
		    krb5_free_ap_rep_enc_part(reply);
#ifdef	ENCRYPTION
			skey.type = SK_DES;
			skey.length = 8;
			skey.data = session_key;
			encrypt_session_key(&skey, 0);
#endif	/* ENCRYPTION */
		    mutual_complete = 1;
		}
		return;
	default:
		if (auth_debug_mode)
			printf("Unknown Kerberos option %d\r\n", data[-1]);
		return;
	}
}

	int
kerberos5_status(ap, name, level)
	Authenticator *ap;
	char *name;
	int level;
{
	if (level < AUTH_USER)
		return(level);

	if (UserNameRequested &&
	    krb5_kuserok(authdat->ticket->enc_part2->client, UserNameRequested))
	{
		strcpy(name, UserNameRequested);
		return(AUTH_VALID);
	} else
		return(AUTH_USER);
}

#define	BUMP(buf, len)		while (*(buf)) {++(buf), --(len);}
#define	ADDC(buf, len, c)	if ((len) > 0) {*(buf)++ = (c); --(len);}

	void
kerberos5_printsub(data, cnt, buf, buflen)
	unsigned char *data, *buf;
	int cnt, buflen;
{
	char lbuf[32];
	register int i;

	buf[buflen-1] = '\0';		/* make sure its NULL terminated */
	buflen -= 1;

	switch(data[3]) {
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

	case KRB_RESPONSE:
		strncpy((char *)buf, " RESPONSE", buflen);
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
#endif
