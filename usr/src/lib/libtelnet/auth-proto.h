/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)auth-proto.h	5.3 (Berkeley) %G%
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

#if	!defined(P)
#ifdef	__STDC__
#define	P(x)	x
#else
#define	P(x)	()
#endif
#endif

#if	defined(AUTHENTICATION)
Authenticator *findauthenticator P((int, int));

void auth_init P((char *, int));
int auth_cmd P((int, char **));
void auth_request P((void));
void auth_send P((unsigned char *, int));
void auth_send_retry P((void));
void auth_is P((unsigned char *, int));
void auth_reply P((unsigned char *, int));
void auth_finished P((Authenticator *, int));
int auth_wait P((char *));
void auth_disable_name P((char *));
void auth_gen_printsub P((unsigned char *, int, unsigned char *, int));

#ifdef	KRB4
int kerberos4_init P((Authenticator *, int));
int kerberos4_send P((Authenticator *));
void kerberos4_is P((Authenticator *, unsigned char *, int));
void kerberos4_reply P((Authenticator *, unsigned char *, int));
int kerberos4_status P((Authenticator *, char *, int));
void kerberos4_printsub P((unsigned char *, int, unsigned char *, int));
#endif

#ifdef	KRB5
int kerberos5_init P((Authenticator *, int));
int kerberos5_send P((Authenticator *));
void kerberos5_is P((Authenticator *, unsigned char *, int));
void kerberos5_reply P((Authenticator *, unsigned char *, int));
int kerberos5_status P((Authenticator *, char *, int));
void kerberos5_printsub P((unsigned char *, int, unsigned char *, int));
#endif
#endif
