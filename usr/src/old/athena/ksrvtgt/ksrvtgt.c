/*
 * $Source: /mit/kerberos/src/kuser/RCS/ksrvtgt.c,v $
 * $Author: steiner $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology. 
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>. 
 *
 * Get a ticket-granting-ticket given a service key file (srvtab)
 * The lifetime is the shortest allowed [1 five-minute interval]
 *
 */

#include <stdio.h>
#include <kerberos/krb.h>
#include <kerberos/conf.h>

const char rcsid[] =
    "$Header: ksrvtgt.c,v 4.2 89/01/13 18:05:19 steiner Exp $";

char	realmbuf[REALM_SZ];
static	char *realm = NULL;
static	char *srvtab = NULL;
static	char srvtabfile[] = KEYFILE;

main(argc,argv)
	int argc;
	char **argv;
{
	register int code;
	if (argc < 3 || argc > 5) {
		fprintf(stderr, "Usage: %s name instance [[realm] srvtab]\n",
		argv[0]);
		exit(1);
	}
	if (argc == 5) {
		realm = argv[3];
		srvtab = argv[4];
	}
	else if (argc == 4)
		srvtab = argv[3];

	if(realm == NULL) {
		if(krb_get_lrealm(realmbuf, 1) != KSUCCESS) {
			fprintf(stderr, "Couldn't get local relm info.\n");
			exit(1);
		}
		realm = realmbuf;
	}

	if(srvtab == NULL)
		srvtab = srvtabfile;

	code = krb_get_svc_in_tkt(argv[1], argv[2], realm,
			      "krbtgt", realm, 1, srvtab);
	if (code != KSUCCESS)
		fprintf(stderr, "%s\n", krb_err_txt[code]);
	exit(code);
}
