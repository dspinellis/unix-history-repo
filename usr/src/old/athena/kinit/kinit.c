/*
 * $Source: /mit/kerberos/src/kuser/RCS/kinit.c,v $
 * $Author: jtkohl $ 
 *
 * Copyright 1987, 1988 by the Massachusetts Institute of Technology. 
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>. 
 *
 * Routine to initialize user to Kerberos.  Prompts optionally for
 * user, instance and realm.  Authenticates user and gets a ticket
 * for the Kerberos ticket-granting service for future use. 
 *
 * Options are: 
 *
 *   -i[instance]
 *   -r[realm]
 *   -v[erbose]
 *   -l[ifetime]
 */

#ifndef	lint
static char rcsid_kinit_c[] =
"$Header: kinit.c,v 4.11 89/01/23 09:34:49 jtkohl Exp $";
#endif	lint

#include <kerberos/mit-copyright.h>
#include <stdio.h>
#include <pwd.h>
#include <kerberos/krb.h>

#include <strings.h>
#include <sys/param.h>

#define	LEN		MAXHOSTNAMELEN
#define	LIFE		96 	/* tick lifetime in 5-min units<8hrs> */
#define	MAX_LIFE	255	/* maximum life in 5-min units */

char   *progname;

main(argc, argv)
    char   *argv[];
{
    char    aname[ANAME_SZ];
    char    inst[INST_SZ];
    char    realm[REALM_SZ];
    char    buf[LEN];
    char   *username = NULL;
    int     iflag, rflag, vflag, lflag, lifetime, k_errno;
    register char *cp;
    register i;

    *inst = *realm = '\0';
    iflag = rflag = vflag = lflag = 0;
    lifetime = LIFE;
    progname = (cp = rindex(*argv, '/')) ? cp + 1 : *argv;

    while (--argc) {
	if ((*++argv)[0] != '-') {
	    if (username)
		usage();
	    username = *argv;
	    continue;
	}
	for (i = 1; (*argv)[i] != '\0'; i++)
	    switch ((*argv)[i]) {
	    case 'i':		/* Instance */
		++iflag;
		continue;
	    case 'r':		/* Realm */
		++rflag;
		continue;
	    case 'v':		/* Verbose */
		++vflag;
		continue;
	    case 'l':
		++lflag;
		continue;
	    default:
		usage();
		exit(1);
	    }
    }
    if (username &&
	(k_errno = kname_parse(aname, inst, realm, username))
	!= KSUCCESS) {
	fprintf(stderr, "%s: %s\n", progname, krb_err_txt[k_errno]);
	iflag = rflag = 1;
	username = NULL;
    }
    if (k_gethostname(buf, LEN)) {
	fprintf(stderr, "%s: k_gethostname failed\n", progname);
	exit(1);
    }
    printf("MIT Project Athena/UC Berkeley (%s)\n", buf);
    if (username) {
	printf("Kerberos Initialization for \"%s", aname);
	if (*inst)
	    printf(".%s", inst);
	if (*realm)
	    printf("@%s", realm);
	printf("\"\n");
    } else {
	printf("Kerberos Initialization\n");
	printf("Kerberos name: ");
	getstr(aname, ANAME_SZ);
	if (!*aname)
	    exit(0);
	if (!k_isname(aname)) {
	    fprintf(stderr, "%s: bad Kerberos name format\n",
		    progname);
	    exit(1);
	}
    }
    /* optional instance */
    if (iflag) {
	printf("Kerberos instance: ");
	getstr(inst, INST_SZ);
	if (!k_isinst(inst)) {
	    fprintf(stderr, "%s: bad Kerberos instance format\n",
		    progname);
	    exit(1);
	}
    }
    if (rflag) {
	printf("Kerberos realm: ");
	getstr(realm, REALM_SZ);
	if (!k_isrealm(realm)) {
	    fprintf(stderr, "%s: bad Kerberos realm format\n",
		    progname);
	    exit(1);
	}
    }
    if (lflag) {
	 printf("Kerberos ticket lifetime (minutes): ");
	 getstr(buf, LEN);
	 lifetime = atoi(buf);
	 if (lifetime < 5)
	      lifetime = 1;
	 else
	      lifetime /= 5;
	 /* This should be changed if the maximum ticket lifetime */
	 /* changes */
	 if (lifetime > MAX_LIFE)
	      lifetime = MAX_LIFE;
    }
    if (!*realm && krb_get_lrealm(realm, 1)) {
	fprintf(stderr, "%s: krb_get_lrealm failed\n", progname);
	exit(1);
    }
    printf("Getting initial ticket for %s.%s@%s\n",
	aname, inst, realm);
    k_errno = krb_get_pw_in_tkt(aname, inst, realm, "krbtgt", realm,
				lifetime, 0);
    if (vflag) {
	printf("Kerberos realm %s:\n", realm);
	printf("%s\n", krb_err_txt[k_errno]);
    } else if (k_errno) {
	fprintf(stderr, "%s: %s\n", progname, krb_err_txt[k_errno]);
	exit(1);
    }
}

usage()
{
    fprintf(stderr, "Usage: %s [-irvl] [name]\n", progname);
    exit(1);
}

getstr(p, len)
	register char	*p;
	int		len;
{
	while(((*p++ = getchar()) != '\n') && --len)
		;
	*--p = '\0';
}
