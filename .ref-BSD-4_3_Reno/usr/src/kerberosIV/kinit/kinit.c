/*
 * $Source: /usr/src/kerberosIV/kinit/RCS/kinit.c,v $
 * $Author: kfall $ 
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
"$Header: /usr/src/kerberosIV/kinit/RCS/kinit.c,v 4.15 90/06/25 21:01:06 kfall Exp $";
#endif	lint

#include <sys/types.h>
#include <sys/param.h>
#include <mit-copyright.h>
#include <string.h>
#include <stdio.h>
#include <des.h>
#include <krb.h>
#include <pwd.h>
#include <paths.h>

#define	LEN		MAXHOSTNAMELEN
#define	LIFE		DEFAULT_TKT_LIFE /* in 5-minute units */
#define	INITIAL_TICKET	"krbtgt"

char	*progname;

char    aname[ANAME_SZ];
char    inst[INST_SZ];
char    realm[REALM_SZ];

main(argc, argv)
    char   *argv[];
{
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
    if (username && (k_errno = kname_parse(aname, inst, realm, username)) !=
	KSUCCESS) {
	fprintf(stderr, "%s: %s\n", progname, krb_err_txt[k_errno]);
	iflag = rflag = 1;
	username = NULL;
    }
    if (k_gethostname(buf, LEN)) {
	fprintf(stderr, "%s: k_gethostname failed\n", progname);
	exit(1);
    }
    if (vflag)
    	printf("4.4 BSD/MIT Project Athena (%s)\n", buf);

    if (username) {
	printf("Kerberos Initialization for \"%s", aname);
	if (*inst)
	    printf(".%s", inst);
	if (*realm)
	    printf("@%s", realm);
	printf("\"\n");
    } else {
	if (iflag) {
		printf("Kerberos Initialization\n");
		printf("Kerberos name: ");
		gets(aname);
	} else {
		/* default to current user name */
		struct passwd	*pwd = getpwuid(geteuid());

		if (pwd == (struct passwd *) NULL) {
			fprintf(stderr, "Unknown Kerberos name for your uid\n");
			printf("Kerberos name: ");
			gets(aname);
		} else
			strncpy(aname, pwd->pw_name, sizeof(aname));
	}
		
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
	gets(inst);
	if (!k_isinst(inst)) {
	    fprintf(stderr, "%s: bad Kerberos instance format\n",
		    progname);
	    exit(1);
	}
    }
    if (rflag) {
	printf("Kerberos realm: ");
	gets(realm);
	if (!k_isrealm(realm)) {
	    fprintf(stderr, "%s: bad Kerberos realm format\n",
		    progname);
	    exit(1);
	}
    }
    if (lflag) {
	 printf("Kerberos ticket lifetime (minutes): ");
	 gets(buf);
	 lifetime = atoi(buf);
	 if (lifetime < 5)
	      lifetime = 1;
	 else
	      lifetime /= 5;
	 /* This should be changed if the maximum ticket lifetime */
	 /* changes */
	 if (lifetime > 255)
	      lifetime = 255;
    }
    if (!*realm && krb_get_lrealm(realm, 1)) {
	fprintf(stderr, "%s: krb_get_lrealm failed\n", progname);
	exit(1);
    }

    k_errno = krb_get_pw_in_tkt(aname, inst, realm, INITIAL_TICKET,
		realm, lifetime, 0);

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
