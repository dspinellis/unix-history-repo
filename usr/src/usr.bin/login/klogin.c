/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)klogin.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#ifdef KERBEROS
#include <sys/param.h>
#include <sys/syslog.h>
#include <kerberosIV/des.h>
#include <kerberosIV/krb.h>
#include <pwd.h>
#include <netdb.h>

#define	PRINCIPAL_NAME	pw->pw_name
#define	PRINCIPAL_INST	""
#define	INITIAL_TICKET	"krbtgt"
#define	VERIFY_SERVICE	"rcmd"

extern int notickets;

/*
 * return 0 on success
 *	  1 if Kerberos not around (try local)
 *	  2 on failure
 */

klogin(pw, localhost, name, tty)
	struct passwd *pw;
	char *localhost, *name, *tty;
{
	int kerror;
	AUTH_DAT authdata;
	KTEXT_ST ticket;
	struct hostent *hp;
	unsigned long faddr;
	char tkfile[MAXPATHLEN], realm[REALM_SZ], savehost[MAXHOSTNAMELEN];

	/*
	 * If we aren't Kerberos-authenticated, try the normal pw file
	 * for a password.  If that's ok, log the user in without issueing
	 * any tickets.
	 */
	if (krb_get_lrealm(realm, 1) != KSUCCESS)
		return(1);

	/*
	 * get TGT for local realm; by convention, store tickets in file
	 * associated with tty name, which should be available.
	 */
	(void)sprintf(tkfile, "%s_%s", TKT_ROOT, tty);

	if (setenv("KRBTKFILE", tkfile, 1) < 0) {
		kerror = INTK_ERR;
		syslog(LOG_ERR, "couldn't set tkfile environ");
	} else {
		(void)unlink(tkfile);
		kerror = krb_get_pw_in_tkt(PRINCIPAL_NAME, PRINCIPAL_INST,
		    realm, INITIAL_TICKET, realm, DEFAULT_TKT_LIFE, name);
	}

	/*
	 * If we got a TGT, get a local "rcmd" ticket and check it so as to
	 * ensure that we are not talking to a bogus Kerberos server.
	 *
	 * There are 2 cases where we still allow a login:
	 *	1: the VERIFY_SERVICE doesn't exist in the KDC
	 *	2: local host has no srvtab, as (hopefully) indicated by a
	 *	   return value of RD_AP_UNDEC from krb_rd_req().
	 */
	if (kerror != INTK_OK) {
		(void)unlink(tkfile);
		if (kerror != INTK_BADPW && kerror != KDC_PR_UNKNOWN)
			syslog(LOG_ERR, "Kerberos intkt error: %s",
			    krb_err_txt[kerror]);
		return(2);
	}

	if (chown(tkfile, pw->pw_uid, pw->pw_gid) < 0)
		syslog(LOG_ERR, "chown tkfile: %m");

	(void)strncpy(savehost, krb_get_phost(localhost), sizeof(savehost));
	savehost[sizeof(savehost)-1] = NULL;
	kerror = krb_mk_req(&ticket, VERIFY_SERVICE, savehost, realm, 33);

	/*
	 * if the "VERIFY_SERVICE" doesn't exist in the KDC for this host,
	 * still allow login with tickets, but log the error condition.
	 */
	if (kerror == KDC_PR_UNKNOWN) {
		syslog(LOG_NOTICE, "warning: TGT not verified (%s)",
		    krb_err_txt[kerror]);
		notickets = 0;
		return(0);
	}

	if (kerror != KSUCCESS) {
		(void)printf("unable to use TGT: (%s)\n", krb_err_txt[kerror]);
		syslog(LOG_NOTICE, "unable to use TGT: (%s)",
		    krb_err_txt[kerror]);
		dest_tkt();
		return(2);
	}

	if (!(hp = gethostbyname(localhost))) {
		syslog(LOG_ERR, "couldn't get local host address");
		return(2);
	}
	bcopy((void *)hp->h_addr, (void *)&faddr, sizeof(faddr));

	kerror = krb_rd_req(&ticket, VERIFY_SERVICE, savehost, faddr,
	    &authdata, "");
	if (kerror == KSUCCESS) {
		notickets = 0;
		return(0);
	}
	if (kerror = RD_AP_UNDEC) {
		syslog(LOG_NOTICE, "krb_rd_req: (%s)\n", krb_err_txt[kerror]);
		notickets = 0;
		return(0);
	}
	(void)printf("unable to verify %s ticket: (%s)\n", VERIFY_SERVICE,
	    krb_err_txt[kerror]);
	syslog(LOG_NOTICE, "couldn't verify %s ticket: %s", VERIFY_SERVICE,
	    krb_err_txt[kerror]);
	return(2);
}
#endif
