/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)klogin.c	5.4 (Berkeley) %G%";
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
 * Attempt to log the user in using Kerberos authentication
 *
 * return 0 on success (will be logged in)
 *	  1 if Kerberos failed (try local password in login)
 */

klogin(pw, localhost, password)
	struct passwd *pw;
	char *localhost, *password;
{
	int kerror;
	AUTH_DAT authdata;
	KTEXT_ST ticket;
	struct hostent *hp;
	unsigned long faddr;
	char realm[REALM_SZ], savehost[MAXHOSTNAMELEN];
	char tkt_location[MAXPATHLEN];

	/*
	 * If we aren't Kerberos-authenticated, try the normal pw file
	 * for a password.  If that's ok, log the user in without issueing
	 * any tickets.
	 */
	if (krb_get_lrealm(realm, 0) != KSUCCESS) {
		syslog(LOG_ERR, "couldn't get local Kerberos realm");
		return(1);
	}

	/*
	 * get TGT for local realm
	 * tickets are stored in a file determined by calling tkt_string()
	 */

	(void)sprintf(tkt_location, "%s%d", TKT_ROOT, pw->pw_uid);
	(void)krb_set_tkt_string(tkt_location);
	(void)dest_tkt();

	kerror = krb_get_pw_in_tkt(PRINCIPAL_NAME, PRINCIPAL_INST,
		    realm, INITIAL_TICKET, realm, DEFAULT_TKT_LIFE, password);
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
		dest_tkt();
		if (kerror != INTK_BADPW && kerror != KDC_PR_UNKNOWN)
			syslog(LOG_ERR, "Kerberos intkt error: %s",
			    krb_err_txt[kerror]);
		return(1);
	}

	if (chown(TKT_FILE, pw->pw_uid, pw->pw_gid) < 0)
		syslog(LOG_ERR, "chown tkfile (%s): %m", TKT_FILE);

	(void)strncpy(savehost, krb_get_phost(localhost), sizeof(savehost));
	savehost[sizeof(savehost)-1] = NULL;

	/*
	 * if the "VERIFY_SERVICE" doesn't exist in the KDC for this host,
	 * still allow login with tickets, but log the error condition.
	 */

	kerror = krb_mk_req(&ticket, VERIFY_SERVICE, savehost, realm, 33);
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
		return(1);
	}

	if (!(hp = gethostbyname(localhost))) {
		syslog(LOG_ERR, "couldn't get local host address");
		dest_tkt();
		return(1);
	}

	bcopy((void *)hp->h_addr, (void *)&faddr, sizeof(faddr));

	kerror = krb_rd_req(&ticket, VERIFY_SERVICE, savehost, faddr,
	    &authdata, "");

	if (kerror == KSUCCESS) {
		notickets = 0;
		return(0);
	}

	/* undecipherable: probably didn't have a srvtab on the local host */
	if (kerror = RD_AP_UNDEC) {
		syslog(LOG_NOTICE, "krb_rd_req: (%s)\n", krb_err_txt[kerror]);
		dest_tkt();
		return(1);
	}
	/* failed for some other reason */
	(void)printf("unable to verify %s ticket: (%s)\n", VERIFY_SERVICE,
	    krb_err_txt[kerror]);
	syslog(LOG_NOTICE, "couldn't verify %s ticket: %s", VERIFY_SERVICE,
	    krb_err_txt[kerror]);
	dest_tkt();
	return(1);
}
#endif
