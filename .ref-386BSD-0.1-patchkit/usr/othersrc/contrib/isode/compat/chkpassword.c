/* chkpassword.c - check the password */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/chkpassword.c,v 7.1 91/02/22 09:15:03 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/chkpassword.c,v 7.1 91/02/22 09:15:03 mrose Interim $
 *
 *
 * $Log:	chkpassword.c,v $
 * Revision 7.1  91/02/22  09:15:03  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/14  11:00:31  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <stdio.h>
#include "general.h"
#include "manifest.h"


char   *crypt ();


#ifdef	KRB_PASSWD
#include <krb.h>
#endif

/* L.McLoughlin added kerberos passwd checking - based on original
 * code from xnlock by S. Lacey.
 * Takes the username, the password from the password file, and the passwd
 * the user is trying to use.
 * Returns 1 if the passwd matches otherwise 0.
 */

#ifndef	KRB_PASSWD
/* ARGSUSED */
#endif

int	chkpassword ( usrname, pwpass, usrpass )
char   *usrname;
char   *pwpass;
char   *usrpass;
{
#ifdef	KRB_PASSWD
	char realm[REALM_SZ];
	int krbval;

	/* 
	 * check to see if the passwd is `*krb*'
	 * if it is, use kerberos
	 */

	if (strcmp(pwpass, "*krb*") == 0) {
		/*
		 * use kerberos, first of all find the realm
		 */
		if (krb_get_lrealm(realm, 1) != KSUCCESS) {
			(void) strncpy(realm, KRB_REALM, sizeof(realm));
		}

		/*
		 * now check the passwd
		 */
		krbval = krb_get_pw_in_tkt(usrname, "",
					   realm, "krbtgt",
					   realm,
					   DEFAULT_TKT_LIFE, usrpass);

		return (krbval == INTK_OK);;
	}
#endif

	/*
	 * use passwd file password
	 */
	return (strcmp(crypt(usrpass, pwpass), pwpass) == 0);
}
