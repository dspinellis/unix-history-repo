/*
 * $Source: /usr/src/kerberosIV/krb/RCS/save_credentials.c,v $
 * $Author: kfall $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef lint
static char *rcsid_save_credentials_c =
"$Header: /usr/src/kerberosIV/krb/RCS/save_credentials.c,v 4.10 90/06/25 20:57:18 kfall Exp $";
#endif /* lint */

#include <mit-copyright.h>
#include <stdio.h>
#include <des.h>
#include <krb.h>

/*
 * This routine takes a ticket and associated info and calls
 * tf_save_cred() to store them in the ticket cache.  The peer
 * routine for extracting a ticket and associated info from the
 * ticket cache is krb_get_cred().  When changes are made to
 * this routine, the corresponding changes should be made
 * in krb_get_cred() as well.
 *
 * Returns KSUCCESS if all goes well, otherwise an error returned
 * by the tf_init() or tf_save_cred() routines.
 */

save_credentials(service, instance, realm, session, lifetime, kvno,
                 ticket, issue_date)
    char *service;              /* Service name */
    char *instance;             /* Instance */
    char *realm;                /* Auth domain */
    C_Block session;            /* Session key */
    int lifetime;               /* Lifetime */
    int kvno;                   /* Key version number */
    KTEXT ticket;               /* The ticket itself */
    long issue_date;            /* The issue time */
{
    int tf_status;   /* return values of the tf_util calls */

    /* Open and lock the ticket file for writing */
    if ((tf_status = tf_init(TKT_FILE, W_TKT_FIL)) != KSUCCESS)
	return(tf_status);

    /* Save credentials by appending to the ticket file */
    tf_status = tf_save_cred(service, instance, realm, session,
			     lifetime, kvno, ticket, issue_date);
    (void) tf_close();
    return (tf_status);
}
