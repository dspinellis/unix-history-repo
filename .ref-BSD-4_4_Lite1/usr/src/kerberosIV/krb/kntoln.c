/*
 * $Source: /usr/src/kerberosIV/krb/RCS/kntoln.c,v $
 * $Author: bostic $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef lint
static char *rcsid_kntoln_c =
"$Header: /usr/src/kerberosIV/krb/RCS/kntoln.c,v 4.10 92/07/19 13:22:26 bostic Exp $";
#endif /* lint */

#include <mit-copyright.h>
#include <des.h>
#include <krb.h>
#include <strings.h>

/*
 * krb_kntoln converts an auth name into a local name by looking up
 * the auth name in the /etc/aname file.  The format of the aname
 * file is:
 *
 * +-----+-----+-----+-----+------+----------+-------+-------+
 * | anl | inl | rll | lnl | name | instance | realm | lname |
 * +-----+-----+-----+-----+------+----------+-------+-------+
 * | 1by | 1by | 1by | 1by | name | instance | realm | lname |
 * +-----+-----+-----+-----+------+----------+-------+-------+
 *
 * If the /etc/aname file can not be opened it will set the
 * local name to the auth name.  Thus, in this case it performs as
 * the identity function.
 *
 * The name instance and realm are passed to krb_kntoln through
 * the AUTH_DAT structure (ad).
 *
 * Now here's what it *really* does:
 *
 * Given a Kerberos name in an AUTH_DAT structure, check that the
 * instance is null, and that the realm is the same as the local
 * realm, and return the principal's name in "lname".  Return
 * KSUCCESS if all goes well, otherwise KFAILURE.
 */

krb_kntoln(ad,lname)
    AUTH_DAT *ad;
    char *lname;
{
    static char lrealm[REALM_SZ] = "";

    if (!(*lrealm) && (krb_get_lrealm(lrealm,0) == KFAILURE))
        return(KFAILURE);

    if (strcmp(ad->pinst,""))
        return(KFAILURE);
    if (strcmp(ad->prealm,lrealm))
        return(KFAILURE);
    (void) strcpy(lname,ad->pname);
    return(KSUCCESS);
}
