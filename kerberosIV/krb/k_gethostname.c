/*
 * $Source: /usr/src/kerberosIV/krb/RCS/k_gethostname.c,v $
 * $Author: kfall $
 *
 * Copyright 1987, 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef lint
static char rcsid_k_gethostname_c[] =
"$Header: /usr/src/kerberosIV/krb/RCS/k_gethostname.c,v 4.2 90/06/23 03:10:52 kfall Exp $";
#endif /* lint */

#include <mit-copyright.h>

/*
 * Return the local host's name in "name", up to "namelen" characters.
 * "name" will be null-terminated if "namelen" is big enough.
 * The return code is 0 on success, -1 on failure.  (The calling
 * interface is identical to gethostname(2).)
 *
 * Currently defined for BSD 4.2 and PC.  The BSD version just calls
 * gethostname(); the PC code was taken from "kinit.c", and may or may
 * not work.
 */

k_gethostname(name, namelen)
    char *name;
    int namelen;
{
    return (gethostname(name, namelen));
}
