/*
 * $Source: /mit/kerberos/src/lib/krb/RCS/one.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef	lint
static char rcsid_one_c[] =
"$Header: one.c,v 4.1 88/11/15 16:51:41 jtkohl Exp $";
#endif	lint

#include <mit-copyright.h>

/*
 * definition of variable set to 1.
 * used in krb_conf.h to determine host byte order.
 */

int krbONE = 1;
