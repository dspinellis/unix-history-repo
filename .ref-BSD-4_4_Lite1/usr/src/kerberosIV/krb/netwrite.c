/*
 * $Source: /mit/kerberos/src/lib/krb/RCS/netwrite.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1987, 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef	lint
static char rcsid_netwrite_c[] =
"$Header: netwrite.c,v 4.1 88/11/15 16:48:58 jtkohl Exp $";
#endif	lint

#include <mit-copyright.h>

/*
 * krb_net_write() writes "len" bytes from "buf" to the file
 * descriptor "fd".  It returns the number of bytes written or
 * a write() error.  (The calling interface is identical to
 * write(2).)
 *
 * XXX must not use non-blocking I/O
 */

int
krb_net_write(fd, buf, len)
int fd;
register char *buf;
int len;
{
    int cc;
    register int wrlen = len;
    do {
	cc = write(fd, buf, wrlen);
	if (cc < 0)
	    return(cc);
	else {
	    buf += cc;
	    wrlen -= cc;
	}
    } while (wrlen > 0);
    return(len);
}
