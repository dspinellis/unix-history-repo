/*
 * $Source: /usr/src/kerberosIV/src/lib/krb/RCS/util.c,v $
 * $Author: kfall $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * Miscellaneous debug printing utilities
 */

#ifndef	lint
static char rcsid_util_c[] =
"$Header: /usr/src/kerberosIV/src/lib/krb/RCS/util.c,v 4.9 90/05/12 00:59:00 kfall Exp $";
#endif	lint

#include <mit-copyright.h>
#include <des.h>
#include <krb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>

/*
 * Print some of the contents of the given authenticator structure
 * (AUTH_DAT defined in "krb.h").  Fields printed are:
 *
 * pname, pinst, prealm, netaddr, flags, cksum, timestamp, session
 */

ad_print(x)
    AUTH_DAT *x;
{
#ifndef lint
    /*
     * Print the contents of an auth_dat struct.  We can't cast a char
     * array (x->address) to a struct in_addr, so we must turn off
     * lint checking here.
     */
    printf("\n%s %s %s %s flags %u cksum 0x%X\n\ttkt_tm 0x%X sess_key",
           x->pname, x->pinst, x->prealm,
           inet_ntoa(x->address), x->k_flags,
           x->checksum, x->time_sec);
#endif /* lint */
    printf("[8] =");
#ifdef NOENCRYPTION
    placebo_cblock_print(x->session);
#else /* Do Encryption */
    des_cblock_print_file(x->session,stdout);
#endif /* NOENCRYPTION */
    /* skip reply for now */
}

#ifdef NOENCRYPTION
/*
 * Print in hex the 8 bytes of the given session key.
 *
 * Printed format is:  " 0x { x, x, x, x, x, x, x, x }"
 */

placebo_cblock_print(x)
    des_cblock x;
{
    unsigned char *y = (unsigned char *) x;
    register int i = 0;

    printf(" 0x { ");

    while (i++ <8) {
        printf("%x",*y++);
        if (i<8) printf(", ");
    }
    printf(" }");
}
#endif /* NOENCRYPTION */
