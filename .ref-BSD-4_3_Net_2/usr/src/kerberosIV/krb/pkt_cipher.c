/*
 * $Source: /usr/src/kerberosIV/krb/RCS/pkt_cipher.c,v $
 * $Author: kfall $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#ifndef lint
static char *rcsid_pkt_cipher_c =
"$Header: /usr/src/kerberosIV/krb/RCS/pkt_cipher.c,v 4.9 90/06/25 20:57:02 kfall Exp $";
#endif /* lint */

#include <mit-copyright.h>
#include <des.h>
#include <krb.h>
#include <prot.h>


/*
 * This routine takes a reply packet from the Kerberos ticket-granting
 * service and returns a pointer to the beginning of the ciphertext in it.
 *
 * See "prot.h" for packet format.
 */

KTEXT
pkt_cipher(packet)
    KTEXT packet;
{
    unsigned char *ptr = pkt_a_realm(packet) + 6
	+ strlen((char *)pkt_a_realm(packet));
    /* Skip a few more fields */
    ptr += 3 + 4;		/* add 4 for exp_date */

    /* And return the pointer */
    return((KTEXT) ptr);
}
