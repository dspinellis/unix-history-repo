/*
 * $Source: /mit/kerberos/src/lib/des/RCS/key_parity.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1989 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * These routines check and fix parity of encryption keys for the DES
 * algorithm.
 *
 * Under U.S. law, this software may not be exported outside the US
 * without license from the U.S. Commerce department.
 *
 * These routines form the library interface to the DES facilities.
 *
 */

#ifndef	lint
static char rcsid_key_parity_c[] =
"$Header: key_parity.c,v 4.0 89/01/22 11:45:12 jtkohl Exp $";
#endif	lint

#include <mit-copyright.h>
#include <des.h>
#include "des_internal.h"

#include "odd.h"          /* Load compile-time generated odd_parity table */

/*
 * des_fixup_key_parity: Forces odd parity per byte; parity is bits
 *                       8,16,...64 in des order, implies 0, 8, 16, ...
 *                       vax order.
 */
void
des_fixup_key_parity(key)
     register des_cblock key;
{
    int i;

    for (i=0; i<sizeof(des_cblock); i++)
      key[i] = odd_parity[key[i]];

    return;
}

/*
 * des_check_key_parity: returns true iff key has the correct des parity.
 *                       See des_fix_key_parity for the definition of
 *                       correct des parity.
 */
int
des_check_key_parity(key)
     register des_cblock key;
{
    int i;

    for (i=0; i<sizeof(des_cblock); i++)
      if (key[i] != odd_parity[key[i]])
	return(0);

    return(1);
}
