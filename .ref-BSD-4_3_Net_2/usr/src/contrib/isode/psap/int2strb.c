/* int2strb.c - integer to string of bits */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/int2strb.c,v 7.1 91/02/22 09:35:41 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/int2strb.c,v 7.1 91/02/22 09:35:41 mrose Interim $
 *
 *
 * $Log:	int2strb.c,v $
 * Revision 7.1  91/02/22  09:35:41  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:39  mrose
 * Release 6.0
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
#include "psap.h"

/*  */

char   *int2strb (n, len)
register int    n;
int     len;
{
    register int    i;
    static char buffer[sizeof (int) + 1];

    bzero (buffer, sizeof (buffer));
    for (i = 0; i < len; i++)
	if (n & (1 << i))
	    buffer[i / 8] |= (1 << (7 - (i % 8)));

    return buffer;
}
