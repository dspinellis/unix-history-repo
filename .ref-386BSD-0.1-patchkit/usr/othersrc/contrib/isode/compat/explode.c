/* explode.c - explode octets into ascii */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/explode.c,v 7.1 91/02/22 09:15:09 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/explode.c,v 7.1 91/02/22 09:15:09 mrose Interim $
 *
 *
 * $Log:	explode.c,v $
 * Revision 7.1  91/02/22  09:15:09  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:22:59  mrose
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
#include "general.h"
#include "manifest.h"

/*    DATA */

static char nib2hex[0x10] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};

/*  */

int	explode (a, b, n)
register char  *a;
register u_char *b;
register int    n;
{
    register int    i;
    register u_char c;

    for (i = 0; i < n; i++) {
	c = *b++;
	*a++ = nib2hex[(c & 0xf0) >> 4];
	*a++ = nib2hex[(c & 0x0f)];
    }
    *a = NULL;

    return (n * 2);
}
