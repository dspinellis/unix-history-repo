/* str2oid.c - string to object identifier */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/str2oid.c,v 7.1 91/02/22 09:37:04 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/str2oid.c,v 7.1 91/02/22 09:37:04 mrose Interim $
 *
 *
 * $Log:	str2oid.c,v $
 * Revision 7.1  91/02/22  09:37:04  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:46  mrose
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

OID	str2oid (s)
char    *s;
{
    int	    i;
    static struct OIDentifier   oids;
    static unsigned int elements[NELEM + 1];

    if ((i = str2elem (s, elements)) < 1)
	return NULLOID;

    oids.oid_elements = elements, oids.oid_nelem = i;

    return (&oids);
}
