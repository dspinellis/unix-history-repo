/* oid_free.c - free an object identifier */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/oid_free.c,v 7.1 91/02/22 09:35:58 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/oid_free.c,v 7.1 91/02/22 09:35:58 mrose Interim $
 *
 *
 * $Log:	oid_free.c,v $
 * Revision 7.1  91/02/22  09:35:58  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:52  mrose
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

int	oid_free (oid)
register OID oid;
{
    if (oid == NULLOID)
	return;

    if (oid -> oid_elements)
	free ((char *) oid -> oid_elements);

    free ((char *) oid);
}
