/* oid2aei.c - application entity titles -- OID to AE info  */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/oid2aei.c,v 7.1 91/02/22 09:14:48 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/oid2aei.c,v 7.1 91/02/22 09:14:48 mrose Interim $
 *
 *
 * $Log:	oid2aei.c,v $
 * Revision 7.1  91/02/22  09:14:48  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:22:18  mrose
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
#include "isoaddrs.h"

/*  */

AEI	oid2aei (oid)
OID	oid;
{
    static AEInfo aeinfo;
    AEI	    aei = &aeinfo;
    static PE pe = NULLPE;

    if (pe)
	pe_free (pe);

    bzero ((char *) aei, sizeof *aei);
    aei -> aei_ap_title = pe = obj2prim (oid, PE_CLASS_UNIV, PE_PRIM_OID);

    return aei;
}
