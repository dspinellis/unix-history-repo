/* prim2oid.c - presentation element to object identifier */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/prim2oid.c,v 7.2 91/02/22 09:36:23 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/prim2oid.c,v 7.2 91/02/22 09:36:23 mrose Interim $
 *
 *
 * $Log:	prim2oid.c,v $
 * Revision 7.2  91/02/22  09:36:23  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/05/22  20:30:03  mrose
 * bug-fix
 * 
 * Revision 7.0  89/11/23  22:13:12  mrose
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

static	int	once_only = 1;
static	OIDentifier oid;

/*  */

OID	prim2oid (pe)
register PE	pe;
{
    register unsigned int i,
			 *ip;
    register PElementData dp,
			  ep;
    register OID	o = &oid;

    if (once_only) {
	bzero ((char *) o, sizeof *o);
	once_only = 0;
    }

    if (pe -> pe_form != PE_FORM_PRIM
	    || (dp = pe -> pe_prim) == NULLPED
	    || pe -> pe_len == 0)
	return pe_seterr (pe, PE_ERR_PRIM, NULLOID);
    ep = dp + pe -> pe_len;

    if (o -> oid_elements) {
	free ((char *) o -> oid_elements);
	o -> oid_elements = NULL;
    }

    for (i = 1; dp < ep; i++) {	/* another whacko OSI encoding... */
	if (*dp == 0x80)
	    return pe_seterr (pe, PE_ERR_OID, NULLOID);

	while (*dp++ & 0x80)
	    if (dp > ep)
		return pe_seterr (pe, PE_ERR_OID, NULLOID);
    }

    if ((ip = (unsigned int *) malloc ((i + 1) * sizeof *ip)) == NULL)
	return pe_seterr (pe, PE_ERR_NMEM, NULLOID);
    o -> oid_elements = ip, o -> oid_nelem = i;
    
    for (dp = pe -> pe_prim; dp < ep; ) {
	i = 0;
	do {
	    i <<= 7; 
	    i |= *dp & 0x7f;
	} while (*dp++ & 0x80);

	if (ip != o -> oid_elements)
	    *ip++ = i;
	else
	    if (i < 40)
		*ip++ = 0, *ip++ = i;
	    else
		if (i < 80)
		    *ip++ = 1, *ip++ = i - 40;
		else
		    *ip++ = 2, *ip++ = i - 80;
    }

    return o;
}

/*  */

#ifdef PEP_TEST
free_oid ()
{
    if (!once_only && oid.oid_elements) {
	free ((char *) oid.oid_elements);
	oid.oid_elements = NULL;
    }
}
#endif
