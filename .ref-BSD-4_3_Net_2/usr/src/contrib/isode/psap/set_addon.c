/* set_addon.c - add member to end of a set */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/set_addon.c,v 7.1 91/02/22 09:36:57 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/set_addon.c,v 7.1 91/02/22 09:36:57 mrose Interim $
 *
 *
 * $Log:	set_addon.c,v $
 * Revision 7.1  91/02/22  09:36:57  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:39  mrose
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

int	set_addon (pe, last, new)
PE	pe,
        last,
        new;
{
    if (pe == NULLPE)
	return NOTOK;
    if (last == NULLPE)
	return set_add (pe, new);
    new -> pe_offset = pe -> pe_cardinal++;
    last -> pe_next = new;

    return OK;
}
