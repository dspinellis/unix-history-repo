/* seq_addon.c - add a member to the end of a sequence (efficiency hack) */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/seq_addon.c,v 7.1 91/02/22 09:36:52 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/seq_addon.c,v 7.1 91/02/22 09:36:52 mrose Interim $
 *
 *
 * $Log:	seq_addon.c,v $
 * Revision 7.1  91/02/22  09:36:52  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:36  mrose
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

int	seq_addon (pe, last, new)
PE	pe,
        last,
        new;
{
    if (pe == NULLPE)
	return NOTOK;
    if (last == NULLPE)
	return seq_add (pe, new, -1);
    new -> pe_offset = pe -> pe_cardinal++;
    last -> pe_next = new;
    return OK;
}
