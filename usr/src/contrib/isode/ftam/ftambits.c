/* ftambits.c - FPM: encode/decode BIT STRINGs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftambits.c,v 7.2 91/02/22 09:22:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftambits.c,v 7.2 91/02/22 09:22:39 mrose Interim $
 *
 *
 * $Log:	ftambits.c,v $
 * Revision 7.2  91/02/22  09:22:39  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:30:06  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:53:25  mrose
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
#include "fpkt.h"

/*  */

PE	bits2fpm (fsb, pairs, actions, fti)
register struct ftamblk *fsb;
struct pair pairs[];
int	actions;
struct FTAMindication *fti;
{
    register struct pair *pp;
    register PE	    fpm;

    if ((fpm = prim2bit (pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_BITS)))
	    == NULLPE) {
no_mem: ;
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
	if (fpm)
	    pe_free (fpm);
	return NULLPE;
    }

    for (pp = pairs; pp -> p_mask; pp++)
	if ((actions & pp -> p_mask) && bit_on (fpm, pp -> p_bitno) == NOTOK)
	    goto no_mem;

    return fpm;
}

/*  */

/* ARGSUSED */

int	fpm2bits (fsb, pairs, fpm, actions, fti)
struct ftamblk *fsb;
struct pair pairs[];
register PE  fpm;
int    *actions;
struct FTAMindication *fti;
{
    register int    i;
	register int	bits_set=0;
    register struct pair *pp;

    i = 0;
    for (pp = pairs; pp -> p_mask; pp++)
	if (bit_test (fpm, pp -> p_bitno) > OK) {
	    i |= pp -> p_mask;
		bits_set++;
	}

    *actions = i;
	if (bits_set == 0)
		return NOTOK;
	else
    	return (bits_set);
}
