/* rygenid.c - ROSY: generate unique invoke ID */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosy/RCS/rygenid.c,v 7.1 91/02/22 09:42:01 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosy/RCS/rygenid.c,v 7.1 91/02/22 09:42:01 mrose Interim $
 *
 *
 * $Log:	rygenid.c,v $
 * Revision 7.1  91/02/22  09:42:01  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:53  mrose
 * Release 5.0
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
#include "rosy.h"

/*    generate unique invoke ID */

/* ARGSUSED */

int	RyGenID (sd)
int	sd;
{
    static int	id = 0;

    return (++id);
}
