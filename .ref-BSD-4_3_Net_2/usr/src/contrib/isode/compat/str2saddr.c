/* str2saddr.c - string value to SSAPaddr */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/str2saddr.c,v 7.1 91/02/22 09:15:56 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/str2saddr.c,v 7.1 91/02/22 09:15:56 mrose Interim $
 *
 *
 * $Log:	str2saddr.c,v $
 * Revision 7.1  91/02/22  09:15:56  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:36  mrose
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
#include "isoaddrs.h"

/*  */

struct SSAPaddr *str2saddr (str)
char   *str;
{
    register struct PSAPaddr *pa;

    if (pa = str2paddr (str))
	return (&pa -> pa_addr);

    return NULLSA;
}
