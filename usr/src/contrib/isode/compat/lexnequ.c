/* lexnequ.c - Compare two strings ignoring case upto n octets */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/lexnequ.c,v 7.1 91/02/22 09:15:26 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/lexnequ.c,v 7.1 91/02/22 09:15:26 mrose Interim $
 *
 *
 * $Log:	lexnequ.c,v $
 * Revision 7.1  91/02/22  09:15:26  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:13  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
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

/*  */

lexnequ (str1, str2, len)
register char   *str1,
		*str2;
int             len;
{
    register int count = 1;

    if (str1 == NULL)
	if (str2 == NULL)
		return (0);
	else
		return (1);

    if (str2 == NULL)
	return (-1);

    while (chrcnv[*str1] == chrcnv[*str2++]) {
	if (count++ >= len)
	    return (0);
	if (*str1++ == NULL)
	    return (0);
    }

    str2--;
    if (chrcnv[*str1] > chrcnv[*str2])
	return (1);
    else
	return (-1);
}
