/* serror.c - get system error */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/serror.c,v 7.2 91/02/22 09:15:47 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/serror.c,v 7.2 91/02/22 09:15:47 mrose Interim $
 *
 *
 * $Log:	serror.c,v $
 * Revision 7.2  91/02/22  09:15:47  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/11  07:09:12  mrose
 * jpo
 * 
 * Revision 7.0  89/11/23  21:23:25  mrose
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

/*    DATA */

extern	int sys_nerr;
extern  char *sys_errlist[];

/*  */

char   *sys_errname (i)
int	i;
{
    static char buffer[30];

    if (0 < i && i < sys_nerr)
	return sys_errlist[i];
    (void) sprintf (buffer, "Error %d", i);

    return buffer;
}
