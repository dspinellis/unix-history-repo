/* py_advise.c - standard "advise" routine for pepsy/pepy */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/py_advise.c,v 7.2 91/02/22 09:50:02 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/py_advise.c,v 7.2 91/02/22 09:50:02 mrose Interim $
 *
 *
 * $Log:	py_advise.c,v $
 * Revision 7.2  91/02/22  09:50:02  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:53:19  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:12:05  mrose
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
#include <varargs.h>

/*  */

#ifndef	lint
char   PY_pepy[BUFSIZ];


void	PY_advise (va_alist)
va_dcl
{
    va_list	ap;

    va_start (ap);

    asprintf (PY_pepy, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	PY_advise (what, fmt)
char   *what,
       *fmt;
{
    PY_advise (what, fmt);
}
#endif
