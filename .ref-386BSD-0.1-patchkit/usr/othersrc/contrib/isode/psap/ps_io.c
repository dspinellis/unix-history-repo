/* ps_io.c - presentation stream I/O dispatch */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/ps_io.c,v 7.1 91/02/22 09:36:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/ps_io.c,v 7.1 91/02/22 09:36:38 mrose Interim $
 *
 *
 * $Log:	ps_io.c,v $
 * Revision 7.1  91/02/22  09:36:38  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:24  mrose
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

int	ps_io (ps, io, data, n, in_line)
register PS	ps;
register IFP	io;
register PElementData data;
register PElementLen n;
int	in_line;
{
    register int    cc;

    if (io == NULLIFP)
	return ps_seterr (ps, PS_ERR_EOF, NOTOK);

    while (n > 0)
	switch (cc = (*io) (ps, data, n, in_line)) {
	    case NOTOK: 
		return NOTOK;

	    case OK: 
		return ps_seterr (ps, PS_ERR_EOF, NOTOK);

	    default: 
		data += cc, n -= cc;
		ps -> ps_byteno += cc;
		break;
	}

    return OK;
}
