/* xdr_ld.c  - xdr routines for remote dbx interface to VxWorks  */

/*  Copyright 1984, 1985, 1986, 1987, 1988, 1989, 1992 Free Software Foundation, Inc.

    This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01a,05jun90,llk  extracted from xdr_dbx.c.
*/

/*
DESCRIPTION
This module contains the eXternal Data Representation (XDR) routines
for object files that are downloaded to VxWorks.  They are used by
remote debuggers that use RPC (such as dbxWorks and vxGdb).
*/

#include "vxWorks.h"
#include "rpc/rpc.h"
#include "xdr_ld.h"

/* forward declarations */

bool_t xdr_String();   	/* xdr routine for argument list */


/*******************************************************************************
*
* xdr_String - xdr routine for strings.
* 
* Used by xdr_arg_info to handle the actual argument
* strings.  normally calls xdr_string - but does something 
* reasonable encode of null pointer.
*/

bool_t xdr_String (xdrs, strp)
    XDR	*xdrs;
    char **strp;

    {
    if ((*strp == NULL) & (xdrs->x_op == XDR_ENCODE)) 
	return(FALSE);
    else 
	return(xdr_string(xdrs, strp, MAXSTRLEN));
    }
/*******************************************************************************
*
* xdr_ldfile - xdr routine for a single element in the load table 
*/

bool_t xdr_ldfile (xdrs, objp)
    XDR *xdrs;
    ldfile *objp;

    {
    if (! xdr_String(xdrs, &objp->name)) 
	return(FALSE);
    if (! xdr_int(xdrs, &objp->txt_addr)) 
	return(FALSE);
    if (! xdr_int(xdrs, &objp->data_addr)) 
	return(FALSE);
    if (! xdr_int(xdrs, &objp->bss_addr)) 
	return(FALSE);

    return(TRUE);
    }
/*******************************************************************************
*
* xdr_ldtabl -
*
* xdr routine for a list of files and load addresses loaded into VxWorks.
*/

bool_t xdr_ldtabl (xdrs,objp)
    XDR *xdrs;
    ldtabl *objp;

    {
    return (xdr_array (xdrs, (char *) &objp->tbl_ent, (UINT *) &objp->tbl_size, 
	    MAXTBLSZ, sizeof(ldfile), xdr_ldfile));
    }
