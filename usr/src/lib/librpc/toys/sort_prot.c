/*
 * sort_prot.c
 * Implements the protcol filter for the toy sort service.
 */

#include <rpc/rpc.h>
#include "sort_prot.h"

int
xdr_sortstrings(xdrs, ssp)
	XDR *xdrs;
	struct sortstrings *ssp;
{

	return (xdr_array(xdrs, &ssp->s, &ssp->ns, MAXSORTSIZE,
	    sizeof (char *), xdr_wrapstring));
}
