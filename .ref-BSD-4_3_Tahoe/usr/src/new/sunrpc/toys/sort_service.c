/*
 * sort_service.c
 * Implements the server side of the sort_service.
 */

#include <rpc/rpc.h>
#include "sort_prot.h"

static int
comparestrings(sp1, sp2) 
	char **sp1, **sp2;
{

	return (strcmp(*sp1, *sp2));
}

static struct sortstrings *
sort(ssp)
	struct sortstrings *ssp;
{

	qsort(ssp->s, ssp->ns, sizeof (char *), comparestrings);
	return(ssp);
}

main()
{

	/* register the serive */
	registerrpc(SORTPROG, SORTVERS, SORT,
	    sort, xdr_sortstrings, xdr_sortstrings);

	/* run the service forever */
	svc_run();  /* never returns */
	exit(1);
}

