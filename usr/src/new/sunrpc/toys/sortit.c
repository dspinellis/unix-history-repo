/*
 * sortit.c
 * Client side application which sorts argc, argv.
 */
#include <stdio.h>
#include <rpc/rpc.h>
#include "sort_prot.h"

main(argc, argv)
	int argc;
	char **argv;
{
	char *machinename;
	struct sortstrings args, res;
	int i;

	if (argc < 2) {
		fprintf(stderr, "usage: %s machinename [s1 ...]\n", argv[0]);
		exit(1);
	}
	machinename = argv[1];
	args.ns = argc;
	args.s = argv;
	res.s = (char **)NULL;

	callrpc(machinename, SORTPROG, SORTVERS, SORT,
	    xdr_sortstrings, &args, xdr_sortstrings, &res);

	for (i = 0; i < res.ns; i++) {
		printf("%s\n", res.s[i]);
	}

	/* should free res here */
	exit(0);
}

