#ifndef lint
static char sccsid[] = "@(#)3.main.c	4.1	(Berkeley)	%G%";
#endif not lint

#include <stdio.h>
#include "def.h"

structure()
	{
	VERT v, *head;

	if (progress)
		fprintf(stderr,"	getreach:\n");
	getreach();
	if (routerr) return;
	if (progress)
		fprintf(stderr,"	getflow:\n");
	getflow();
	if (progress)
		fprintf(stderr,"	getthen:\n");
	getthen(START);
	head = challoc(nodenum * sizeof(*head));
	for (v = 0; v < nodenum; ++v)
		head[v] = UNDEFINED;
	for (v = START; DEFINED(v); v = RSIB(v))
		fixhd(v,UNDEFINED,head);
			/* fixhd must be called before getloop so that
				it gets applied to IFVX which becomes NXT(w) for UNTVX w */
	if (progress)
		fprintf(stderr,"	getloop:\n");
	getloop();
	if (progress)
		fprintf(stderr,"	getbranch:\n");
	getbranch(head);
	chfree(head,nodenum * sizeof(*head));
	head = 0;
	}
