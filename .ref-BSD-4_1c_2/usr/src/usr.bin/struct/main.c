#ifndef lint
static char sccsid[] = "@(#)main.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <signal.h>
#include <stdio.h>
#include "1.defs.h"
#include "def.h"


char (*input)(), (*unput)();
FILE *outfd	= stdout;



main(argc,argv)
int argc;
char *argv[];
	{
	int anyoutput;
	int dexit();
	char *getargs();
	char input1(), unput1(), input2(), unput2();
	anyoutput = FALSE;
	getargs(argc,argv);
	if (debug == 2) debfd = stderr;
	else if (debug)
		debfd = fopen("debug1","w");

	if (signal(SIGINT, SIG_IGN) !=SIG_IGN)
		signal(SIGINT,dexit);
	prog_init();

	for (;;)
		{
		++routnum;
		routerr = 0;

		input = input1;
		unput = unput1;
		if (!mkgraph()) break;
		if (debug) prgraph();
		if (routerr) continue;

		if (progress)fprintf(stderr,"build:\n");
		build();
		if (debug) prtree();
		if (routerr) continue;

		if (progress)fprintf(stderr,"structure:\n");
		structure();
		if (debug) prtree();
		if (routerr) continue;
		input = input2;
		unput = unput2;

		if (progress)fprintf(stderr,"output:\n");
		output();
		if (routerr) continue;
		anyoutput = TRUE;
		freegraf();
		}
	if (anyoutput)
		exit(0);
	else
		exit(1);
	}


dexit()
	{
	exit(1);
	}
