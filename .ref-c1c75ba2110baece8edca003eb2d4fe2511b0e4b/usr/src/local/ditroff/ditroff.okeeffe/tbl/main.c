#ifndef lint
static char sccsid[] = "@(#)main.c	1.4 (CWI) 86/11/10";
#endif lint

/*
 * tbl troff preprocessor.
 *
 * Tidied, and changed by jna
 *
 */

#include <signal.h>
#include "defs.h"
#include "ext.h"

static int     sargc;
static char  **sargv;

main(argc, argv)
int	argc; char   *argv[];
{
	char line[BUFSIZ];
	register char *p;
	char *getenv();
	extern int badsig();

	signal(SIGPIPE, badsig);

	if (p = getenv("TYPESETTER")) {
		if(strcmp(p, "har") == 0)
			device = HARRIS;
		else if(strcmp(p, "ver") == 0)
			device = DEVVER;
		else if(strcmp(p, "psc") == 0)
			device = DEVPSC;
		/* johan doesnt want to see error messages in a context like
			TYPESETTER=1650.10 tbl files | eqn | nroff -ms
		so I removed the warning here. Besides, there is no complaint
		for an option -Tfoo either. */
	}

	tabin = stdin;
	sargv = ++argv;
	if((sargc = --argc) > 0)
		(void) swapin();

	while(gets1(line)){
		printf("%s\n", line);
		if(prefix(".TS", line))
			tableput();
	}
	fclose(tabin);
	return(0);
}

swapin()
{
	while(sargc > 0 && **sargv == '-'){
		if(strcmp("-ms", *sargv) == 0){
			*sargv = MACROS;
			break;
		}
		if(strcmp("-mm", *sargv) == 0){
			*sargv = PYMACS;
			break;
		}
		if(strcmp("-TX", *sargv) == 0){
			pr1403 = 1;
			sargc--;
			sargv++;
			break;
		}
		if(strcmp("-Thar", *sargv) == 0){
			device = HARRIS;
			sargc--;
			sargv++;
			break;
		}
		if(strcmp("-Tver", *sargv) == 0){
			device = DEVVER;
			sargc--;
			sargv++;
			break;
		}
		if(strcmp("-Tpsc", *sargv) == 0){
			device = DEVPSC;
			sargc--;
			sargv++;
			break;
		}
		if(strcmp("-d", *sargv) == 0){
			dbg++;
			sargc--;
			sargv++;
			break;
		}
		sargc--;
		sargv++;
	}
	if(sargc <= 0)
		return(0);
	if(tabin != stdin)
		(void) fclose(tabin);
	if(strcmp(*sargv, "-") == 0)
		tabin = stdin;
	else
		tabin = fopen(ifile = *sargv, "r");
	iline = 1;
	printf(".ds f. %s\n", ifile);
	/*
	 * support for .lf request (jna)
	 */
	printf(".lf 1 %s\n", ifile);
	if(tabin == NULL)
		error("Can't open file");
	sargc--;
	sargv++;
	return(1);
}

badsig()
{
	signal(SIGPIPE, SIG_IGN);
	exit(0);
}

tableput(){

	switch(device){
	case CAT:
		dprint(".\\\" -- device CAT\n");
		break;
	case HARRIS:
		dprint(".\\\" -- device HARRIS\n");
		break;
	case DEVPSC:
		dprint(".\\\" -- device PostScript\n");
		break;
	case DEVVER:
		dprint(".\\\" -- device VERSATEC\n");
		break;
	}
	dprint(".\\\" -- saveline\n");
	saveline();
	dprint(".\\\" -- savefill\n");
	savefill();
	dprint(".\\\" -- ifdivert\n");
	ifdivert();
	dprint(".\\\" -- cleanfc\n");
	cleanfc();
	dprint(".\\\" -- getcomm\n");
	getcomm();
	dprint(".\\\" -- getspec\n");
	getspec();
	dprint(".\\\" -- gettbl\n");
	gettbl();
	dprint(".\\\" -- getstop\n");
	getstop();
	dprint(".\\\" -- checkuse\n");
	checkuse();
	dprint(".\\\" -- choochar\n");
	choochar();
	dprint(".\\\" -- maktab\n");
	maktab();
	dprint(".\\\" -- runout\n");
	runout();
	dprint(".\\\" -- release\n");
	release();
	dprint(".\\\" -- rstofill\n");
	rstofill();
	dprint(".\\\" -- endoff\n");
	endoff();
	dprint(".\\\" -- restline\n");
	restline();
	dprint(".\\\" -- end off tableput\n");
}
