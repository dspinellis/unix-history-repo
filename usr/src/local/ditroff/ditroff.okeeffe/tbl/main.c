#ifndef lint
static char sccsid[] = "@(#)main.c	1.1 (CWI) 85/10/01";
#endif lint


 /* t1.c: main control and input switching */

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
	extern int badsig();

	signal(SIGPIPE, badsig);

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

 /* t2.c:  subroutine sequencing for one table */
tableput(){

	switch(device){
	case CAT:
		printf(".\\\" -- device CAT\n");
		break;
	case HARRIS:
		printf(".\\\" -- device HARRIS\n");
		break;
	case DEVVER:
		printf(".\\\" -- device VERSATEC\n");
		break;
	}
printf(".\\\" -- saveline\n");
	saveline();
printf(".\\\" -- savefill\n");
	savefill();
printf(".\\\" -- ifdivert\n");
	ifdivert();
printf(".\\\" -- cleanfc\n");
	cleanfc();
printf(".\\\" -- getcomm\n");
	getcomm();
printf(".\\\" -- getspec\n");
	getspec();
printf(".\\\" -- gettbl\n");
	gettbl();
printf(".\\\" -- getstop\n");
	getstop();
printf(".\\\" -- checkuse\n");
	checkuse();
printf(".\\\" -- choochar\n");
	choochar();
printf(".\\\" -- maktab\n");
	maktab();
printf(".\\\" -- runout\n");
	runout();
printf(".\\\" -- release\n");
	release();
printf(".\\\" -- rstofill\n");
	rstofill();
printf(".\\\" -- endoff\n");
	endoff();
printf(".\\\" -- restline\n");
	restline();
printf(".\\\" -- end off tableput\n");
}
