static char *sccsid = "@(#)lisp.c	34.1 10/3/80";

#include	"global.h"

/* main *****************************************************************/
/* Execution of the lisp system begins here.  This is the top level	*/
/* executor which is an infinite loop.  The structure is similar to	*/
/* error.								*/

extern char _sobuf[];
extern lispval reborn;
extern int rlevel;
static int virgin = 0;
int	Xargc;
char	**Xargv;
extern int environ;

main(argc,argv,arge)
char **argv;
{
	lispval temp, matom();
	extern int errp;
	extern int holbeg,holend,usehole;
	extern int *curhbeg;

	snpand(0);
	
	environ = arge;
	setbuf(stdout,_sobuf);
	Xargc = argc;
	Xargv = argv;
	virgin = 0;
	initial();
/* printf("poport = 0%o\n",poport); */
	while(retval = setexit())
		switch (retval)	{

		case BRGOTO:	error("GOTO LABEL NOT FOUND",FALSE);

		case BRRETN:	error("NO PROG TO RETURN FROM",FALSE);
		
		case BRRETB:
		default:	popnames(orgbnp);

	}
	for(EVER) {
		lbot = np = orgnp;
		rlevel = 0;
		depth = 0;
		errp = 0;
		clearerr(piport = stdin);
		clearerr(poport = stdout);
		np++->val = matom("top-level");
		np++->val = nil;
		Lapply();
	}
}
Ntpl()
{
	lispval Lread(),Istsrch();
	snpand(0);

	if (virgin == 0) {
		fputs(Istsrch(matom("version"))->d.cdr->d.cdr->d.cdr,poport);
		virgin = 1;
	}
	lbot = np;
	np++->val = P(stdin);
	np++->val = eofa;
	while (TRUE)
		{
		fputs("\n-> ",stdout);
		dmpport(stdout);
		vtemp = Lread();
		if(vtemp == eofa) exit(0);
		printr(eval(vtemp),stdout);
		}
	}

#ifndef VMS
exit(code)
{
	extern int fvirgin;
	extern char *stabf;
	if(!fvirgin) unlink(stabf);
	_cleanup();
	proflush();
	_exit(code);
}
#endif
