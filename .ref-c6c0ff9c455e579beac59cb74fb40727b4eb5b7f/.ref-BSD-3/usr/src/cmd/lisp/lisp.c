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

main(argc,argv)
char **argv;
{
	lispval temp, matom();
	extern int errp;
	snpand(0);
	
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
	lispval Lread();
	snpand(0);

	if (virgin == 0) {
		fputs("Franz Lisp, Opus 32",poport);
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

exit(code)
{
	extern int fvirgin;
	extern char *stabf;
	if(!fvirgin) unlink(stabf);
	_cleanup();
	proflush();
	_exit(code);
}
