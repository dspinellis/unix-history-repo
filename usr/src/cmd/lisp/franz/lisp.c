static char *sccsid = "@(#)lisp.c	35.5 7/1/81";

#include	"global.h"
#include	"frame.h"

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
	extern struct frame *errp;
	extern int holbeg,holend,usehole;
	extern int *curhbeg;
	
	environ = arge;
	setbuf(stdout,_sobuf);
	Xargc = argc;
	Xargv = argv;
	virgin = 0;
	errp = (struct frame *)0;
	initial();

	errp = Pushframe(F_RESET);
	switch(retval)
	{
	case C_RESET: break;	/* what to do? */
	case C_INITIAL: break;	/* first time  */
	}

	for(EVER) {
		lbot = np = orgnp;
		rlevel = 0;
		depth = 0;
		clearerr(piport = stdin);
		clearerr(poport = stdout);
		vmsautors();
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

/* franzexit :: give up the ghost
 * this function is called whenever one decides to kill this process. 
 * We clean up a bit then call then standard exit routine.  C code 
 * in franz should never call exit() directly.
 */
franzexit(code)
{
	extern int fvirgin;
	extern char *stabf;
	if(!fvirgin) unlink(stabf);	/* give up any /tmp symbol tables */
	exit(code);
/* is this something special??	_cleanup();
 *			        proflush();
 *				_exit(code);
 */
				
}
/*
 *	This code implements the VMS autorestore feature:
 *	Specifying lisp -r name will cause name to be
 *	restorelisp'd and both -r and name to be deleted
 *	from the lisp argv list.
 */
static vmsautors()
{
#ifdef VMS
      static int VMSrestore_done = 0;
      if (VMSrestore_done == 0){
	    VMSrestore_done = 1;
	    if ( (Xargc > 2) && (strcmp(Xargv[1],"-r") == 0) ) {
		    np++->val = matom(Xargv[2]);
		    Xargv[2] = Xargv[0];
		    Xargv++; Xargv++;
		    Xargc -= 2;
		    Lrestlsp();
	    }
      }
#endif
}
