#ifndef lint
static char *rcsid =
   "$Header: inits.c,v 1.7 85/03/24 11:03:12 sklower Exp $";
#endif

/*					-[Sat Jan 29 12:59:39 1983 by jkf]-
 * 	inits.c				$Locker:  $
 * initialization routines
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include <signal.h>
#include "frame.h"

/* initial
 * initializes the parts of the system that cannot be automatically
 * accomplished in the declarations.
 */
 
int reborn=0;	/*  flag to tell whether we are in fast-load version  */
extern char *stabf;
extern int fvirgin;
extern int keywait;
extern sigstruck, sigdelay;
initial()
{
	int sigalrmh(), sigfpeh(),  sginth();
	lispval Isstatus(),Istsrch();
	extern int hashtop;

	/* clear any memory of pending SIGINT's */
	exception = FALSE;
	sigintcnt = 0;

	if( signal(SIGINT,SIG_IGN) != SIG_IGN)
	      signal(SIGINT,sginth);
	if( signal(SIGHUP,SIG_IGN) != SIG_IGN)
	      signal(SIGHUP,sginth);
	signal(SIGFPE,sginth);
	signal(SIGALRM,sginth);
	signal(SIGPIPE,sginth);
	/* signals SIGBUS and SIGSEGV will be set up when the status list
	   is set up when the lisp is virgin, and will be set up according
	   to the current value on the status list if the lisp is reborn
	*/

#ifdef SPISFP
	{extern long *exsp; xsp = exsp;}
#endif

	if( reborn ) {
		np = lbot = orgnp;
		Nioreset();
		stabf = 0;
		fvirgin = 1;
		loading->a.clb = nil;
		gcrebear();

		/* set up SIGBUS and SIGSEGV from current value 
		   of status flag dumpcore
		*/
		Isstatus(matom("dumpcore"),
			 (Istsrch(matom("dumpcore")))->d.cdr->d.cdr->d.cdr);

		makenv();
		return;
	}
	for (hash=0;hash<hashtop;hash++) hasht[hash] = (struct atom *) CNIL;
	
	sbrk( LBPG-(((int)sbrk(0)) % LBPG) );	/* even up the break */
	makevals();

	orgnp = np;
	makenv();

}

static
makenv()
{
	register lispval env, temp;
	register char *p, *q;
	char **envp, envstr[STRBLEN];
	extern char **environ;

	lbot = np;
	env = nil;
	np++->val = env;
	for (envp=environ; *envp!=NULL; envp++) ;
	while (--envp >= environ) {
		for(p= *envp,q=envstr; *p!='=' ; p++)
			if(q < envstr + STRBLEN)
				*q++ = *p;
		*q = 0; p++;
		/* at this point lbot->val==env, so it is protected
		   from gc */
		lbot->val = temp = newdot();
		temp->d.cdr = env;
		env = temp;
		temp = newdot();
		env->d.car = temp;
		temp->d.car = matom(envstr);
		temp->d.cdr = matom(p);
	}
	matom("environment")->a.clb = env;
	np--;
}

sginth(signo){
	re_enable(signo,sginth);
	sigstruck |= (1 << signo);
	/* handle SIGINT differently since it is the only
	   asychronous interrupt we handle		*/
	if( signo == SIGINT) {
	    if( ++sigintcnt == 1)
	    {  /* if this is the first interrupt, we just set a flag
		  which will be checked in qfuncl and eval.  This will
		  allow us to handle these interrupts when we are
		  ready.
	       */
	       exception = TRUE;
	       /*putchar('A');*/
	       fflush(stdout);
	       sigstruck &= ~(1 << signo);
	       return;
	    }
	    else if (sigintcnt == 2)
	    {  /* the setting of  exception was ignored, we better
		  make sure that all calls from compiled code
		  go through qlinker
		*/
		signal(SIGINT,SIG_IGN);  /* this may take a while, dont allow ints*/
		clrtt(0);
		/*putchar('B');*/
		fflush(stdout);
		signal(SIGINT,sginth);  /* ok to interrupt again */
		sigstruck &= ~(1 << signo);
		return;
	    }
	    else {
		/*putchar('C');*/
		fflush(stdout);
	    }
	}

	sigcall(signo);
}
sigcall(which)
register which;
{
	extern lispval Lfuncal();
	Savestack(1);

	if(which == SIGINT) { sigintcnt = 0; exception = 0; }

	if(sigacts[which]!=((lispval) 0)) {
		pbuf pb;
		int mustpop = 0;
		if(errp && errp->class==F_TO_FORT) {
			np = errp->svnp;
			mustpop = 1;
			errp = Pushframe(F_TO_LISP,nil,nil);
		}
		lbot = np;
		np -> val = sigacts[which];
		INRNP;
		np -> val = inewint((long)which);
		INRNP;
	{lispval temp;temp = rdrsdot, rdrsdot = rdrsdot2, rdrsdot2 = temp; /*KLUDGE*/}
		Lfuncal();
		if (mustpop) errp = Popframe();
	{lispval temp;temp = rdrsdot, rdrsdot = rdrsdot2, rdrsdot2 = temp; /*KLUDGE*/}
	}
	sigstruck &= ~ (1<<which);
	Restorestack();
}
delayoff(){
	sigdelay = FALSE;
	if(sigstruck)
		dosig();
}
dosig()
{
	register int i; int which;
	if(!sigdelay) 
		for(which=0, i = 1;  i <= 65536;  which++,i<<=1) {
			keywait = FALSE;
			if(sigstruck & i)
				sigcall(which);
		}
}
badmr(number)
{
	signal(number,badmr);
	fflush(stdout);
	error("Internal bad memory reference, you are advised to (reset).",FALSE);
}

#define	mask(s)	(1 << ((s)-1))
static
re_enable(signo,handler)
int (*handler)();
{
#if (os_4_2| os_4_3)
	sigsetmask(sigblock(0) &~ mask(signo));
#else
	signal(signo,handler);
#endif
}
