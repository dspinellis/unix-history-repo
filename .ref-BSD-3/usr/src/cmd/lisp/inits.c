#include "global.h"
#include <signal.h>
/************************************************************************/
/*                                                                      */
/*   file: inits.i                                                      */
/*   contents: initialization routines                                  */
/*                                                                      */


/* initial **************************************************************/
/* initializes the parts of the system that cannot be automatically	*/
/* accomplished in the declarations.					*/

int reborn=0;	/*  flag to tell whether we are in fast-load version  */
extern char *stabf;
extern int fvirgin;
extern int keywait;
extern sigstruck, sigdelay;
initial()
{
	int sigalrmh(), sigfpeh(),  siginth();
	lispval Isstatus(),Istsrch();

	if( signal(SIGINT,SIG_IGN) != SIG_IGN)
	      signal(SIGINT,siginth);
	if( signal(SIGHUP,SIG_IGN) != SIG_IGN)
	      signal(SIGHUP,siginth);
	signal(SIGFPE,siginth);
	signal(SIGALRM,siginth);
	/* signals SIGBUS and SIGSEGV will be set up when the status list
	   is set up when the lisp is virgin, and will be set up according
	   to the current value on the status list if the lisp is reborn
	*/

	if( reborn ) {
		register FILE *p = _iob + 3;
		static FILE empty;
		for(; p < _iob + _NFILE; p++)
			*p = empty;
		np = lbot = orgnp;
		stabf = 0;
		fvirgin = 1;
		loading->clb = nil;

		/* set up SIGBUS and SIGSEGV from current value 
		   of status flag dumpcore
		*/
		Isstatus(matom("dumpcore"),
			 (Istsrch(matom("dumpcore")))->cdr->cdr->cdr);

		makenv();
		return;
	}
	for (hash=0;hash<HASHTOP;hash++) hasht[hash] = (struct atom *) CNIL;
	
	sbrk( NBPG-(((int)sbrk(0)) % NBPG) );	/* even up the break */
	makevals();

	orgnp = np;
	makenv();

}

static
makenv()
{
	register lispval env, temp;
	register char *p, *q;
	register struct argent *lbot, *np;
	char **envp, envstr[64];
	extern char **environ;

	lbot = np;
	env = nil;
	np++->val = env;
	for (envp=environ; *envp!=NULL; envp++) ;
	while (--envp >= environ) {
		for(p= *envp,q=envstr; (*q++ = *p++)!='=';);
		*--q = 0;
		/* at this point lbot->val==env, so it is protected
		   from gc */
		lbot->val = temp = newdot();
		temp->cdr = env;
		env = temp;
		temp = newdot();
		temp->car = matom(envstr);
		temp->cdr = matom(p);
		env->car = temp;
	}
	matom("environment")->clb = env;
}

siginth(signo){
	signal(signo,siginth);
	sigstruck |= (1 << signo);
	/*if(signo==SIGBUS || signo==SIGBUS || keywait)*/
		sigcall(signo);
}
sigcall(which)
register which;
{
	extern lispval Lfuncal();
	extern lispval sigacts[16];
	struct argent *oldlbot, *oldnp, saved;

	if(sigacts[which]!=((lispval) 0)) {
		oldlbot = lbot;
		oldnp = np;
		lbot = np;
		np -> val = sigacts[which];
		INRNP;
		np -> val = inewint(which);
		INRNP;
		Lfuncal();
		lbot = oldlbot;
		np = oldnp;
	}
	sigstruck &= ~ (1<<which);
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
badmemr(number)
{
	signal(number,badmemr);
	fflush(stdout);
	error("Internal bad memory reference, you are advised to (reset).",FALSE);
}
