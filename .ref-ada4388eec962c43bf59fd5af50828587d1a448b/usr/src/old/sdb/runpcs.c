static	char sccsid[] = "@(#)runpcs.c 4.2 %G%";
#
/*
 *
 *	UNIX debugger
 *
 */

#include "head.h"
#include <a.out.h>
#include <stab.h>
struct user u;
#include <stdio.h>

#ifndef SIGTRAP
#define	SIGTRAP SIGTRC
#endif

MSG		NOFORK;
MSG		ENDPCS;
MSG		BADWAIT;

ADDR		sigint;
ADDR		sigqit;
ADDR		userpc;

/* breakpoints */
BKPTR		bkpthead;

CHAR		lastc;

INT		fcor;
INT		fsym;
STRING		errflg;
int		errno;
INT		signo;

L_INT		dot;
STRING		symfil;
INT		wtflag;
INT		pid;
INT		adrflg;
L_INT		loopcnt;






getsig(sig)
{	return(sig);
}

runpcs(runmode,execsig)
{
	REG BKPTR	bkpt;
	IF adrflg THEN userpc=dot; FI
	WHILE --loopcnt>=0
	DO
		if (debug) printf("\ncontinue %x %d\n",userpc,execsig);
		IF runmode==SINGLE
		THEN delbp(); /* hardware handles single-stepping */
		ELSE /* continuing from a breakpoint is hard */
			IF bkpt=scanbkpt(userpc)
			THEN execbkpt(bkpt,execsig); execsig=0;
			FI
			setbp();
		FI
		ptrace(runmode,pid,userpc,execsig);
		bpwait(); chkerr(); execsig=0; delbp(); readregs();

	loop1:	IF (signo==0) ANDF (bkpt=scanbkpt(userpc))
		THEN /* stopped by BPT instruction */
			if (debug) printf("\n BPT code; '%s'%o'%o'%d",
				bkpt->comm,bkpt->comm[0],EOR,bkpt->flag);
			dot=bkpt->loc;
			IF bkpt->comm[0] != EOR
			THEN acommand(bkpt->comm);
			FI
			IF bkpt->flag==BKPTEXEC
			ORF ((bkpt->flag=BKPTEXEC)
				ANDF bkpt->comm[0]!=EOR)
			THEN execbkpt(bkpt,execsig); execsig=0; loopcnt++;
			     goto loop1;
 			ELSE bkpt->flag=BKPTSET; bkpt->count=bkpt->initcnt;
			FI
		ELSE execsig=signo;
		     if (execsig) break;
		FI
	OD
 		if (debug) printf("Returning from runpcs\n");
}

#define BPOUT 0
#define BPIN 1
INT bpstate;

endpcs()
{
	REG BKPTR	bkptr;
 		if (debug) printf("Entering endpcs with pid=%d\n");
	IF pid
	THEN ptrace(EXIT,pid,0,0); pid=0; userpc=1;
	     FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
	     DO IF bkptr->flag
		THEN bkptr->flag=BKPTSET;
		FI
	     OD
	FI
	bpstate=BPOUT;
}

#ifdef VFORK
nullsig()
{

}
#endif

setup()
{
	close(fsym); fsym = -1;
#ifdef VFORK
	IF (pid = vfork()) == 0
#else
	IF (pid = fork()) == 0
#endif
	THEN ptrace(SETTRC,0,0,0);
	     signal(SIGINT,sigint); signal(SIGQUIT,sigqit);
#ifdef VFORK
	     signal(SIGTRAP,nullsig);
#endif
 		if (debug) printf("About to doexec  pid=%d\n",pid);
	     doexec(); _exit(0);
	ELIF pid == -1
	THEN error(NOFORK);
	ELSE bpwait(); readregs();
	if (debug) printf("About to open symfil = %s\n", symfil);
	     fsym=open(symfil,wtflag);
	     IF errflg
	     THEN printf("%s: cannot execute\n",symfil);
 		if (debug) printf("%d %s\n", errflg, errflg);
		  endpcs();
	     FI
	FI
	bpstate=BPOUT;
}

execbkpt(bkptr,execsig)
BKPTR	bkptr;
{
	if (debug) printf("exbkpt: %d\n",bkptr->count);
	delbp();
	ptrace(SINGLE,pid,bkptr->loc,execsig);
	bkptr->flag=BKPTSET;
	bpwait(); chkerr(); readregs();
}

extern STRING environ;

doexec()
{
	char *argl[MAXARG], args[LINSIZ];
	register char c, redchar, *argsp, **arglp, *filnam;

	arglp = argl;
	argsp = args;
	*arglp++ = symfil;
	c = ' ';

	do {
		while (eqany(c, " \t")) {
			c = rdc();
		} 
		if (eqany(c, "<>")) {
			redchar = c;
			do {
				c = rdc();
			} while (eqany(c, " \t"));
			filnam = argsp;
			do {
				*argsp++ = c;
				c = rdc();
			} while (!eqany(c, " <>\t\n"));
			*argsp++ = '\0';
			if (redchar == '<') {
				close(0);
				if (open(filnam,0) < 0) {
					printf("%s: cannot open\n",filnam);
					fflush(stdout);
					_exit(0);
				}
			} else {
				close(1);
				if (creat(filnam,0666) < 0) {
					printf("%s: cannot create\n",filnam);
					fflush(stdout);
					 _exit(0);
				}
			}
		} else if (c != '\n') {
			*arglp++ = argsp;
			do {
				*argsp++ = c;
				c = rdc();
			} while(!eqany(c, " <>\t\n"));
			*argsp++ = '\0';
		}
	} while (c != '\n');
	*arglp = (char *) 0;
	if (debug) {
		char **dap;
		printf("About to exect(%s, %d, %d)\n",symfil,argl,environ);
		for (dap = argl; *dap; dap++) {
			printf("%s, ", *dap);
		}
	}
	exect(symfil, argl, environ);
	perror("Returned from exect");
}

BKPTR	scanbkpt(adr)
ADDR adr;
{
	REG BKPTR	bkptr;
	FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
	DO IF bkptr->flag ANDF bkptr->loc==adr
	   THEN break;
	   FI
	OD
	return(bkptr);
}

delbp()
{
	REG ADDR	a;
	REG BKPTR	bkptr;
	IF bpstate!=BPOUT
	THEN
		FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
		DO	IF bkptr->flag
			THEN a=bkptr->loc;
				ptrace(WIUSER,pid,a,
					(bkptr->ins&0xFF)|(ptrace(RIUSER,pid,a,0)&~0xFF));
			FI
		OD
		bpstate=BPOUT;
	FI
}

setbp()
{
	REG ADDR		a;
	REG BKPTR	bkptr;

	IF bpstate!=BPIN
	THEN
		FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
		DO IF bkptr->flag
		   THEN a = bkptr->loc;
			bkptr->ins = ptrace(RIUSER, pid, a, 0);
			ptrace(WIUSER, pid, a, BPT | (bkptr->ins&~0xFF));
			IF errno
			THEN error("cannot set breakpoint: ");
			     printf("%s:%d @ %d\n", adrtoprocp(dot)->pname,
				adrtolineno(dot), dot);
			FI
		   FI
		OD
		bpstate=BPIN;
	FI
}

bpwait()
{
	REG ADDR w;
	ADDR stat;

	signal(SIGINT, 1);
	if (debug) printf("Waiting for pid %d\n",pid);
	WHILE (w = wait(&stat))!=pid ANDF w != -1 DONE
	if (debug) printf("Ending wait\n");
	if (debug) printf("w = %d; pid = %d; stat = %o;\n", w,pid,stat);
	signal(SIGINT,sigint);
	IF w == -1
	THEN pid=0;
	     errflg=BADWAIT;
	ELIF (stat & 0177) != 0177
	THEN IF signo = stat&0177
	     THEN sigprint();
	     FI
	     IF stat&0200
	     THEN error(" - core dumped");
		  close(fcor);
		  setcor();
	     FI
	     pid=0;
	     errflg=ENDPCS;
	ELSE signo = stat>>8;
    	     if (debug) printf("PC = %d, dbsubn = %d\n",
		ptrace(RUREGS, pid, PC, 0), extaddr("_dbsubn")); 
	     IF signo!=SIGTRAP ANDF
		ptrace(RUREGS, pid, PC, 0) != extaddr("_dbsubn")
	     THEN sigprint();
	     ELSE signo=0;
	     FI
	FI
}

REGLIST reglist[];
readregs()
{
	/*get REG values from pcs*/
	REG i;
	FOR i=24; --i>=0; 
	DO *(ADDR *)(((ADDR)&u)+reglist[i].roffs) =
		    ptrace(RUREGS, pid, reglist[i].roffs, 0);
	OD
 	userpc= *(ADDR *)(((ADDR)&u)+PC);
}

char 
readchar() {
	lastc = *argsp++;
	if (lastc == '\0') lastc = '\n';
	return(lastc);
}

char
rdc()
{
	register char c;

	c = *argsp++;
	return(c == '\0' ? '\n' : c);
}
