#ifndef lint
static	char sccsid[] = "@(#)runpcs.c	4.6 %G%";
#endif
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"

extern	MAP	txtmap;

MSG		NOFORK;
MSG		ENDPCS;
MSG		BADWAIT;

CHAR		*lp;
ADDR		sigint;
ADDR		sigqit;

/* breakpoints */
BKPTR		bkpthead;

REGLIST		reglist[];

CHAR		lastc;

INT		fcor;
INT		fsym;
STRING		errflg;
INT		errno;
INT		signo;
INT		sigcode;

L_INT		dot;
STRING		symfil;
INT		wtflag;
L_INT		pid;
L_INT		expv;
INT		adrflg;
L_INT		loopcnt;





/* service routines for sub process control */

getsig(sig)
{	return(expr(0) ? expv : sig);
}

ADDR userpc = 1;

runpcs(runmode,execsig)
{
	INT		rc;
	REG BKPTR	bkpt;
	IF adrflg THEN userpc=dot; FI
	printf("%s: running\n", symfil);

	WHILE --loopcnt>=0
	DO
#ifdef DEBUG
		printf("\ncontinue %x %d\n",userpc,execsig);
#endif
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

		IF (signo==0) ANDF (bkpt=scanbkpt(userpc))
		THEN /* stopped by BPT instruction */
#ifdef DEBUG
			printf("\n BPT code; '%s'%o'%o'%d",
				bkpt->comm,bkpt->comm[0],EOR,bkpt->flag);
#endif
			dot=bkpt->loc;
			IF bkpt->flag==BKPTEXEC
			ORF ((bkpt->flag=BKPTEXEC)
				ANDF bkpt->comm[0]!=EOR
				ANDF command(bkpt->comm,':')
				ANDF --bkpt->count)
			THEN execbkpt(bkpt,execsig); execsig=0; loopcnt++;
			ELSE bkpt->count=bkpt->initcnt; rc=1;
			FI
		ELSE execsig=signo; rc=0;
		FI
	OD
	return(rc);
}

#define BPOUT 0
#define BPIN 1
INT bpstate = BPOUT;

endpcs()
{
	REG BKPTR	bkptr;
	IF pid
	THEN ptrace(PT_KILL,pid,0,0); pid=0; userpc=1;
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
#ifndef VFORK
	IF (pid = fork()) == 0
#else
	IF (pid = vfork()) == 0
#endif
	THEN ptrace(PT_TRACE_ME,0,0,0);
#ifdef VFORK
	     signal(SIGTRAP,nullsig);
#endif
	     signal(SIGINT,sigint); signal(SIGQUIT,sigqit);
	     doexec(); exit(0);
	ELIF pid == -1
	THEN error(NOFORK);
	ELSE bpwait(); readregs(); lp[0]=EOR; lp[1]=0;
	     fsym=open(symfil,wtflag);
	     IF errflg
	     THEN printf("%s: cannot execute\n",symfil);
		  endpcs(); error(0);
	     FI
	FI
	bpstate=BPOUT;
}

execbkpt(bkptr,execsig)
BKPTR	bkptr;
{
#ifdef DEBUG
	printf("exbkpt: %d\n",bkptr->count);
#endif
	delbp();
	ptrace(PT_STEP,pid,bkptr->loc,execsig);
	bkptr->flag=BKPTSET;
	bpwait(); chkerr(); readregs();
}


doexec()
{
	STRING		argl[MAXARG];
	CHAR		args[LINSIZ];
	STRING		p, *ap, filnam;
	extern STRING environ;
	ap=argl; p=args;
	*ap++=symfil;
	REP	IF rdc()==EOR THEN break; FI
		*ap = p;
		/*
		 * First thing is to look for direction characters
		 * and get filename.  Do not use up the args for filenames.
		 * Then get rid of spaces before next args.
		 */
		IF lastc=='<'
		THEN	REP readchar(); PER lastc==SP ORF lastc==TB DONE
			filnam = p;
			WHILE lastc!=EOR ANDF lastc!=SP ANDF lastc!=TB ANDF lastc!='>'
				DO *p++=lastc; readchar(); OD
			*p = 0;
			close(0);
			IF open(filnam,0)<0
			THEN	printf("%s: cannot open\n",filnam); _exit(0);
			FI
			p = *ap;
		ELIF lastc=='>'
		THEN	REP readchar(); PER lastc==SP ORF lastc==TB DONE
			filnam = p;
			WHILE lastc!=EOR ANDF lastc!=SP ANDF lastc!=TB ANDF lastc!='<'
				DO *p++=lastc; readchar(); OD
			*p = '\0';
			close(1);
			IF creat(filnam,0666)<0
			THEN	printf("%s: cannot create\n",filnam); _exit(0);
			FI
			p = *ap;
		ELSE	
			WHILE lastc!=EOR ANDF lastc!=SP ANDF lastc!=TB ANDF lastc!='>' ANDF lastc!='<'
				DO *p++=lastc; readchar(); OD
			*p++ = '\0';
	 		ap++;
		FI
	PER lastc!=EOR DONE
	*ap++=0;
	exect(symfil, argl, environ);
	perror(symfil);
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
				IF a < txtmap.e1 THEN
					ptrace(PT_WRITE_I,pid,a,bkptr->ins);
				ELSE
					ptrace(PT_WRITE_D,pid,a,bkptr->ins);
				FI
			FI
		OD
		bpstate=BPOUT;
	FI
}

#ifdef vax
#define	SETBP(ins)	(BPT | ((ins) &~ 0xFF))
#endif

setbp()
{
	REG ADDR		a;
	REG BKPTR	bkptr;

	IF bpstate!=BPIN
	THEN
		FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
		DO IF bkptr->flag
		   THEN a = bkptr->loc;
			IF a < txtmap.e1 THEN
				bkptr->ins = ptrace(PT_READ_I, pid, a, 0);
				ptrace(PT_WRITE_I, pid, a, SETBP(bkptr->ins));
			ELSE
				bkptr->ins = ptrace(PT_READ_D, pid, a, 0);
				ptrace(PT_WRITE_D, pid, a, SETBP(bkptr->ins));
			FI
			IF errno
			THEN prints("cannot set breakpoint: ");
			     psymoff(bkptr->loc,ISYM,"\n");
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
	WHILE (w = wait(&stat))!=pid ANDF w != -1 DONE
	signal(SIGINT,sigint);
	IF w == -1
	THEN pid=0;
	     errflg=BADWAIT;
	ELIF (stat & 0177) != 0177
	THEN sigcode = 0;
	     IF signo = stat&0177
	     THEN sigprint();
	     FI
	     IF stat&0200
	     THEN prints(" - core dumped");
		  close(fcor);
		  setcor();
	     FI
	     pid=0;
	     errflg=ENDPCS;
	ELSE signo = stat>>8;
	     sigcode = ptrace(PT_READ_U, pid, &((struct user *)0)->u_code, 0);
	     IF signo!=SIGTRAP
	     THEN sigprint();
	     ELSE signo=0;
	     FI
	     flushbuf();
	FI
}

readregs()
{
	/*get REG values from pcs*/
	REG i;
	FOR i=24; --i>=0; 
	DO *(ADDR *)(((ADDR)&u)+reglist[i].roffs) =
		    ptrace(PT_READ_U, pid, reglist[i].roffs, 0);
	OD
 	userpc= *(ADDR *)(((ADDR)&u)+PC);
}


