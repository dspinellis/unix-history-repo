#ifndef lint
static	char sccsid[] = "@(#)pcs.c	1.1 (Berkeley) 2/25/86";
#endif

/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"


MSG		NOBKPT;
MSG		SZBKPT;
MSG		EXBKPT;
MSG		NOPCS;
MSG		BADMOD;

/* breakpoints */
BKPTR		bkpthead;

CHAR		*lp;
CHAR		lastc;

INT		signo;
L_INT		dot;
L_INT		pid;
L_INT		cntval;
L_INT		loopcnt;

L_INT		entrypt;
INT		adrflg;



/* sub process control */

subpcs(modif)
{
	REG		check;
	REG		execsig,runmode;
	REG BKPTR	bkptr;
	REG	STRING	comptr;
	execsig=0; loopcnt=cntval;

	switch (modif) {

	    /* delete breakpoint */
	    case 'd': case 'D':
		IF (bkptr=scanbkpt(dot))
		THEN bkptr->flag=0; return;
		ELSE error(NOBKPT);
		FI

	    /* set breakpoint */
	    case 'b': case 'B':
		IF (bkptr=scanbkpt(dot))
		THEN bkptr->flag=0;
		FI
		FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
		DO IF bkptr->flag == 0
		   THEN break;
		   FI
		OD
		IF bkptr==0
		THEN IF (bkptr=(BKPTR)sbrk(sizeof *bkptr)) == (BKPTR)-1
		     THEN error(SZBKPT);
		     ELSE bkptr->nxtbkpt=bkpthead;
			  bkpthead=bkptr;
		     FI
		FI
		bkptr->loc = dot;
		bkptr->initcnt = bkptr->count = cntval;
		bkptr->flag = BKPTSET;
		check=MAXCOM-1; comptr=bkptr->comm; rdc(); lp--;
		REP *comptr++ = readchar();
		PER check-- ANDF lastc!=EOR DONE
		*comptr=0; lp--;
		IF check
		THEN return;
		ELSE error(EXBKPT);
		FI

	    /* exit */
	    case 'k' :case 'K':
		IF pid
		THEN printf("%d: killed", pid); endpcs(); return;
		FI
		error(NOPCS);

	    /* run program */
	    case 'r': case 'R':
		endpcs();
		setup(); runmode=CONTIN;
/*
		IF adrflg 
		THEN IF !scanbkpt(dot) THEN loopcnt++; FI
		ELSE IF !scanbkpt(entrypt+2) THEN loopcnt++; FI
		FI
*/
		break;

	    /* single step */
	    case 's': case 'S':
		IF pid
		THEN
			runmode=SINGLE; execsig=getsig(signo);
		ELSE setup(); loopcnt--;
		FI
		break;

	    /* continue with optional signal */
	    case 'c': case 'C': case 0:
		IF pid==0 THEN error(NOPCS); FI
		runmode=CONTIN; execsig=getsig(signo);
		break;

	    default: error(BADMOD);
	}

	IF loopcnt>0 ANDF runpcs(runmode,execsig)
	THEN printf("breakpoint%16t");
	ELSE printf("stopped at%16t");
	FI
	delbp();
	printpc();
}

