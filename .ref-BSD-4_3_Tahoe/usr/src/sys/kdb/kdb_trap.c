/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_trap.c	7.5 (Berkeley) 12/15/86
 */

/*
 * Trap handler - command loop entry point.
 */
#include "../kdb/defs.h"

char	*NOEOR;

int	executing;
char	*lp;

char	lastc;

ADDR	userpc;
int	lastcom;

ADDR	maxoff = MAXOFF;
long	maxpos = MAXPOS;

/*
 * Kdb trap handler; entered on all fatal
 * and/or debugger related traps or faults.
 */
kdb(type, code, curproc)
	int type, code;
	struct proc *curproc;
{

	var[varchk('t')] = type;
	var[varchk('c')] = code;
	var[varchk('p')] = (int)curproc;
	printtrap((long)type, (long)code);
	userpc = dot = pcb.pcb_pc;
	switch (setexit()) {

	case SINGLE:
		setsstep();		/* hardware single step */
		/* fall thru... */
	case CONTIN:
		return (1);
	case 0:
		if (nextpcs(type))
			printf("breakpoint%16t");
		else
			printf("stopped at%16t");
		printpc();
		break;
	}
	if (executing)
		delbp();
	executing = 0;
	for (;;) {
		flushbuf();
		if (errflg) {
			printf("%s\n", errflg);
			errflg = 0;
		}
		if (mkfault) {
			mkfault=0;
			printc('\n');
			printf(DBNAME);
		}
		kdbwrite("kdb> ", 5);
		lp=0; (void) rdc(); lp--;
		(void) command((char *)0, lastcom);
		if (lp && lastc!='\n')
			error(NOEOR);
	}
}

/*
 * If there has been an error or a fault, take the error.
 */
chkerr()
{
	if (errflg || mkfault)
		error(errflg);
}

/*
 * An error occurred; save the message for
 * later printing, and reset to main command loop.
 */
error(n)
	char *n;
{

	errflg = n;
	reset(ERROR);
}
