/*
 * Copyright (c) 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_trap.c	7.8 (Berkeley) %G%
 */

/*
 * Trap handler - command loop entry point.
 */
#include "../kdb/defs.h"

char	*kdbNOEOR;

int	kdbexecuting;
char	*kdblp;

char	kdblastc;

ADDR	kdbuserpc;
int	kdblastcom;

ADDR	kdbmaxoff = MAXOFF;
long	kdbmaxpos = MAXPOS;

/*
 * Kdb trap handler; entered on all fatal
 * and/or debugger related traps or faults.
 */
kdb(type, code, curproc, kstack)
	int type, code;
	struct proc *curproc;
	int kstack;
{

	kdbvar[kdbvarchk('t')] = type;
	kdbvar[kdbvarchk('c')] = code;
	kdbvar[kdbvarchk('p')] = (int)curproc;
	if (kdbexecuting)
		kdbdelbp();
	kdbexecuting = 0;
	if (kstack)
		kdbprintf("(from kernel stack)\n"); /* after delbp() */
	kdbprinttrap((long)type, (long)code);
	kdbuserpc = kdbdot = kdbpcb.pcb_pc;
	switch (setexit()) {

	case SINGLE:
		setsstep();		/* hardware single step */
		/* fall thru... */
	case CONTIN:
		return (1);
	case PANIC:
		return (0);
	case 0:
		if (kdbnextpcs(type))
			kdbprintf("breakpoint%16t");
		else
			kdbprintf("stopped at%16t");
		kdbprintpc();
		break;
	}
	if (kdbexecuting)
		kdbdelbp();
	kdbexecuting = 0;
	for (;;) {
		kdbflushbuf();
		if (kdberrflg) {
			kdbprintf("%s\n", kdberrflg);
			kdberrflg = 0;
		}
		if (kdbmkfault) {
			kdbmkfault=0;
			kdbprintc('\n');
			kdbprintf(DBNAME);
		}
		kdbwrite("kdb> ", 5);
		kdblp=0; (void) kdbrdc(); kdblp--;
		(void) kdbcommand((char *)0, kdblastcom);
		if (kdblp && kdblastc!='\n')
			kdberror(kdbNOEOR);
	}
}

/*
 * If there has been an error or a fault, take the error.
 */
kdbchkerr()
{
	if (kdberrflg || kdbmkfault)
		kdberror(kdberrflg);
}

/*
 * An error occurred; save the message for
 * later printing, and reset to main command loop.
 */
kdberror(n)
	char *n;
{

	kdberrflg = n;
	reset(ERROR);
}
