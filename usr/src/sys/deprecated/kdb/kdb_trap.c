/*	kdb_trap.c	7.1	86/11/20	*/

/*
 * Trap handler - command loop entry point.
 */
#include "../kdb/defs.h"

char	*NOEOR;

int	executing;
char	*lp;
long	maxpos;

char	lastc;
int	eof;

ADDR	userpc;
int	lastcom;

ADDR	maxoff = MAXOFF;
long	maxpos = MAXPOS;

kdb(type, sp, curproc)
	int type, *sp;
	struct proc *curproc;
{

	userpc = dot = pcb.pcb_pc;
	var[varchk('t')] = (int)sp;
	switch (setexit()) {

	case SINGLE:
		pcb.pcb_psl |= TBIT;
		/* fall thru... */
	case CONTIN:
		return (1);
	case 0:
		if (nextpcs(type, 0))
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
		lp=0; rdc(); lp--;
		if (eof)
			return (1);
		command(0, lastcom);
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
	reset(1);
}
