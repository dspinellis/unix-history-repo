/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bpact.c	5.1 (Berkeley) 6/5/85";
#endif not lint
/*
 * Routines for doing the right thing when a breakpoint is reached.
 */

#include "defs.h"
#include "breakpoint.h"
#include "sym.h"
#include "tree.h"
#include "source.h"
#include "mappings.h"
#include "runtime.h"
#include "process.h"
#include "machine.h"
#include "main.h"
#include "bp.rep"
#include "tree/tree.rep"

typedef enum { SAVE, NOSAVE } SAVEBP;

LOCAL SAVEBP handlebp();

/*
 * A "delayed" breakpoint is one that has an action involving execution
 * of code, e.g. at a CALL we want to step from the beginning of the
 * procedure to the first line before printing parameters.
 */

LOCAL short delayed;

#define NONE 0
#define DELAY_CALL 1
#define DELAY_STOP 2

/*
 * Take action at a breakpoint; if it's not a breakpoint return FALSE.
 *
 * As we go through the list of breakpoints, we have to remember
 * the previous one so that "handlebp" can delete breakpoints on
 * the fly if necessary.
 *
 * If the breakpoint is a STOP_BP, handlebp will set "isstopped".  After
 * going through the loop, bpact checks if "isstopped" is set and calls
 * printstatus if it is.  This is so multiple breakpoints at the same
 * address, one of which is a STOP_BP, still work.
 */

#define isswitch(bptype) ( \
	bptype == ALL_ON || bptype == ALL_OFF || \
	bptype == TERM_ON || bptype == TERM_OFF || \
	bptype == BLOCK_ON || bptype == BLOCK_OFF || \
	bptype == STOP_ON || bptype == STOP_OFF \
)

BOOLEAN bpact()
{
	register BPINFO *p;
	BPINFO *prev, *next;
	BOOLEAN found;
	ADDRESS oldpc;

	delayed = NONE;
	found = FALSE;
	prev = NIL;
	for (p = bphead; p != NIL; p = next) {
		next = p->bpnext;
		if (p->bpaddr == pc) {
			prbpfound(p);
			found = TRUE;
			if (p->bpcond == NIL || isswitch(p->bptype) || cond(p->bpcond)) {
				prbphandled();
				if (handlebp(p) == NOSAVE) {
					prbpnosave();
					if (prev == NIL) {
						bphead = next;
					} else {
						prev->bpnext = next;
					}
					dispose(p);
				} else {
					prbpsave();
					prev = p;
				}
			} else {
				prev = p;
			}
		} else {
			prev = p;
		}
	}
	if (delayed != NONE) {
		oldpc = pc;
		runtofirst();
		if ((delayed&DELAY_CALL) == DELAY_CALL) {
			SYM *s, *t;

			s = curfunc;
			t = whatblock(return_addr());
			if (t == NIL) {
				panic("can't find block for caller addr %d", caller_addr());
			}
			printcall(s, t);
			addbp(return_addr(), RETURN, s, NIL, NIL, 0);
		}
		if (pc != oldpc) {
			bpact();
		}
		if (isstopped) {
			printstatus();
		}
	} else {
		if (isstopped) {
			printstatus();
		}
	}
	fflush(stdout);
	return(found);
}

/*
 * Handle an expected breakpoint appropriately, return whether
 * or not to save the breakpoint.
 */

LOCAL SAVEBP handlebp(p)
BPINFO *p;
{
	register SYM *s, *t;
	SAVEBP r;

	r = SAVE;
	switch(p->bptype) {
		case ALL_ON:
			curfunc = p->bpblock;
			addcond(TRPRINT, p->bpcond);
			if (p->bpline >= 0) {
				tracing++;
			} else {
				inst_tracing++;
			}
			addbp(return_addr(), ALL_OFF, curfunc, p->bpcond, NIL, 0);
			break;

		case ALL_OFF:
			r = NOSAVE;
			if (p->bpline >= 0) {
				tracing--;
			} else {
				inst_tracing--;
			}
			delcond(TRPRINT, p->bpcond);
			curfunc = p->bpblock;
			break;

		case STOP_ON:
			var_tracing++;
			curfunc = p->bpblock;
			if (p->bpnode != NIL) {
				addvar(TRSTOP, p->bpnode, p->bpcond);
			} else if (p->bpcond != NIL) {
				addcond(TRSTOP, p->bpcond);
			}
			addbp(return_addr(), STOP_OFF, curfunc, p->bpcond, p->bpnode, 0);
			break;

		case STOP_OFF:
			r = NOSAVE;
			delcond(TRSTOP, p->bpcond);
			var_tracing--;
			curfunc = p->bpblock;
			break;

		case INST:
			curline = p->bpline;
			if (curline > 0) {
				printf("trace:  ");
				printlines(curline, curline);
			} else {
				printf("inst trace:	");
				printinst(pc, pc);
			}
			break;

		case STOP_BP:
			if (p->bpblock != NIL) {
				delayed |= DELAY_STOP;
				curfunc = p->bpblock;
			}
			curline = p->bpline;
			isstopped = TRUE;
			break;

		case BLOCK_ON: {
			BPINFO *nbp;

			s = p->bpblock;
			t = p->bpnode->nameval;
			nbp = newbp(codeloc(t), CALL, t, p->bpcond, NIL, 0);
			addbp(return_addr(), BLOCK_OFF, (SYM *) nbp, NIL, NIL, 0);
			break;
		}

		case BLOCK_OFF: {
			BPINFO *oldbp;

			r = NOSAVE;
			oldbp = (BPINFO *) p->bpblock;
			delbp(oldbp->bpid);
			break;
		}

		case CALL:
			delayed |= DELAY_CALL;
			curfunc = p->bpblock;
			break;

		case RETURN:
			r = NOSAVE;
			s = p->bpblock;
			printrtn(s);
			break;

		case TERM_ON: {
			ADDRESS addr;

			curfunc = p->bpblock;
			addvar(TRPRINT, p->bpnode, p->bpcond);
			addr = return_addr();
			addbp(addr, TERM_OFF, curfunc, p->bpcond, p->bpnode, 0);
			var_tracing++;
			break;
		}

		case TERM_OFF:
			r = NOSAVE;
			var_tracing--;
			delvar(TRPRINT, p->bpnode, p->bpcond);
			curfunc = p->bpblock;
			break;

		case AT_BP:
			printf("at line %d: ", p->bpline);
			eval(p->bpnode);
			prtree(p->bpnode);
			printf(" = ");
			printval(p->bpnode->nodetype);
			putchar('\n');
			break;

		/*
		 * Returning from a called procedure.
		 * Further breakpoint processing is not done, since if
		 * there were any it wouldn't be associated with the call.
		 */
		case CALLPROC:
			procreturn(p->bpblock);
			delbp(p->bpid);
			erecover();
			/* NOTREACHED */

		case END_BP:
			r = NOSAVE;
			endprogram();

		default:
			panic("unknown bptype %d in cont", p->bptype);
			/* NOTREACHED */
	}
	return(r);
}

/*
 * Internal trace routines.
 */

LOCAL char *prbptype[] ={
	"ALL_ON", "ALL_OFF", "INST", "CALL", "RETURN", "BLOCK_ON", "BLOCK_OFF",
	"TERM_ON", "TERM_OFF", "AT_BP", "STOP_BP", "CALLPROC", "END_BP",
	"STOP_ON", "STOP_OFF",
};

LOCAL prbpfound(p)
BPINFO *p;
{
	if (option('b')) {
		printf("%s breakpoint found at pc %d, line %d -- ",
			prbptype[(int) p->bptype], p->bpaddr, p->bpline);
	}
}

LOCAL prbphandled()
{
	if (option('b')) {
		printf("handled, ");
	}
}

LOCAL prbpnosave()
{
	if (option('b')) {
		printf("not saved\n");
		fflush(stdout);
	}
}

LOCAL prbpsave()
{
	if (option('b')) {
		printf("saved\n");
		fflush(stdout);
	}
}
