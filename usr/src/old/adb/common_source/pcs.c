#ifndef lint
static char sccsid[] = "@(#)pcs.c	5.3 (Berkeley) %G%";
#endif

/*
 * adb - subprocess control
 */

#include "defs.h"
#include "bkpt.h"
#include <machine/reg.h>	/* for getpc() *//* XXX */
#include <sys/file.h>
#include <sys/ptrace.h>
#include <sys/wait.h>

extern char NOBKPT[];
extern char SZBKPT[];
extern char EXBKPT[];
extern char NOPCS[];
extern char BADMOD[];
extern char NOFORK[];
extern char ENDPCS[];
extern char BADWAIT[];

struct bkpt *bkpthead;		/* head of breakpoint list */

static long runcount;		/* number of times to loop past breakpoints */

/* bpstate remembers whether we have installed the breakpoints */
static enum { BPOUT, BPIN } bpstate;

char	*malloc();

/* run modes */
#define	CONTINUOUS	0
#define	SINGLESTEP	1

/* sub process control */

subpcs(modif)
	int modif;
{
	register int check;
	register struct bkpt *bp;
	int execsig, runmode;
	char *comptr;

	switch (modif) {

	case 'd':
		/* delete breakpoint */
		if ((bp = scanbkpt(dot)) == NULL)
			error(NOBKPT);
		bp->state = BKPT_FREE;
		return;

	case 'D':
		/* delete all breapoints */
		for (bp = bkpthead; bp != NULL; bp = bp->next)
			bp->state = BKPT_FREE;
		return;

	case 'b':
	case 'B':
		/* set breakpoint */
		if ((bp = scanbkpt(dot)) != NULL)
			bp->state = BKPT_FREE;
		else {
			for (bp = bkpthead; bp != NULL; bp = bp->next)
				if (bp->state == BKPT_FREE)
					break;
			if (bp == NULL) {
				bp = (struct bkpt *)malloc(sizeof *bp);
				if (bp == NULL)
					error(EXBKPT);
				bp->next = bkpthead;
				bkpthead = bp;
			}
		}
		bp->loc = dot;
		bp->initcnt = bp->count = ecount;
		bp->state = BKPT_SET;
		check = MAX_BKPTCOM - 1;
		comptr = bp->comm;
		(void) rdc();
		unreadc();
		do {
			*comptr++ = readchar();
		} while (check-- && lastc != '\n');
		*comptr = 0;
		unreadc();
		if (check == 0)
			error(SZBKPT);
		return;

	case 'k':
	case 'K':
		/* kill process */
		if (pid == 0)
			error(NOPCS);
		adbprintf("%d: killed", pid);
		endpcs();
		return;

	case 'r':
	case 'R':
		/* run program */
		endpcs();
		setup();
		runcount = ecount;
		runmode = CONTINUOUS;
		execsig = 0;
		if (gavedot) {
			if (scanbkpt(dot) == NULL)
				runcount++;
		} else {
			if (scanbkpt(entrypc()) == NULL)
				runcount++;
		}
		break;

	case 's':
	case 'S':
		/* single step, with optional signal */
		runcount = ecount;
		if (pid) {
			runmode = SINGLESTEP;
			execsig = oexpr() ? expv : signo;
		} else {
			setup();
			runmode = SINGLESTEP;
			execsig = 0;
			runcount--;
		}
		break;

	case 'c':
	case 'C':
	case 0:
		/* continue with optional signal */
		runcount = ecount;
		if (pid == 0)
			error(NOPCS);
		runmode = CONTINUOUS;
		execsig = oexpr() ? expv : signo;
		break;

	default:
		error(BADMOD);
		/* NOTREACHED */
	}

	if (runcount > 0 && runpcs(runmode, execsig))
		adbprintf("breakpoint%16t");
	else
		adbprintf("stopped at%16t");
	delbp();
	printpc();
}

/*
 * Print all breakpoints.
 */
printbkpts()
{
	register struct bkpt *b;

	adbprintf("breakpoints\ncount%8tbkpt%24tcommand\n");
	for (b = bkpthead; b != NULL; b = b->next) {
		if (b->state != BKPT_FREE) {
			adbprintf("%-8.8D", b->count);
			psymoff("%R", b->loc, SP_INSTR, maxoff, "%24t");
			prints(b->comm);
		}
	}
}

/*
 * Remove (restore to original instruction(s)) all breakpoints.
 */
delbp()
{
	register struct bkpt *b;

	if (bpstate != BPOUT) {
		for (b = bkpthead; b != NULL; b = b->next)
			if (b->state != BKPT_FREE && clr_bpt(b))
				bperr(b, "clear");
		bpstate = BPOUT;
	}
}

/*
 * Insert all breakpoints.
 */
setbp()
{
	register struct bkpt *b;

	if (bpstate != BPIN) {
		for (b = bkpthead; b != NULL; b = b->next)
			if (b->state != BKPT_FREE && set_bpt(b))
				bperr(b, "set");
		bpstate = BPIN;
	}
}

static
bperr(b, how)
	struct bkpt *b;
	char *how;
{

	adbprintf("cannot %s breakpoint: ", how);
	psymoff("%R", b->loc, SP_INSTR, maxoff, "\n");
}

/*
 * ... return true iff stopped due to breakpoint
 */
int
runpcs(runmode, execsig)
	int runmode, execsig;
{
	register struct bkpt *bkpt;
	int rc;

	/* always set pc, so that expr>pc works too */
	setpc(gavedot ? dot : getpc());
	adbprintf("%s: running\n", symfile.name);
	while (--runcount >= 0) {
		/* BEGIN XXX (machine dependent?, delete ptrace, etc) */
		if (runmode == SINGLESTEP)
			delbp();	/* hardware handles single-stepping */
		else {	/* continuing from a breakpoint is hard */
			if ((bkpt = scanbkpt(getpc())) != NULL) {
				execbkpt(bkpt, execsig);
				execsig = 0;
			}
			setbp();
		}
		(void) ptrace(runmode == CONTINUOUS ? PT_CONTINUE : PT_STEP,
			pid, (int *)getpc(), execsig);
		/* END XXX */
		bpwait();
		checkerr();
		execsig = 0;
		delbp();
		readregs();

		if (signo != 0 || (bkpt = scanbkpt(getpc())) == NULL) {
			execsig = signo;
			rc = 0;
			continue;
		}
		/* stopped by BPT instruction */
#ifdef DEBUG
		adbprintf("\n BPT code: comm=%s%8tstate=%d",
		    bkpt->comm, bkpt->state);
#endif
		dot = bkpt->loc;
		switch (bkpt->state) {
			char *p;

		case BKPT_SET:
			bkpt->state = BKPT_TRIPPED;
			if (*bkpt->comm == '\n')
				break;
			p = lp;
			command(bkpt->comm, ':');
			lp = p;
			if (gavedot && edot == 0) /* maybe dot==0 ??? */
				break;
			if (--bkpt->count == 0)
				break;
			/* FALLTHROUGH */

		case BKPT_TRIPPED:
			execbkpt(bkpt, execsig);
			execsig = 0;
			runcount++;
			continue;

		default:
			panic("runpcs");
			/* NOTREACHED */
		}
		bkpt->count = bkpt->initcnt;
		rc = 1;
	}
	return (rc);
}

endpcs()
{
	register struct bkpt *bp;

	if (pid) {
		(void) ptrace(PT_KILL, pid, (int *)0, 0);	/* XXX */
		pid = 0;
		for (bp = bkpthead; bp != NULL; bp = bp->next)
			if (bp->state != BKPT_FREE)
				bp->state = BKPT_SET;
	}
	bpstate = BPOUT;
}

#ifdef VFORK
nullsig()
{

}
#endif

setup()
{

	(void) close(symfile.fd);
	symfile.fd = -1;
#ifndef VFORK
#define vfork fork
#endif
	if ((pid = vfork()) == 0) {
		(void) ptrace(PT_TRACE_ME, 0, (int *)0, 0);	/* XXX */
#ifdef VFORK
		(void) signal(SIGTRAP, nullsig);
#endif
		(void) signal(SIGINT, sigint);
		(void) signal(SIGQUIT, sigquit);
		doexec();
		exit(0);
	} else if (pid == -1)
		error(NOFORK);
	else {
		bpwait();
		readregs();
		symfile.fd = open(symfile.name, wtflag);
		if (errflag) {
			adbprintf("%s: cannot execute\n", symfile.name);
			endpcs();
			error((char *)0);
		}
	}
	bpstate = BPOUT;
}

execbkpt(bp, execsig)
	struct bkpt *bp;
	int execsig;
{

#ifdef DEBUG
	adbprintf("exbkpt: %d\n", bp->count);
#endif
	delbp();
	(void) ptrace(PT_STEP, pid, (int *)bp->loc, execsig);	/* XXX */
	bp->state = BKPT_SET;
	bpwait();
	checkerr();
	readregs();
}

static char separators[] = "<> \t\n";

doexec()
{
	register char *p, **ap;
	register int c;
	char *argl[LINELEN / 2 + 1];
	char args[LINELEN];
	extern char **environ;
	char *index();

	ap = argl;
	p = args;
	*ap++ = symfile.name;
	do {
		switch (c = rdc()) {

		case '\n':
			break;

		case '<':
			setfile(0, O_RDONLY, 0, p, "open");
			break;

		case '>':
			setfile(1, O_CREAT|O_WRONLY, 0666, p, "create");
			break;

		default:
			*ap = p;
			while (index(separators, c) == NULL) {
				*p++ = c;
				c = readchar();
			}
			*p++ = '\0';
			ap++;
		}
	} while (c != '\n');
	unreadc();
	*ap++ = 0;
	execve(symfile.name, argl, environ);
	perror(symfile.name);
}

static int
setfile(fd, flags, mode, namebuf, err)
	int fd, flags, mode;
	char *namebuf, *err;
{
	register char *p = namebuf;
	register int c = rdc();

	while (index(separators, c) == NULL) {
		*p++ = c;
		c = readchar();
	}
	*p = 0;
	(void) close(fd);
	if (open(namebuf, flags, mode) < 0) {
		adbprintf("%s: cannot %s\n", namebuf, err);
		_exit(0);
		/* NOTREACHED */
	}
}

struct bkpt *
scanbkpt(a)
	register addr_t a;
{
	register struct bkpt *bp;

	for (bp = bkpthead; bp != NULL; bp = bp->next)
		if (bp->state != BKPT_FREE && bp->loc == a)
			break;
	return (bp);
}

bpwait()
{
	register int w;
	union wait status;

	(void) signal(SIGINT, SIG_IGN);
	while ((w = wait(&status)) != pid && w != -1)
		 /* void */ ;
	(void) signal(SIGINT, intcatch);
	if (w == -1) {
		pid = 0;
		errflag = BADWAIT;
	} else if (!WIFSTOPPED(status)) {
		sigcode = 0;
		if ((signo = status.w_termsig) != 0)
			sigprint();
		if (status.w_coredump) {
			prints(" - core dumped");
			(void) close(corefile.fd);
			setcore();
		}
		pid = 0;
		bpstate = BPOUT;
		errflag = ENDPCS;
	} else {
		signo = status.w_stopsig;
		sigcode = ptrace(PT_READ_U, pid,
				 &((struct user *)0)->u_code, 0); /* XXX */
		if (signo != SIGTRAP)
			sigprint();
		else
			signo = 0;
		flushbuf();
	}
}
