/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ptrace.c	5.4 (Berkeley) %G%";
#endif not lint

/*
 * routines for tracing the execution of a process
 *
 * The system call "ptrace" does all the work, these
 * routines just try to interface easily to it.
 */

#include "defs.h"
#include <signal.h>
#include <sys/param.h>
#include <machine/reg.h>
#include "process.h"
#include "object.h"
#include "process.rep"

#       include "pxinfo.h"

#ifdef mc68000
#	define U_PAGE 0x2400
#	define U_AR0  (14*sizeof(int))
	LOCAL int ar0val = -1;
#endif

/*
 * This magic macro enables us to look at the process' registers
 * in its user structure.  Very gross.
 */

#if defined(vax) || defined(tahoe)
#	define regloc(reg)     (ctob(UPAGES) + ( sizeof(int) * (reg) ))
#else
#	define regloc(reg)     (ar0val + ( sizeof(int) * (reg) ))
#endif

#define WMASK           (~(sizeof(WORD) - 1))
#define cachehash(addr) ((unsigned) ((addr >> 2) % CSIZE))

#define ischild(pid)    ((pid) == 0)
#define traceme()       ptrace(0, 0, 0, 0)
#define setrep(n)       (1 << ((n)-1))
#define istraced(p)     (p->sigset&setrep(p->signo))

/*
 * ptrace options (specified in first argument)
 */

#define UREAD   3       /* read from process's user structure */
#define UWRITE  6       /* write to process's user structure */
#define IREAD   1       /* read from process's instruction space */
#define IWRITE  4       /* write to process's instruction space */
#define DREAD   2       /* read from process's data space */
#define DWRITE  5       /* write to process's data space */
#define CONT    7       /* continue stopped process */
#define SSTEP   9       /* continue for approximately one instruction */
#define PKILL   8       /* terminate the process */

/*
 * Start up a new process by forking and exec-ing the
 * given argument list, returning when the process is loaded
 * and ready to execute.  The PROCESS information (pointed to
 * by the first argument) is appropriately filled.
 *
 * If the given PROCESS structure is associated with an already running
 * process, we terminate it.
 */

/* VARARGS2 */
pstart(p, cmd, argv, infile, outfile)
PROCESS *p;
char *cmd;
char **argv;
char *infile;
char *outfile;
{
    int status;
    FILE *in, *out;

    if (p->pid != 0) {                  /* child already running? */
	ptrace(PKILL, p->pid, 0, 0);    /* ... kill it! */
    }
#ifdef tahoe
    INTFP = (ADDRESS)0;
#endif tahoe
    psigtrace(p, SIGTRAP, TRUE);
    if ((p->pid = fork()) == -1) {
	panic("can't fork");
    }
    if (ischild(p->pid)) {
	traceme();
	if (infile != NIL) {
	    if ((in = fopen(infile, "r")) == NIL) {
		printf("can't read %s\n", infile);
		exit(1);
	    }
	    fswap(0, fileno(in));
	}
	if (outfile != NIL) {
	    if ((out = fopen(outfile, "w")) == NIL) {
		printf("can't write %s\n", outfile);
		exit(1);
	    }
	    fswap(1, fileno(out));
	}
	execvp(cmd, argv);
	panic("can't exec %s", argv[0]);
    }
    pwait(p->pid, &status);
    getinfo(p, status);
}

/*
 * Continue a stopped process.  The argument points to a PROCESS structure.
 * Before the process is restarted it's user area is modified according to
 * the values in the structure.  When this routine finishes,
 * the structure has the new values from the process's user area.
 *
 * Pcont terminates when the process stops with a signal pending that
 * is being traced (via psigtrace), or when the process terminates.
 */

pcont(p)
PROCESS *p;
{
    int status;

    if (p->pid == 0) {
	error("program not active");
    }
    do {
	setinfo(p);
	sigs_off();
	if (ptrace(CONT, p->pid, p->pc, p->signo) < 0) {
	    panic("can't continue process");
	}
	pwait(p->pid, &status);
	sigs_on();
	getinfo(p, status);
    } while (p->status == STOPPED && !istraced(p));
}

/*
 * single step as best ptrace can
 */

pstep(p)
PROCESS *p;
{
    int status;

    setinfo(p);
    sigs_off();
    ptrace(SSTEP, p->pid, p->pc, p->signo);
    pwait(p->pid, &status);
    sigs_on();
    getinfo(p, status);
}

/*
 * Return from execution when the given signal is pending.
 */

psigtrace(p, sig, sw)
PROCESS *p;
int sig;
int sw;
{
    if (sw) {
	p->sigset |= setrep(sig);
    } else {
	p->sigset &= ~setrep(sig);
    }
}

/*
 * Don't catch any signals.
 * Particularly useful when letting a process finish uninhibited (i.e. px).
 */

unsetsigtraces(p)
PROCESS *p;
{
    p->sigset = 0;
}

/*
 * turn off attention to signals not being caught
 */

LOCAL void *onintr, *onquit;

LOCAL sigs_off()
{
    onintr = signal(SIGINT, SIG_IGN);
    onquit = signal(SIGQUIT, SIG_IGN);
}

/*
 * turn back on attention to signals
 */

LOCAL sigs_on()
{
    (void) signal(SIGINT, onintr);
    (void) signal(SIGQUIT, onquit);
}

/*
 * get PROCESS information from process's user area
 */

#if vax
    LOCAL int rloc[] ={
	R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11,
    };
#endif
#if tahoe
    LOCAL int rloc[] ={
	R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12,
    };
#endif
#if mc68000
    LOCAL int rloc[] ={
	R0, R1, R2, R3, R4, R5, R6, R7, AR0, AR1, AR2, AR3, AR4, AR5,
    };
#endif

LOCAL getinfo(p, status)
register PROCESS *p;
register int status;
{
    register int i;

    p->signo = (status&0177);
    p->exitval = ((status >> 8)&0377);
    if (p->signo == STOPPED) {
	p->status = p->signo;
	p->signo = p->exitval;
	p->exitval = 0;
    } else {
	p->status = FINISHED;
	return;
    }
#if !defined(vax) && !defined(tahoe)
    if (ar0val < 0){
	ar0val = ptrace(UREAD, p->pid, U_AR0, 0);
	ar0val -= U_PAGE;
    }
#endif
    for (i = 0; i < NREG; i++) {
	p->reg[i] = ptrace(UREAD, p->pid, regloc(rloc[i]), 0);
	p->oreg[i] = p->reg[i];
    }
#if defined(vax) || defined(tahoe)
    p->fp = p->ofp = ptrace(UREAD, p->pid, regloc(FP), 0);
    p->sp = p->osp = ptrace(UREAD, p->pid, regloc(SP), 0);
    p->pc = p->opc = ptrace(UREAD, p->pid, regloc(PC), 0);
#endif
#ifdef vax
    p->ap = p->oap = ptrace(UREAD, p->pid, regloc(AP), 0);
#endif
#ifdef mc68000
    p->fp = p->ofp = ptrace(UREAD, p->pid, regloc(AR6), 0);
    p->ap = p->oap = p->fp;
    p->sp = p->osp = ptrace(UREAD, p->pid, regloc(SP), 0);
    p->pc = p->opc = ptrace(UREAD, p->pid, regloc(PC), 0);
#endif
}

/*
 * set process's user area information from given PROCESS structure
 */

LOCAL setinfo(p)
register PROCESS *p;
{
    register int i;
    register int r;

    if (istraced(p)) {
	p->signo = 0;
    }
    for (i = 0; i < NREG; i++) {
	if ((r = p->reg[i]) != p->oreg[i]) {
	    ptrace(UWRITE, p->pid, regloc(rloc[i]), r);
	}
    }
#if vax || tahoe
    if ((r = p->fp) != p->ofp) {
	ptrace(UWRITE, p->pid, regloc(FP), r);
    }
#endif
#if vax
    if ((r = p->ap) != p->oap) {
	ptrace(UWRITE, p->pid, regloc(AP), r);
    }
#endif
#if mc68000
    if ((r = p->fp) != p->ofp) {
	ptrace(UWRITE, p->pid, regloc(AR6), r);
    }
#endif
    if ((r = p->sp) != p->osp) {
	ptrace(UWRITE, p->pid, regloc(SP), r);
    }
    if ((r = p->pc) != p->opc) {
	ptrace(UWRITE, p->pid, regloc(PC), r);
    }
}

/*
 * Structure for reading and writing by words, but dealing with bytes.
 */

typedef union {
    WORD pword;
    BYTE pbyte[sizeof(WORD)];
} PWORD;

/*
 * Read (write) from (to) the process' address space.
 * We must deal with ptrace's inability to look anywhere other
 * than at a word boundary.
 */

LOCAL WORD fetch();
LOCAL store();

pio(p, op, seg, buff, addr, nbytes)
PROCESS *p;
PIO_OP op;
PIO_SEG seg;
char *buff;
ADDRESS addr;
int nbytes;
{
    register int i, k;
    register ADDRESS newaddr;
    register char *cp;
    char *bufend;
    PWORD w;
    ADDRESS wordaddr;
    int byteoff;

    if (p->status != STOPPED) {
	error("program is not active");
    }
    cp = buff;
    newaddr = addr;
    wordaddr = (newaddr&WMASK);
    if (wordaddr != newaddr) {
	w.pword = fetch(p, seg, wordaddr);
	for (i = newaddr - wordaddr; i<sizeof(WORD) && nbytes>0; i++) {
	    if (op == PREAD) {
		*cp++ = w.pbyte[i];
	    } else {
		w.pbyte[i] = *cp++;
	    }
	    nbytes--;
	}
	if (op == PWRITE) {
	    store(p, seg, wordaddr, w.pword);
	}
	newaddr = wordaddr + sizeof(WORD);
    }
    byteoff = (nbytes&(~WMASK));
    nbytes -= byteoff;
    bufend = cp + nbytes;
    while (cp < bufend) {
	if (op == PREAD) {
	    w.pword = fetch(p, seg, newaddr);
	    for (k = 0; k < sizeof(WORD); k++) {
		*cp++ = w.pbyte[k];
	    }
	} else {
	    for (k = 0; k < sizeof(WORD); k++) {
		w.pbyte[k] = *cp++;
	    }
	    store(p, seg, newaddr, w.pword);
	}
	newaddr += sizeof(WORD);
    }
    if (byteoff > 0) {
	w.pword = fetch(p, seg, newaddr);
	for (i = 0; i < byteoff; i++) {
	    if (op == PREAD) {
		*cp++ = w.pbyte[i];
	    } else {
		w.pbyte[i] = *cp++;
	    }
	}
	if (op == PWRITE) {
	    store(p, seg, newaddr, w.pword);
	}
    }
}

/*
 * Get a word from a process at the given address.
 * The address is assumed to be on a word boundary.
 *
 * We use a simple cache scheme to avoid redundant references to
 * the instruction space (which is assumed to be pure).  In the
 * case of px, the "instruction" space lies between ENDOFF and
 * ENDOFF + objsize.
 *
 * It is necessary to use a write-through scheme so that
 * breakpoints right next to each other don't interfere.
 */

LOCAL WORD fetch(p, seg, addr)
PROCESS *p;
PIO_SEG seg;
register int addr;
{
    register CACHEWORD *wp;
    register WORD w;

    switch (seg) {
	case TEXTSEG:
	    panic("tried to fetch from px i-space");
	    /* NOTREACHED */

	case DATASEG:
	    if (addr >= ENDOFF && addr < ENDOFF + objsize) {
		wp = &p->word[cachehash(addr)];
		if (addr == 0 || wp->addr != addr) {
		    w = ptrace(DREAD, p->pid, addr, 0);
		    wp->addr = addr;
		    wp->val = w;
		} else {
		    w = wp->val;
		}
	    } else {
		w = ptrace(DREAD, p->pid, addr, 0);
	    }
	    break;

	default:
	    panic("fetch: bad seg %d", seg);
	    /* NOTREACHED */
    }
    return(w);
}

/*
 * Put a word into the process' address space at the given address.
 * The address is assumed to be on a word boundary.
 */

LOCAL store(p, seg, addr, data)
PROCESS *p;
PIO_SEG seg;
int addr;
WORD data;
{
    register CACHEWORD *wp;

    switch (seg) {
	case TEXTSEG:
	    wp = &p->word[cachehash(addr)];
	    wp->addr = addr;
	    wp->val = data;
	    ptrace(IWRITE, p->pid, addr, data);
	    break;

	case DATASEG:
	    if (addr >= ENDOFF && addr < ENDOFF + objsize) {
		wp = &p->word[cachehash(addr)];
		wp->addr = addr;
		wp->val = data;
	    }
	    ptrace(DWRITE, p->pid, addr, data);
	    break;

	default:
	    panic("store: bad seg %d", seg);
	    /*NOTREACHED*/
    }
}

/*
 * Initialize the instruction cache for a process.
 * This is particularly necessary after the program has been remade.
 */

initcache(process)
PROCESS *process;
{
    register int i;

    for (i = 0; i < CSIZE; i++) {
	process->word[i].addr = 0;
    }
}

/*
 * Swap file numbers so as to redirect standard input and output.
 */

LOCAL fswap(oldfd, newfd)
int oldfd;
int newfd;
{
    if (oldfd != newfd) {
	close(oldfd);
	dup(newfd);
	close(newfd);
    }
}

#ifdef tahoe
BOOLEAN didret;

void
chkret(p, status)
PROCESS *p;
int status;
{
	if (((status == (SIGILL << 8) | STOPPED) ||
	    (status == (SIGTRAP << 8) | STOPPED))) {
		didret = FALSE;
	} else {
		didret = TRUE;
	}
}

void
doret(p)
PROCESS *p;
{
	register count = 0;

	if (!didret) {
	    do {
		if (++count > 5) {
		    panic("px would not return to interpreter");
		}
		p->pc = RETLOC;
		pstep(p);
	    } while(INTFP && p->fp != INTFP);
	    didret = TRUE;
	}
}
#endif
