/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)ptrace.c 1.3 %G%";

/*
 * routines for tracing the execution of a process
 *
 * The system call "ptrace" does all the work, these
 * routines just try to interface easily to it.
 */

#include "defs.h"
#include <signal.h>
#include <sys/param.h>
#include <sys/reg.h>
#include "process.h"
#include "object.h"
#include "process.rep"

#   if (isvaxpx)
#       include "pxinfo.h"
#   endif

/*
 * This magic macro enables us to look at the process' registers
 * in its user structure.  Very gross.
 */

#define regloc(reg)     (ctob(UPAGES) + ( sizeof(int) * (reg) ))

#define WMASK           (~(sizeof(WORD) - 1))
#define cachehash(addr) ((unsigned) ((addr >> 2) % CSIZE))

#define FIRSTSIG        SIGINT
#define LASTSIG         SIGQUIT
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

typedef int INTFUNC();

LOCAL INTFUNC *sigfunc[NSIG];

LOCAL sigs_off()
{
    register int i;

    for (i = FIRSTSIG; i < LASTSIG; i++) {
	if (i != SIGKILL) {
	    sigfunc[i] = signal(i, SIG_IGN);
	}
    }
}

/*
 * turn back on attention to signals
 */

LOCAL sigs_on()
{
    register int i;

    for (i = FIRSTSIG; i < LASTSIG; i++) {
	if (i != SIGKILL) {
	    signal(i, sigfunc[i]);
	}
    }
}

/*
 * get PROCESS information from process's user area
 */

LOCAL int rloc[] ={
    R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11,
};

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
    for (i = 0; i < NREG; i++) {
	p->reg[i] = ptrace(UREAD, p->pid, regloc(rloc[i]), 0);
	p->oreg[i] = p->reg[i];
    }
    p->fp = p->ofp = ptrace(UREAD, p->pid, regloc(FP), 0);
    p->ap = p->oap = ptrace(UREAD, p->pid, regloc(AP), 0);
    p->sp = p->osp = ptrace(UREAD, p->pid, regloc(SP), 0);
    p->pc = p->opc = ptrace(UREAD, p->pid, regloc(PC), 0);
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
    if ((r = p->fp) != p->ofp) {
	ptrace(UWRITE, p->pid, regloc(FP), r);
    }
    if ((r = p->sp) != p->osp) {
	ptrace(UWRITE, p->pid, regloc(SP), r);
    }
    if ((r = p->ap) != p->oap) {
	ptrace(UWRITE, p->pid, regloc(AP), r);
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
    register int i;
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
	    *((WORD *) cp) = fetch(p, seg, newaddr);
	} else {
	    store(p, seg, newaddr, *((WORD *) cp));
	}
	cp += sizeof(WORD);
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
#           if (isvaxpx)
		panic("tried to fetch from px i-space");
		/* NOTREACHED */
#           else
		wp = &p->word[cachehash(addr)];
		if (addr == 0 || wp->addr != addr) {
		    w = ptrace(IREAD, p->pid, addr, 0);
		    wp->addr = addr;
		    wp->val = w;
		} else {
		    w = wp->val;
		}
		break;
#           endif

	case DATASEG:
#           if (isvaxpx)
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
#           else
		w = ptrace(DREAD, p->pid, addr, 0);
#           endif
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
#           if (isvaxpx)
		if (addr >= ENDOFF && addr < ENDOFF + objsize) {
		    wp = &p->word[cachehash(addr)];
		    wp->addr = addr;
		    wp->val = data;
		}
#           endif
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
