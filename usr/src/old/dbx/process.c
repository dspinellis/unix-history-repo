/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)process.c 1.3 %G%";

/*
 * Process management.
 *
 * This module contains the routines to manage the execution and
 * tracing of the debuggee process.
 */

#include "defs.h"
#include "process.h"
#include "machine.h"
#include "events.h"
#include "tree.h"
#include "operators.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "main.h"
#include "coredump.h"
#include <signal.h>
#include <errno.h>
#include <sys/param.h>
#include <machine/reg.h>
#include <sys/stat.h>

#ifndef public

typedef struct Process *Process;

Process process;

#include "machine.h"

#endif

#define NOTSTARTED 1
#define STOPPED 0177
#define FINISHED 0

/*
 * Cache-ing of instruction segment is done to reduce the number
 * of system calls.
 */

#define CSIZE 1003       /* size of instruction cache */

typedef struct {
    Word addr;
    Word val;
} CacheWord;

/*
 * This structure holds the information we need from the user structure.
 */

struct Process {
    int pid;			/* process being traced */
    int mask;			/* ps */
    Word reg[NREG];		/* process's registers */
    Word oreg[NREG];		/* registers when process last stopped */
    short status;		/* either STOPPED or FINISHED */
    short signo;		/* signal that stopped process */
    int exitval;		/* return value from exit() */
    long sigset;		/* bit array of traced signals */
    CacheWord word[CSIZE];	/* text segment cache */
};

/*
 * These definitions are for the arguments to "pio".
 */

typedef enum { PREAD, PWRITE } PioOp;
typedef enum { TEXTSEG, DATASEG } PioSeg;

private struct Process pbuf;

#define MAXNCMDARGS 10         /* maximum number of arguments to RUN */

private Boolean just_started;
private int argc;
private String argv[MAXNCMDARGS];
private String infile, outfile;

/*
 * Initialize process information.
 */

public process_init()
{
    register Integer i;
    Char buf[10];

    process = &pbuf;
    process->status = (coredump) ? STOPPED : NOTSTARTED;
    setsigtrace();
    for (i = 0; i < NREG; i++) {
	sprintf(buf, "$r%d", i);
	defregname(identname(buf, false), i);
    }
    defregname(identname("$ap", true), ARGP);
    defregname(identname("$fp", true), FRP);
    defregname(identname("$sp", true), STKP);
    defregname(identname("$pc", true), PROGCTR);
    if (coredump) {
	coredump_readin(process->mask, process->reg, process->signo);
    }
}

/*
 * Routines to get at process information from outside this module.
 */

public Word reg(n)
Integer n;
{
    register Word w;

    if (n == NREG) {
	w = process->mask;
    } else {
	w = process->reg[n];
    }
    return w;
}

public setreg(n, w)
Integer n;
Word w;
{
    process->reg[n] = w;
}

/*
 * Begin execution.
 *
 * We set a breakpoint at the end of the code so that the
 * process data doesn't disappear after the program terminates.
 */

private Boolean remade();

public start(argv, infile, outfile)
String argv[];
String infile, outfile;
{
    String pargv[4];
    Node cond;

    if (coredump) {
	coredump = false;
	fclose(corefile);
	coredump_close();
    }
    if (argv == nil) {
	argv = pargv;
	pargv[0] = objname;
	pargv[1] = nil;
    } else {
	argv[argc] = nil;
    }
    if (remade(objname)) {
	reinit(argv, infile, outfile);
    }
    pstart(process, argv, infile, outfile);
    if (process->status == STOPPED) {
	pc = 0;
	curfunc = program;
	if (objsize != 0) {
	    cond = build(O_EQ, build(O_SYM, pcsym), build(O_LCON, lastaddr()));
	    event_once(cond, buildcmdlist(build(O_ENDX)));
	}
    }
}

/*
 * Check to see if the object file has changed since the symbolic
 * information last was read.
 */

private time_t modtime;

private Boolean remade(filename)
String filename;
{
    struct stat s;
    Boolean b;

    stat(filename, &s);
    b = (Boolean) (modtime != 0 and modtime < s.st_mtime);
    modtime = s.st_mtime;
    return b;
}

/*
 * Set up what signals we want to trace.
 */

private setsigtrace()
{
    register Integer i;
    register Process p;

    p = process;
    for (i = 1; i <= NSIG; i++) {
	psigtrace(p, i, true);
    }
    psigtrace(p, SIGHUP, false);
    psigtrace(p, SIGKILL, false);
    psigtrace(p, SIGALRM, false);
    psigtrace(p, SIGTSTP, false);
    psigtrace(p, SIGCONT, false);
    psigtrace(p, SIGCHLD, false);
}

/*
 * Initialize the argument list.
 */

public arginit()
{
    infile = nil;
    outfile = nil;
    argv[0] = objname;
    argc = 1;
}

/*
 * Add an argument to the list for the debuggee.
 */

public newarg(arg)
String arg;
{
    if (argc >= MAXNCMDARGS) {
	error("too many arguments");
    }
    argv[argc++] = arg;
}

/*
 * Set the standard input for the debuggee.
 */

public inarg(filename)
String filename;
{
    if (infile != nil) {
	error("multiple input redirects");
    }
    infile = filename;
}

/*
 * Set the standard output for the debuggee.
 * Probably should check to avoid overwriting an existing file.
 */

public outarg(filename)
String filename;
{
    if (outfile != nil) {
	error("multiple output redirect");
    }
    outfile = filename;
}

/*
 * Start debuggee executing.
 */

public run()
{
    process->status = STOPPED;
    fixbps();
    curline = 0;
    start(argv, infile, outfile);
    just_started = true;
    isstopped = false;
    cont();
}

/*
 * Continue execution wherever we left off.
 *
 * Note that this routine never returns.  Eventually bpact() will fail
 * and we'll call printstatus or step will call it.
 */

typedef int Intfunc();

private Intfunc *dbintr;
private intr();

#define succeeds    == true
#define fails       == false

public cont()
{
    dbintr = signal(SIGINT, intr);
    if (just_started) {
	just_started = false;
    } else {
	if (not isstopped) {
	    error("can't continue execution");
	}
	isstopped = false;
	step();
    }
    for (;;) {
	if (single_stepping) {
	    printnews();
	} else {
	    setallbps();
	    resume();
	    unsetallbps();
	    if (bpact() fails) {
		printstatus();
	    }
	}
	step();
    }
    /* NOTREACHED */
}

/*
 * This routine is called if we get an interrupt while "running" px
 * but actually in the debugger.  Could happen, for example, while
 * processing breakpoints.
 *
 * We basically just want to keep going; the assumption is
 * that when the process resumes it will get the interrupt
 * which will then be handled.
 */

private intr()
{
    signal(SIGINT, intr);
}

public fixintr()
{
    signal(SIGINT, dbintr);
}

/*
 * Resume execution.
 */

public resume()
{
    register Process p;

    p = process;
    if (traceexec) {
	printf("execution resumes at pc 0x%x\n", process->reg[PROGCTR]);
	fflush(stdout);
    }
    pcont(p);
    pc = process->reg[PROGCTR];
    if (traceexec) {
	printf("execution stops at pc 0x%x on sig %d\n",
	    process->reg[PROGCTR], p->signo);
	fflush(stdout);
    }
}

/*
 * Continue execution up to the next source line.
 *
 * There are two ways to define the next source line depending on what
 * is desired when a procedure or function call is encountered.  Step
 * stops at the beginning of the procedure or call; next skips over it.
 */

/*
 * Stepc is what is called when the step command is given.
 * It has to play with the "isstopped" information.
 */

public stepc()
{
    if (not isstopped) {
	error("can't continue execution");
    }
    isstopped = false;
    dostep(false);
    isstopped = true;
}

public next()
{
    if (not isstopped) {
	error("can't continue execution");
    }
    isstopped = false;
    dostep(true);
    isstopped = true;
}

public step()
{
    dostep(false);
}

/*
 * Resume execution up to the given address.  It is assumed that
 * no breakpoints exist between the current address and the one
 * we're stepping to.  This saves us from setting all the breakpoints.
 */

public stepto(addr)
Address addr;
{
    setbp(addr);
    resume();
    unsetbp(addr);
    if (not isbperr()) {
	printstatus();
    }
}

/*
 * Print the status of the process.
 * This routine does not return.
 */

public printstatus()
{
    if (process->status == FINISHED) {
	exit(0);
    } else {
	curfunc = whatblock(pc);
	getsrcpos();
	if (process->signo == SIGINT) {
	    isstopped = true;
	    printerror();
	} else if (isbperr() and isstopped) {
	    printf("stopped ");
	    if (curline > 0) {
		printsrcpos();
		putchar('\n');
		printlines(curline, curline);
	    } else {
		printf("in ");
		printwhich(stdout, curfunc);
		printf(" at 0x%x\n", pc);
		printinst(pc, pc);
	    }
	    erecover();
	} else {
	    fixbps();
	    fixintr();
	    isstopped = true;
	    printerror();
	}
    }
}

/*
 * Some functions for testing the state of the process.
 */

public Boolean notstarted(p)
Process p;
{
    return (Boolean) (p->status == NOTSTARTED);
}

public Boolean isfinished(p)
Process p;
{
    return (Boolean) (p->status == FINISHED);
}

/*
 * Return the signal number which stopped the process.
 */

public Integer errnum(p)
Process p;
{
    return p->signo;
}

/*
 * Return the termination code of the process.
 */

public Integer exitcode(p)
Process p;
{
    return p->exitval;
}

/*
 * These routines are used to access the debuggee process from
 * outside this module.
 *
 * They invoke "pio" which eventually leads to a call to "ptrace".
 * The system generates an I/O error when a ptrace fails, we catch
 * that here and assume its due to a misguided address.
 */

extern Intfunc *onsyserr();

private badaddr;
private rwerr();

/*
 * Read from the process' instruction area.
 */

public iread(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    Intfunc *f;

    f = onsyserr(EIO, rwerr);
    badaddr = addr;
    if (coredump) {
	coredump_readtext(buff, addr, nbytes);
    } else {
	pio(process, PREAD, TEXTSEG, buff, addr, nbytes);
    }
    onsyserr(EIO, f);
}

/* 
 * Write to the process' instruction area, usually in order to set
 * or unset a breakpoint.
 */

public iwrite(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    Intfunc *f;

    if (coredump) {
	error("no process to write to");
    }
    f = onsyserr(EIO, rwerr);
    badaddr = addr;
    pio(process, PWRITE, TEXTSEG, buff, addr, nbytes);
    onsyserr(EIO, f);
}

/*
 * Read for the process' data area.
 */

public dread(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    Intfunc *f;

    f = onsyserr(EIO, rwerr);
    badaddr = addr;
    if (coredump) {
	coredump_readdata(buff, addr, nbytes);
    } else {
	pio(process, PREAD, DATASEG, buff, addr, nbytes);
    }
    onsyserr(EIO, f);
}

/*
 * Write to the process' data area.
 */

public dwrite(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    Intfunc *f;

    if (coredump) {
	error("no process to write to");
    }
    f = onsyserr(EIO, rwerr);
    badaddr = addr;
    pio(process, PWRITE, DATASEG, buff, addr, nbytes);
    onsyserr(EIO, f);
}

/*
 * Error handler.
 */

private rwerr()
{
    error("bad read/write process address 0x%x", badaddr);
}

/*
 * Ptrace interface.
 */

/*
 * This magic macro enables us to look at the process' registers
 * in its user structure.  Very gross.
 */

#define regloc(reg)     (ctob(UPAGES) + ( sizeof(int) * (reg) ))

#define WMASK           (~(sizeof(Word) - 1))
#define cachehash(addr) ((unsigned) ((addr >> 2) % CSIZE))

#define FIRSTSIG        SIGINT
#define LASTSIG         SIGQUIT
#define ischild(pid)    ((pid) == 0)
#define traceme()       ptrace(0, 0, 0, 0)
#define setrep(n)       (1 << ((n)-1))
#define istraced(p)     (p->sigset&setrep(p->signo))

/*
 * Ptrace options (specified in first argument).
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
private pstart(p, argv, infile, outfile)
Process p;
String argv[];
String infile;
String outfile;
{
    int status;
    File in, out;

    if (p->pid != 0) {          	/* child already running? */
	ptrace(PKILL, p->pid, 0, 0);    /* ... kill it! */
    }
    psigtrace(p, SIGTRAP, true);
    if ((p->pid = fork()) == -1) {
	panic("can't fork");
    }
    if (ischild(p->pid)) {
	traceme();
	if (infile != nil) {
	    in = fopen(infile, "r");
	    if (in == nil) {
		printf("can't read %s\n", infile);
		exit(1);
	    }
	    fswap(0, fileno(in));
	}
	if (outfile != nil) {
	    out = fopen(outfile, "w");
	    if (out == nil) {
		printf("can't write %s\n", outfile);
		exit(1);
	    }
	    fswap(1, fileno(out));
	}
	execvp(argv[0], argv);
	panic("can't exec %s", argv[0]);
    }
    pwait(p->pid, &status);
    getinfo(p, status);
    if (p->status != STOPPED) {
	error("program could not begin execution");
    }
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

private pcont(p)
Process p;
{
    int status;

    if (p->pid == 0) {
	error("program not active");
    }
    do {
	setinfo(p);
	sigs_off();
	if (ptrace(CONT, p->pid, p->reg[PROGCTR], p->signo) < 0) {
	    panic("can't continue process");
	}
	pwait(p->pid, &status);
	sigs_on();
	getinfo(p, status);
    } while (p->status == STOPPED and not istraced(p));
}

/*
 * Single step as best ptrace can.
 */

public pstep(p)
Process p;
{
    int status;

    setinfo(p);
    sigs_off();
    ptrace(SSTEP, p->pid, p->reg[PROGCTR], p->signo);
    pwait(p->pid, &status);
    sigs_on();
    getinfo(p, status);
}

/*
 * Return from execution when the given signal is pending.
 */

public psigtrace(p, sig, sw)
Process p;
int sig;
Boolean sw;
{
    if (sw) {
	p->sigset |= setrep(sig);
    } else {
	p->sigset &= ~setrep(sig);
    }
}

/*
 * Don't catch any signals.
 * Particularly useful when letting a process finish uninhibited.
 */

public unsetsigtraces(p)
Process p;
{
    p->sigset = 0;
}

/*
 * Turn off attention to signals not being caught.
 */

private Intfunc *sigfunc[NSIG];

private sigs_off()
{
    register int i;

    for (i = FIRSTSIG; i < LASTSIG; i++) {
	if (i != SIGKILL) {
	    sigfunc[i] = signal(i, SIG_IGN);
	}
    }
}

/*
 * Turn back on attention to signals.
 */

private sigs_on()
{
    register int i;

    for (i = FIRSTSIG; i < LASTSIG; i++) {
	if (i != SIGKILL) {
	    signal(i, sigfunc[i]);
	}
    }
}

/*
 * Get process information from user area.
 */

private int rloc[] ={
    R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, AP, FP, SP, PC
};

private getinfo(p, status)
register Process p;
register int status;
{
    register int i;

    p->signo = (status&0177);
    p->exitval = ((status >> 8)&0377);
    if (p->signo != STOPPED) {
	p->status = FINISHED;
    } else {
	p->status = p->signo;
	p->signo = p->exitval;
	p->exitval = 0;
	p->mask = ptrace(UREAD, p->pid, regloc(PS), 0);
	for (i = 0; i < NREG; i++) {
	    p->reg[i] = ptrace(UREAD, p->pid, regloc(rloc[i]), 0);
	    p->oreg[i] = p->reg[i];
	}
    }
}

/*
 * Set process's user area information from given process structure.
 */

private setinfo(p)
register Process p;
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
}

/*
 * Structure for reading and writing by words, but dealing with bytes.
 */

typedef union {
    Word pword;
    Byte pbyte[sizeof(Word)];
} Pword;

/*
 * Read (write) from (to) the process' address space.
 * We must deal with ptrace's inability to look anywhere other
 * than at a word boundary.
 */

private Word fetch();
private store();

private pio(p, op, seg, buff, addr, nbytes)
Process p;
PioOp op;
PioSeg seg;
char *buff;
Address addr;
int nbytes;
{
    register int i;
    register Address newaddr;
    register char *cp;
    char *bufend;
    Pword w;
    Address wordaddr;
    int byteoff;

    if (p->status != STOPPED) {
	error("program is not active");
    }
    cp = buff;
    newaddr = addr;
    wordaddr = (newaddr&WMASK);
    if (wordaddr != newaddr) {
	w.pword = fetch(p, seg, wordaddr);
	for (i = newaddr - wordaddr; i < sizeof(Word) and nbytes > 0; i++) {
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
	newaddr = wordaddr + sizeof(Word);
    }
    byteoff = (nbytes&(~WMASK));
    nbytes -= byteoff;
    bufend = cp + nbytes;
    while (cp < bufend) {
	if (op == PREAD) {
	    *((Word *) cp) = fetch(p, seg, newaddr);
	} else {
	    store(p, seg, newaddr, *((Word *) cp));
	}
	cp += sizeof(Word);
	newaddr += sizeof(Word);
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
 * A simple cache scheme is used to avoid redundant ptrace calls
 * to the instruction space since it is assumed to be pure.
 *
 * It is necessary to use a write-through scheme so that
 * breakpoints right next to each other don't interfere.
 */

private Integer nfetchs, nreads, nwrites;

private Word fetch(p, seg, addr)
Process p;
PioSeg seg;
register int addr;
{
    register CacheWord *wp;
    register Word w;

    switch (seg) {
	case TEXTSEG:
	    ++nfetchs;
	    wp = &p->word[cachehash(addr)];
	    if (addr == 0 or wp->addr != addr) {
		++nreads;
		w = ptrace(IREAD, p->pid, addr, 0);
		wp->addr = addr;
		wp->val = w;
	    } else {
		w = wp->val;
	    }
	    break;

	case DATASEG:
	    w = ptrace(DREAD, p->pid, addr, 0);
	    break;

	default:
	    panic("fetch: bad seg %d", seg);
	    /* NOTREACHED */
    }
    return w;
}

/*
 * Put a word into the process' address space at the given address.
 * The address is assumed to be on a word boundary.
 */

private store(p, seg, addr, data)
Process p;
PioSeg seg;
int addr;
Word data;
{
    register CacheWord *wp;

    switch (seg) {
	case TEXTSEG:
	    ++nwrites;
	    wp = &p->word[cachehash(addr)];
	    wp->addr = addr;
	    wp->val = data;
	    ptrace(IWRITE, p->pid, addr, data);
	    break;

	case DATASEG:
	    ptrace(DWRITE, p->pid, addr, data);
	    break;

	default:
	    panic("store: bad seg %d", seg);
	    /* NOTREACHED */
    }
}

public printptraceinfo()
{
    printf("%d fetchs, %d reads, %d writes\n", nfetchs, nreads, nwrites);
}

/*
 * Swap file numbers so as to redirect standard input and output.
 */

private fswap(oldfd, newfd)
int oldfd;
int newfd;
{
    if (oldfd != newfd) {
	close(oldfd);
	dup(newfd);
	close(newfd);
    }
}
