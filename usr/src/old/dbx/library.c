/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)library.c	5.5 (Berkeley) 3/5/91";
#endif /* not lint */

/*
 * General purpose routines.
 */

#include <stdio.h>
#include <errno.h>
#include <signal.h>

#define public
#define private static
#define and &&
#define or ||
#define not !
#define ord(enumcon)	((int) enumcon)
#define nil(type)	((type) 0)

typedef int integer;
typedef enum { FALSE, TRUE } boolean;
typedef char *String;
typedef FILE *File;
typedef String Filename;

#undef FILE

String cmdname;			/* name of command for error messages */
Filename errfilename;		/* current file associated with error */
short errlineno;		/* line number associated with error */

/*
 * Definitions for doing memory allocation.
 */

extern char *malloc();

#define alloc(n, type)	((type *) malloc((unsigned) (n) * sizeof(type)))
#define dispose(p)	{ free((char *) p); p = 0; }

/*
 * Macros for doing freads + fwrites.
 */

#define get(fp, var)	fread((char *) &(var), sizeof(var), 1, fp)
#define put(fp, var)	fwrite((char *) &(var), sizeof(var), 1, fp)

/*
 * String definitions.
 */

extern String strcpy(), index(), rindex();
extern int strlen();

#define strdup(s)		strcpy(malloc((unsigned) strlen(s) + 1), s)
#define streq(s1, s2)	(strcmp(s1, s2) == 0)

typedef int IntFunc();

IntFunc *onsyserr();

typedef struct {
    IntFunc *func;
} ErrInfo;

#define ERR_IGNORE ((IntFunc *) 0)
#define ERR_CATCH  ((IntFunc *) 1)

/*
 * Call a program.
 *
 * Four entries:
 *
 *	call, callv - call a program and wait for it, returning status
 *	back, backv - call a program and don't wait, returning process id
 *
 * The command's standard input and output are passed as FILE's.
 */


#define MAXNARGS 1000    /* unchecked upper limit on max num of arguments */
#define BADEXEC 127	/* exec fails */

#define ischild(pid)    ((pid) == 0)

/* VARARGS3 */
public int call(name, in, out, args)
String name;
File in;
File out;
String args;
{
    String *ap, *argp;
    String argv[MAXNARGS];

    argp = &argv[0];
    *argp++ = name;
    ap = &args;
    while (*ap != nil(String)) {
	*argp++ = *ap++;
    }
    *argp = nil(String);
    return callv(name, in, out, argv);
}

/* VARARGS3 */
public int back(name, in, out, args)
String name;
File in;
File out;
String args;
{
    String *ap, *argp;
    String argv[MAXNARGS];

    argp = &argv[0];
    *argp++ = name;
    ap = &args;
    while (*ap != nil(String)) {
	*argp++ = *ap++;
    }
    *argp = nil(String);
    return backv(name, in, out, argv);
}

public int callv(name, in, out, argv)
String name;
File in;
File out;
String *argv;
{
    int pid, status;

    pid = backv(name, in, out, argv);
    pwait(pid, &status);
    return status;
}

public int backv(name, in, out, argv)
String name;
File in;
File out;
String *argv;
{
    int pid;

    fflush(stdout);
    if (ischild(pid = fork())) {
	fswap(0, fileno(in));
	fswap(1, fileno(out));
	onsyserr(EACCES, ERR_IGNORE);
	execvp(name, argv);
	_exit(BADEXEC);
    }
    return pid;
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

/*
 * Invoke a shell on a command line.
 */

#define DEF_SHELL	"csh"

public shell(s)
String s;
{
    extern String getenv();
    String sh;

    if ((sh = getenv("SHELL")) == nil(String)) {
	sh = DEF_SHELL;
    }
    if (s != nil(String) and *s != '\0') {
	call(sh, stdin, stdout, "-c", s, 0);
    } else {
	call(sh, stdin, stdout, 0);
    }
}

/*
 * Wait for a process the right way.  We wait for a particular
 * process and if any others come along in between, we remember them
 * in case they are eventually waited for.
 *
 * This routine is not very efficient when the number of processes
 * to be remembered is large.
 *
 * To deal with a kernel idiosyncrasy, we keep a list on the side
 * of "traced" processes, and do not notice them when waiting for
 * another process.
 */

typedef struct pidlist {
    int pid;
    int status;
    struct pidlist *next;
} Pidlist;

private Pidlist *pidlist, *ptrclist, *pfind();

public ptraced(pid)
int pid;
{
    Pidlist *p;

    p = alloc(1, Pidlist);
    p->pid = pid;
    p->next = ptrclist;
    ptrclist = p;
}

public unptraced(pid)
int pid;
{
    register Pidlist *p, *prev;

    prev = nil(Pidlist *);
    p = ptrclist;
    while (p != nil(Pidlist *) and p->pid != pid) {
	prev = p;
	p = p->next;
    }
    if (p != nil(Pidlist *)) {
	if (prev == nil(Pidlist *)) {
	    ptrclist = p->next;
	} else {
	    prev->next = p->next;
	}
	dispose(p);
    }
}

private boolean isptraced(pid)
int pid;
{
    register Pidlist *p;

    p = ptrclist;
    while (p != nil(Pidlist *) and p->pid != pid) {
	p = p->next;
    }
    return (boolean) (p != nil(Pidlist *));
}

public pwait(pid, statusp)
int pid, *statusp;
{
    Pidlist *p;
    int pnum, status;

    p = pfind(pid);
    if (p != nil(Pidlist *)) {
	*statusp = p->status;
	dispose(p);
    } else {
	pnum = wait(&status);
	while (pnum != pid and pnum >= 0) {
	    if (not isptraced(pnum)) {
		p = alloc(1, Pidlist);
		p->pid = pnum;
		p->status = status;
		p->next = pidlist;
		pidlist = p;
	    }
	    pnum = wait(&status);
	}
	if (pnum < 0) {
	    p = pfind(pid);
	    if (p == nil(Pidlist *)) {
		panic("pwait: pid %d not found", pid);
	    }
	    *statusp = p->status;
	    dispose(p);
	} else {
	    *statusp = status;
	}
    }
}

/*
 * Look for the given process id on the pidlist.
 *
 * Unlink it from list if found.
 */

private Pidlist *pfind(pid)
int pid;
{
    register Pidlist *p, *prev;

    prev = nil(Pidlist *);
    for (p = pidlist; p != nil(Pidlist *); p = p->next) {
	if (p->pid == pid) {
	    break;
	}
	prev = p;
    }
    if (p != nil(Pidlist *)) {
	if (prev == nil(Pidlist *)) {
	    pidlist = p->next;
	} else {
	    prev->next = p->next;
	}
    }
    return p;
}

/*
 * System call error handler.
 *
 * The syserr routine is called when a system call is about to
 * set the c-bit to report an error.  Certain errors are caught
 * and cause the process to print a message and immediately exit.
 */

extern int sys_nerr;
extern char *sys_errlist[];
 
/*
 * Before calling syserr, the integer errno is set to contain the
 * number of the error.  The routine "_mycerror" is a dummy which
 * is used to force the loader to get my version of cerror rather
 * than the usual one.
 */

extern int errno;
extern _mycerror();

/*
 * Initialize error information, setting defaults for handling errors.
 */

private ErrInfo *errinfo;

private initErrInfo ()
{
    integer i;

    errinfo = alloc(sys_nerr, ErrInfo);
    for (i = 0; i < sys_nerr; i++) {
	errinfo[i].func = ERR_CATCH;
    }
    errinfo[0].func = ERR_IGNORE;
    errinfo[EPERM].func = ERR_IGNORE;
    errinfo[ENOENT].func = ERR_IGNORE;
    errinfo[ESRCH].func = ERR_IGNORE;
    errinfo[EBADF].func = ERR_IGNORE;
    errinfo[ENOTTY].func = ERR_IGNORE;
    errinfo[EOPNOTSUPP].func = ERR_IGNORE;
}

public syserr()
{
    register ErrInfo *e;

    if (errno < 0 or errno > sys_nerr) {
	fatal("errno %d", errno);
    } else {
	if (errinfo == nil(ErrInfo *)) {
	    initErrInfo();
	}
	e = &(errinfo[errno]);
	if (e->func == ERR_CATCH) {
	    fatal(sys_errlist[errno]);
	} else if (e->func != ERR_IGNORE) {
	    (*e->func)();
	}
    }
}

/*
 * Catcherrs' purpose is to initialize the errinfo table, get this module
 * loaded, and make sure my cerror is loaded (only applicable when this is
 * in a library).
 */

public catcherrs()
{
    _mycerror();
    initErrInfo();
}

/*
 * Turn off the error catching mechanism completely by having all errors
 * ignored.  This is most useful between a fork and an exec.
 */

public nocatcherrs()
{
    integer i;

    for (i = 0; i < sys_nerr; i++) {
	errinfo[i].func = ERR_IGNORE;
    }
}

/*
 * Change the action on receipt of an error, returning the previous action.
 */

public IntFunc *onsyserr(n, f)
int n;
IntFunc *f;
{
    IntFunc *oldf;

    if (errinfo == nil(ErrInfo *)) {
	initErrInfo();
    }
    oldf = errinfo[n].func;
    errinfo[n].func = f;
    return oldf;
}

/*
 * Print the message associated with the given signal.
 * Like a "perror" for signals.
 */

#ifdef SIGWINCH
public int sys_nsig = NSIG;
#else not 4.3 BSD
/*
 * This table is correct for 4.2-like systems but will
 * be inadequate for System V (which is the sort of
 * Unix that needs it!).
 */
public String sys_siglist[] = {
    "no signal",
    "hangup",
    "interrupt",
    "quit",
    "illegal instruction",
    "trace trap",
    "IOT instruction",
    "EMT instruction",
    "floating point exception",
    "kill",
    "bus error",
    "segmentation violation",
    "bad argument to system call",
    "broken pipe",
    "alarm clock",
    "soft kill",
    "urgent I/O condition",
    "stop signal not from tty",
    "stop signal from tty",
    "continue",
    "child termination",
    "stop (tty input)",
    "stop (tty output)",
    "possible input/output",
    "exceeded CPU time limit",
    "exceeded file size limit"
};
public int sys_nsig = sizeof sys_siglist / sizeof sys_siglist[0];

public psignal(s, n)
String s;
integer n;
{
    String msg;
    integer len;
    extern String sys_siglist[];

    if (n >= 0 and n < sys_nsig) {
	msg = sys_siglist[n];
    } else {
	msg = "Unknown signal";
    }
    len = strlen(s);
    if (len > 0) {
	write(2, s, len);
	write(2, ": ", 2);
    }
    write(2, msg, strlen(msg));
    write(2, "\n", 1);
}
#endif

/*
 * Standard error handling routines.
 */

private short nerrs;
private short nwarnings;

/*
 * Main driver of error message reporting.
 */

/* VARARGS2 */
private errmsg(errname, shouldquit, s, a, b, c, d, e, f, g, h, i, j, k, l, m)
String errname;
boolean shouldquit;
String s;
{
    fflush(stdout);
    if (shouldquit and cmdname != nil(String)) {
	fprintf(stderr, "%s: ", cmdname);
    }
    if (errfilename != nil(Filename)) {
	fprintf(stderr, "%s: ", errfilename);
    }
    if (errlineno > 0) {
	fprintf(stderr, "%d: ", errlineno);
    }
    if (errname != nil(String)) {
	fprintf(stderr, "%s: ", errname);
    }
    fprintf(stderr, s, a, b, c, d, e, f, g, h, i, j, k, l, m);
    putc('\n', stderr);
    fflush(stderr);
    if (shouldquit) {
	quit(1);
    }
}

/*
 * For when printf isn't sufficient for printing the error message ...
 */

public beginerrmsg()
{
    fflush(stdout);
    if (errfilename != nil(String)) {
	fprintf(stderr, "%s: ", errfilename);
    }
    if (errlineno > 0) {
	fprintf(stderr, "%d: ", errlineno);
    }
}

public enderrmsg()
{
    putc('\n', stderr);
    fflush(stderr);
    erecover();
}

/*
 * The messages are listed in increasing order of seriousness.
 *
 * First are warnings.
 */

/* VARARGS1 */
public warning(s, a, b, c, d, e, f, g, h, i, j, k, l, m)
String s;
{
    nwarnings++;
    errmsg("warning", FALSE, s, a, b, c, d, e, f, g, h, i, j, k, l, m);
}

/*
 * Errors are a little worse, they mean something is wrong,
 * but not so bad that processing can't continue.
 *
 * The routine "erecover" is called to recover from the error,
 * a default routine is provided that does nothing.
 */

/* VARARGS1 */
public error(s, a, b, c, d, e, f, g, h, i, j, k, l, m)
String s;
{
    extern erecover();

    nerrs++;
    errmsg(nil(String), FALSE, s, a, b, c, d, e, f, g, h, i, j, k, l, m);
    erecover();
}

/*
 * Non-recoverable user error.
 */

/* VARARGS1 */
public fatal(s, a, b, c, d, e, f, g, h, i, j, k, l, m)
String s;
{
    errmsg("fatal error", TRUE, s, a, b, c, d, e, f, g, h, i, j, k, l, m);
}

/*
 * Panics indicate an internal program error.
 */

/* VARARGS1 */
public panic(s, a, b, c, d, e, f, g, h, i, j, k, l, m)
String s;
{
    errmsg("internal error", TRUE, s, a, b, c, d, e, f, g, h, i, j, k, l, m);
}

short numerrors()
{
    short r;

    r = nerrs;
    nerrs = 0;
    return r;
}

short numwarnings()
{
    short r;

    r = nwarnings;
    nwarnings = 0;
    return r;
}

/*
 * Recover from an error.
 *
 * This is the default routine which we aren't using since we have our own.
 *
public erecover()
{
}
 *
 */

/*
 * Default way to quit from a program is just to exit.
 *
public quit(r)
int r;
{
    exit(r);
}
 *
 */

/*
 * Compare n-byte areas pointed to by s1 and s2
 * if n is 0 then compare up until one has a null byte.
 */

public int cmp(s1, s2, n)
register char *s1, *s2;
register unsigned int n;
{
    if (s1 == nil(char *) || s2 == nil(char *)) {
	panic("cmp: nil pointer");
    }
    if (n == 0) {
	while (*s1 == *s2++) {
	    if (*s1++ == '\0') {
		return(0);
	    }
	}
	return(*s1 - *(s2-1));
    } else {
	for (; n != 0; n--) {
	    if (*s1++ != *s2++) {
		return(*(s1-1) - *(s2-1));
	    }
	}
	return(0);
    }
}

/*
 * Move n bytes from src to dest.
 * If n is 0 move until a null is found.
 */

public mov(src, dest, n)
register char *src, *dest;
register unsigned int n;
{
    if (src == nil(char *))
	panic("mov: nil source");
    if (dest == nil(char *))
	panic("mov: nil destination");
    if (n != 0) {
	for (; n != 0; n--) {
	    *dest++ = *src++;
	}
    } else {
	while ((*dest++ = *src++) != '\0');
    }
}

#ifdef IRIS /* or in general for 4.2 - System V C library interface */

public bcopy (fromaddr, toaddr, n)
char *fromaddr, *toaddr;
int n;
{
    blt(toaddr, fromaddr, n);
}

public bzero (addr, n)
char *addr;
int n;
{
    register char *p, *q;

    p = addr;
    q = p + n;
    while (p < q) {
	*p++ = '\0';
    }
}

#include <string.h>

public char *index (s, c)
char *s, c;
{
    return strchr(s, c);
}

public char *rindex (s, c)
char *s, c;
{
    return strrchr(s, c);
}

#endif
