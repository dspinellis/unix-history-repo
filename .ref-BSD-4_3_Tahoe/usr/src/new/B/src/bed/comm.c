/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: comm.c,v 2.4 85/08/22 16:00:49 timo Exp $";

/*
 * B editor -- Communication with B interpreter.
 */

#include "feat.h"
#ifdef BTOP

#include <signal.h>
#include <setjmp.h>
#include <ctype.h>

#include "b.h"
#include "node.h"
#include "supr.h"
#include "unix.h"
#include "cell.h" /* For winheight */

#define TABS 8

string unixerror();


/*
 * Communication to other modules (demo, getc, ...):
 */

Visible bool interrupted; /* Set when interrupt caught but not propagated */
Visible bool canjump; /* Set when disrupt() can safely longjmp(jumpback) */
Visible jmp_buf jumpback; /* Set by other module where to jump */

/*
 * Pipeline protocol with interpreter:
 */

#define ESCAPE '\001' /* Character signalling special function */
#define RESYNC '\177' /* Character signalling acknowledge of interrupt */
#define INTRCHILD SIGTRAP /* Signal to send as interrupt */

#ifndef INTERPRETER
#define INTERPRETER "/usr/new/lib/B/bint"
#endif

/*
 * Local definitions:
 */

#ifndef INTRMSG
#define INTRMSG "*** Interrupted" /* Acknowledges interrupt */
#endif INTRMSG

#define Moreinput(stream) ((stream)->_cnt > 0)

Hidden int fdown[2]; /* File descriptors for pipe down */
Hidden int fup[2]; /* Pipe up */

Hidden int pid; /* Process id of child */

Hidden FILE *pdown; /* FILE pointer for pipe down to child process */
Hidden FILE *pup; /* Pipe up */

Hidden string interpreter; /* Name of interpreter to be used */


Hidden char pushback[100]; /* Limited pushback facility */
Hidden int npushback; /* Number of characters pushed back */


/*
 * Routine to set canjump, do a getc, and clear canjump.
 */

Visible int
ffgetc(fp)
	FILE *fp;
{
	register int c;

	canjump = Yes;
	c = getc(fp);
	canjump = No;
	return c;
}


/*
 * Similar for fgets.
 */

Visible string
ffgets(buf, len, fp)
	string buf;
	int len;
	FILE *fp;
{
	canjump = Yes;
	buf = fgets(buf, len, fp);
	canjump = No;
	return buf;
}


/*
 * Assign values to `fdown' and `fup'.
 */

Hidden Procedure
getdevices()
{
	if (pipe(fdown) < 0 || pipe(fup) < 0)
		syserr("%s", unixerror("can't pipe"));
}


/*
 * Do the magic required for child-birth.
 */

Hidden Procedure
makechild()
{
#ifdef VFORK
	pid = vfork();
#else VFORK
	pid = fork();
#endif VFORK
	if (pid == -1)
		syserr("%s", unixerror("can't fork"));
	if (pid == 0) /* Child */
		exec_b(); /* Does not return */
	/* Parent */
	close(fdown[0]);
	close(fup[1]);
}


/*
 * Code executed in the child process.  Never returns.
 * Just dup the pipe ends to files 0, a and 2 (stdin, stdout and stderr),
 * then close the original pipes.
 */

Hidden Procedure
exec_b()
{
	close(fdown[1]), close(fup[0]);
	close(0), close(1), close(2);
	dup(fdown[0]), dup(fup[1]), dup(fup[1]);
	close(fdown[0]), close(fup[1]);
	execl(interpreter, interpreter, "-i", (char*)NULL);
	fprintf(stderr, "*** ");
	perror(interpreter);
	_exit(1);
}


/*
 * Interrupt handler.
 * Usually only the flag `interrupted' is set.
 *
 * When `canjump' is on, it is cleared and we do a longjmp
 * back to where jumpbuf leads us (usually done when a read
 * system call is interrupted, as 4.2BSD tends to continue
 * these rather than have them return with errno = EINTR).
 */

Hidden Procedure
disrupt()
{
	interrupted = Yes;
	signal(SIGINT, disrupt);
	if (canjump) {
		canjump = No;
		longjmp(jumpback, 1);
	}
}


/*
 * Start the B interpreter as a subprocess.
 * Set up communication pipes in pdown, pup.
 */

Visible Procedure
start_b(ppdown, ppup)
	FILE **ppdown;
	FILE **ppup;
{
	interpreter = getenv("B_INTERPRETER");
	if (!interpreter)
		interpreter = INTERPRETER;
	getdevices();
	makechild();
	pdown = fdopen(fdown[1], "w");
	pup = fdopen(fup[0], "r");
	if (!pdown || !pup)
		syserr("%s", unixerror("can't fdopen"));
	*ppdown = pdown;
	*ppup = pup;
	signal(SIGINT, disrupt);
}


/*
 * Routine to be called after each line of data has been passed
 * to the B interpreter; it checks whether the immediate next
 * output is a request for an immediate command, and if so,
 * eats the request and returns Yes.  Otherwise it pushes back the
 * request for later processing by sleur(), and returns No.
 * ***** The prompt parameter is a relict of old times. *****
 */

Visible bool
expect(prompt)
	string prompt; /* Only first char used; should be ">" */
{
	register int c;

	fflush(pdown);
	if (setjmp(jumpback))
		return No;
	if (npushback)
		c = pushback[--npushback];
	else
		c = ffgetc(pup);
	if (c != ESCAPE) {
		if (c != EOF)
			pushback[npushback++] = c;
		return No;
	}
	if (npushback)
		c = pushback[--npushback];
	else
		c = ffgetc(pup);
	if (c == *prompt)
		return Yes;
	if (c != EOF)
		pushback[npushback++] = c;
	pushback[npushback++] = ESCAPE;
	return No;
}


Visible int
sleur()
{
	register int c;
	register int x = 0;
	bool show = Yes; /* No when looking for interrupt sync char */
	bool idle = Yes; /* Yes when no output done yet this call */

	fflush(pdown);

	for (;;) {
		if (interrupted) {
			interrupted = No;
			intrchild();
			show = No;
		}
		if (show && npushback == 0 && !Moreinput(pup))
			fflush(stdout);
		if (setjmp(jumpback))
			continue;
		if (npushback > 0)
			c = pushback[--npushback];
		else
			c = ffgetc(pup);
		if (c == EOF) { /* End-of-file: B interpreter has terminated. */
			fflush(stdout);
			return EOF;
		}
		if (c == RESYNC) {
			/* B interpreter acknowledges interrupt. */
			if (!show) {
				if (x != 0) putchar('\n');
				fputs(INTRMSG, stdout);
				putchar('\n');
				x = 0;
				show = Yes;
			}
			continue;
		}
		if (show) {
			if (c != ESCAPE) {
				putchar(c);
				switch (c) {
				case '\t':
					x = (x/TABS + 1)*TABS;
					break;
				case '\b':
					if (x > 0) --x;
					break;
				case '\r':
				case '\n':
					x = 0;
					break;
				default:
					if (isascii(c) && isprint(c)
						|| c == ' ') ++x;
					break;
				}
			}
			else {
				/* Control-A: B interpreter needs input. */
				if (setjmp(jumpback))
					continue;
				if (npushback)
					c = pushback[--npushback];
				else {
					c = ffgetc(pup);
					if (c == EOF) {
						return EOF;
					}
				}
				if (c == '>') {
					/* Newline before command prompt */
					if (x != 0) putchar('\n');
					x = 0;
				}
				setindent(x);
				fflush(stdout);
				return c;
			}
		}
	}
}


/*
 * Send the child a termination signal (SIGTERM).
 */

Visible Procedure
termchild()
{
	if (pid) {
		kill(pid, SIGTERM);
		pid = 0;
	}
}


/*
 * Send the child an interrupt signal.  (By convention, this is SIGTRAP).
 */

Visible Procedure
intrchild()
{
	if (pid) {
		kill(pid, INTRCHILD);
		fflush(stdout);
	}
}


/*
 * Wait for child process and report abnormal exit statuses.
 */

Visible Procedure
waitchild()
{
	int k;
	int status;

	if (pid) {
		while ((k = wait(&status)) != -1) {
			if (k != pid)
#ifndef SMALLSYS
				fprintf(stderr, "*** [Pid %d status 0%o]\n", pid, status)
#endif SMALLSYS
				;
			else {
#ifndef SMALLSYS
				if (status&0377)
					fprintf(stderr, "*** Interpreter killed by signal %d%s\n",
						status&0177, status&0200 ? " - core dumped" : "");
				else if (status)
					fprintf(stderr, "*** Interpreter exit(%d)\n", status>>8);
#endif SMALLSYS
				pid = 0;
				break;
			}
		}
#ifndef SMALLSYS
		if (pid)
			fprintf(stderr, "*** Can't get interpreter status\n");
#endif SMALLSYS
		pid = 0;
	}
}

#endif BTOP
