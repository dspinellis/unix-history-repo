/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: unix.c,v 2.6 85/08/22 16:09:38 timo Exp $";

/*
 * B editor -- UNIX interface, i.e. signal and tty fiddling.
 */

/* #define BADTABSTOPS /* Obsolete -- "b" doesn't set the tabs any more */
	/* Defined if (soft) tabs may have been placed at strange positions. */
	/* Actually this has only effect if curses(3) is used.
	   However this source file doesn't #include "curses.h" so we can't
	   check for that, and will assume curses(3) is always used.
	   For very slow baudrates when curses(3) is used, it may prove useful
	   to undefine BADTABSTOPS.  The "b" shell script must then be modified
	   to keep the tabs at the UNIX 8 space apart default. */

#include "b.h" /* Only for definitions like bool, string, Hidden etc. */
#include "unix.h" /* What kind of UNIX is this? */

#ifdef SIGNAL
#include <signal.h>
#endif SIGNAL

#ifdef SGTTY_H
#include <sgtty.h>
#endif SGTTY_H

extern bool slowterminal; /* Set for speeds <= 600 baud */
extern bool hushbaby; /* Set if no bells are to be heard */
extern bool dflag; /* Debugging mode */


#define COPYSAVEFILE ".Bed_buf"

Visible char copysavefile[200] = COPYSAVEFILE;


#define Ctl(x) ('x'&037)

#ifndef QUITCHAR
#define QUITCHAR Ctl(\\)
#endif QUITCHAR

#ifndef INTRCHAR
#define INTRCHAR Ctl(])
#endif INTRCHAR

#define REDRAW Ctl(L) /* From "keys.h" */


#ifdef SIGNAL
/*
 * Call exit code when signal arrives, then resend the signal.
 */

catch(sig)
	int sig;
{
	signal(sig, SIG_DFL);
#ifndef NDEBUG
	fprintf(stderr, "*** Caught signal %d \n\r", sig);
	if (sig == SIGQUIT) { /* QUIT only resets terminal modes */
		endterm();
		endunix();
	}
	else
#endif NDEBUG
		endall();
#ifdef BTOP
	termchild(); /* Kill possible child, but don't wait for it */
#endif BTOP
	kill(getpid(), sig);
}
#endif SIGNAL


#ifdef SIGTSTP /* I.e., only on BSD systems with job control. */
/*
 * Reset tty modes etc. when STOP signal arrives (control-Z).
 * This is like interrupt but the program may continue later
 * so we must not do all exit code).
 *
 * In order that the code works for 4.1 and 4.2 BSD Unix (V7 and sys III/V
 * don't have the SIGTSTP signal at all, so there wo don't bother), we use
 * neither the awkward "-ljobs" mechanism nor the nicer but (yet!) even
 * less portable sigmask/sigblock system calls.  Rather, to kill ourselves
 * again after the screen and tty modes have been restored, we use another
 * signal, i.e., SIGSTOP (which is uncatchable).
 *
 * Note! Since curses' initscr() also executes signal(SIGTSTP, tstp),
 * and initscr() is called after initunix(), the name of this routine
 * must be tstp, overriding a routine of the same name in the curses
 * library which does not do what we want.
 */

tstp(sig)
	int sig;
{
	int (*prevttousig)() = signal(SIGTTOU, SIG_IGN);
		/* Ignore SIGTTOU so stty calls won't stop us again! */
	char cread = REDRAW;

#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** Caught stop signal %d \n\r", sig);
#endif NDEBUG
	signal(sig, SIG_DFL);
	endterm();
	unfixttymodes();
	signal(SIGTTOU, prevttousig);
	kill(getpid(), SIGSTOP); /* Hard stop */

	/*
	 * A stop signal made us go to sleep in Tumbolia.
	 * When we awake, we continue at this point.
	 * The world may well have changed a little bit,
	 * so do the tty initializations anew.
	 */

	fixttymodes();
	initterm();

#ifdef TIOCSTI
	/* Simulate receipt of REDRAW initially so we come up
	   with a nice display. */
	ioctl(0, TIOCSTI, &cread);
#endif TIOCSTI
	signal(SIGTSTP, tstp);
}
#endif SIGTSTP


/*
 * Prepare for interrupts (UNIX `signals') to be caught so
 * we can reset the tty modes and perform miscellaneous other
 * exit routines.
 * Note -- if a signal arrives before the call to fixttymodes,
 * the unfixttymodes may render the terminal useless.  The fix is
 * easy, but I'm too lazy now (just read the statuses BEFORE,
 * but change them only AFTER signal setting).
 */

initunix()
{
#ifdef SIGNAL
	register int i;
#endif SIGNAL

#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** initunix();\n\r");
#endif NDEBUG

#ifdef SIGNAL
	for (i = 1; i <= NSIG; ++i) {
#ifndef NDEBUG
		if (i == SIGQUIT)
			continue;
#endif NDEBUG
#ifdef SIGCONT
		if (i == SIGCONT)
			continue;
#endif SIGCONT
#ifdef SIGCHLD
		if (i == SIGCHLD)
			continue;
#endif SIGCHLD
		if (signal(i, SIG_IGN) != SIG_IGN) {
			signal(i, catch);
#ifndef NDEBUG
			if (dflag)
				fprintf(stderr, "Catching signal %d\n", i);
#endif NDEBUG
		}
	}
	/* Stop/continue must be handled differently, see stop() above. */
#ifdef SIGTSTP
	if (signal(SIGTSTP, SIG_IGN) != SIG_IGN)
		signal(SIGTSTP, tstp);
#endif SIGTSTP

#endif SIGNAL

#ifdef SGTTY_H
	fixttymodes();
#endif SGTTY_H
	setcopybuffer();
}


/*
 * The last termination routine to be called.
 * It also resets all signals to their default status.
 */

endunix()
{
#ifdef SIGNAL
	int i;
#endif SIGNAL

	fflush(stdout);
#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** endunix();\n\r");
#endif NDEBUG
#ifdef SGTTY_H
	unfixttymodes();
#endif SGTTY_H

#ifdef SIGNAL
	for (i = 1; i <= NSIG; ++i)
		signal(i, SIG_DFL);
#endif SIGNAL
}


/*
 * Determine the name of the file where the copy buffer is saved.
 */

Hidden Procedure
setcopybuffer()
{
	string home = getenv("HOME");

	if (home)
		sprintf(copysavefile, "%.150s/%.40s", home, COPYSAVEFILE);
	/* Else, retain default initialization! */
}


/*
 * Return a string like the one that perror(arg) would print
 * (see UNIX manual page perror(3) for details).
 * Like all C library routines returning strings, the string points
 * to static storage that is overwritten on each call.
 * If arg is fairly long, it may get truncated.
 */

string
unixerror(arg)
	string arg;
{
	static char msg[200];
#ifdef PERROR
	extern int sys_nerr, errno;
	extern string sys_errlist[];

	if (errno > 0 && errno < sys_nerr)
		sprintf(msg, "%.80s: %.80s", arg, sys_errlist[errno]);
	else
		sprintf(msg, "%.80s: UNIX error %d", arg, errno);
#else !PERROR
	sprintf(msg, "%.68s: I/O error", arg);
#endif !PERROR
	msg[80] = '\0';
	return msg;
}


#ifdef SGTTY_H
/*
 * Hacks to fix certain peculiarities due to the hostile environment
 * in which the editor lives.
 */

Hidden struct sgttyb oldtty;

#ifdef TIOCSETC
Hidden struct tchars oldtchars;
#endif

#ifdef TIOCSLTC
Hidden struct ltchars oldltchars;
#endif

Hidden Procedure
fixttymodes()
{
	gtty(2, &oldtty);
	if (oldtty.sg_ospeed <= B600)
		slowterminal = Yes;
#ifdef BADTABSTOPS
	/*
	 * Turn on XTABS mode, to be able to live when terminal tabs are
	 * set at 4 rather than 8 columns (the B interpreter used to set
	 * this).
	 */
	if (!(oldtty.sg_flags & XTABS)) {
		struct sgttyb newtty;
		gtty(2, &newtty);
		newtty.sg_flags |= XTABS;
		ioctl(0, TIOCSETN, &newtty);
	}
#endif BADTABSTOPS

#ifdef TIOCSETC /* I.e., not at pre-version 7 UNIX systems */
	/*
	 * Set the quit character to ^\ and the interrupt at DEL.
	 * The start/stop characters are kept only if they are ^S/^Q.
	 */
	{
		struct tchars newtchars;
		ioctl(0, TIOCGETC, &oldtchars);
		ioctl(0, TIOCGETC, &newtchars);
		if ((newtchars.t_intrc & 0377) != 0377
			&& newtchars.t_intrc != 0177/*DEL*/)
			newtchars.t_intrc = INTRCHAR;
		if ((newtchars.t_quitc & 0377) != 0377)
			newtchars.t_quitc = QUITCHAR;
		if (newtchars.t_startc != Ctl(Q))
			newtchars.t_startc = -1;
		if (newtchars.t_stopc != Ctl(S))
			newtchars.t_stopc = -1;
		ioctl(0, TIOCSETC, &newtchars);
	}
#endif TIOCSETC

#ifdef TIOCSLTC /* I.e., at 4.xBSD systems */
	/*
	 * Turn off all local control characters except keep stop (^Z) and delayed
	 * stop (^Y) when these are the originals.
	 */
	{
		static struct ltchars newltchars = {-1, -1, -1, -1, -1, -1};

		ioctl(0, TIOCGLTC, &oldltchars);
		if (oldltchars.t_suspc == Ctl(Z))
			newltchars.t_dsuspc = Ctl(Z);
		ioctl(0, TIOCSLTC, &newltchars);
	}
#endif
}


/*
 * Undo the effects of fixttymodes(), see comments there.
 */

Hidden Procedure
unfixttymodes()
{
	if (!oldtty.sg_ospeed)
		return; /* Not yet initialized! */
#ifdef BADTABSTOPS
	ioctl(0, TIOCSETN, &oldtty);
#endif
#ifdef TIOCSETC
	ioctl(0, TIOCSETC, &oldtchars);
#endif
#ifdef TIOCSLTC
	ioctl(0, TIOCSLTC, &oldltchars);
#endif
}
#endif SGTTY_H


/*
 * Return Yes if more input immediately available
 */

#ifdef IBMPC

Visible bool
moreinput()
{
	return kbhit();
}

#else !IBMPC

/*
 * ***** UNIX DEPENDENCE *****
 * Assumes the standard UNIX definition of FILE: assumes there is
 * buffered input if stdin->_cnt > 0, so uses the `_cnt' field.
 *
 * ***** 4.2 BSD DEPENDENCE *****
 * If the symbol SIGNAL is defined, uses the select() system call to determine
 * whether more input is available; see select(2) in 4.2 BSD manual.
 *
 * ***** 4.1 BSD DEPENDENCE *****
 * If the symbol FIONREAD is defined, uses the correponding ioctl call to
 * determine whether more input is available; see tty(4) in 4.1 BSD manual.
 */

#ifdef SELECT
#include <sys/time.h>
#endif SELECT

Visible bool
moreinput()
{
	if (stdin->_cnt > 0)
		return Yes;
#ifdef SELECT
	{
		int readfds;
		int nfds;
		static struct timeval timeout = {0, 0};

		readfds = 1<<fileno(stdin);
		nfds = 1+fileno(stdin);
		nfds = select(nfds, &readfds, (int*)0, (int*)0, &timeout);
		if (nfds > 0) {
			if (dflag)
				fputc('\07', stderr);
			return Yes;
		}
	}
#else SELECT
#ifdef FIONREAD
	{
		long n = 0;

		if (ioctl(0, FIONREAD, &n) != -1 && n > 0)
			return Yes;
	}
#endif FIONREAD
#endif SELECT
	return No;
}
#endif !IBMPC


#ifdef SETENV
/*
 * Routine to add or change an environment variable.
 * (No longer used.)
 */

extern string *environ;

setenv(entry)
	string entry;
{
	string equals = index(entry, '=');
	int len;
	string *ep;
	static string *myenviron;

	if (!equals)
		syserr("setenv: no = sign");
	len = equals - entry;
	for (ep = environ; *ep && !Strnequ(*ep, entry, len+1); ++ep)
		;
	if (*ep) {
		*ep = entry;
		return;
	}
	len = ep - environ + 2;
	if (myenviron) {
		myenviron = (string*)
			realloc((string)myenviron, (unsigned)(len * sizeof(string)));
		if (!myenviron)
			syserr("setenv: realloc");
	}
	else {
		myenviron = (string*) malloc((unsigned)(len * sizeof(string)));
		if (!myenviron)
			syserr("setenv: malloc");
		for (ep = environ; *ep; ++ep)
			myenviron[ep-environ] = *ep;
	}
	myenviron[len-1] = (string)NULL;
	myenviron[len-2] = entry;
	environ = myenviron;
}
#endif SETENV


#ifdef PWB
/*
 * Substitute getenv routine - there is no environment on PWB systems,
 * but as a substitute (not te be encouraged!) we allow a file with the
 * name of the environment variable to contain the desired value;
 * e.g. the file "TERM" may contain a line saying hp2621 or hp etc.
 */

Visible string
getenv(name)
	string name;
{
	static char buffer[100];
	FILE *fp;
	string cp;

	fp = fopen(name, "r");
	if (!fp)
		return NULL;
	if (!fgets(buffer, sizeof buffer, fp))
		buffer[0] = '\0';
	else {
		cp = index(buffer, '\n');
		if (cp)
			*cp = '\0';
	}
	fclose(fp);
	return buffer;
 }
#endif PWB
