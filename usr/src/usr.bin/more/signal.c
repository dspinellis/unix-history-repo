/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mark Nudleman.
 * 
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)signal.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routines dealing with signals.
 *
 * A signal usually merely causes a bit to be set in the "signals" word.
 * At some convenient time, the mainline code checks to see if any
 * signals need processing by calling psignal().
 * If we happen to be reading from a file [in iread()] at the time
 * the signal is received, we call intread to interrupt the iread.
 */

#include "less.h"
#include <signal.h>

/*
 * "sigs" contains bits indicating signals which need to be processed.
 */
public int sigs;

#define	S_INTERRUPT	01
#ifdef SIGTSTP
#define	S_STOP		02
#endif
#if defined(SIGWINCH) || defined(SIGWIND)
#define S_WINCH		04
#endif

extern int sc_width, sc_height;
extern int screen_trashed;
extern int lnloop;
extern int linenums;
extern int scroll;
extern int reading;

/*
 * Interrupt signal handler.
 */
	static HANDLER
interrupt()
{
	SIGNAL(SIGINT, interrupt);
	sigs |= S_INTERRUPT;
	if (reading)
		intread();
}

#ifdef SIGTSTP
/*
 * "Stop" (^Z) signal handler.
 */
	static HANDLER
stop()
{
	SIGNAL(SIGTSTP, stop);
	sigs |= S_STOP;
	if (reading)
		intread();
}
#endif

#ifdef SIGWINCH
/*
 * "Window" change handler
 */
	public HANDLER
winch()
{
	SIGNAL(SIGWINCH, winch);
	sigs |= S_WINCH;
	if (reading)
		intread();
}
#else
#ifdef SIGWIND
/*
 * "Window" change handler
 */
	public HANDLER
winch()
{
	SIGNAL(SIGWIND, winch);
	sigs |= S_WINCH;
	if (reading)
		intread();
}
#endif
#endif

/*
 * Set up the signal handlers.
 */
	public void
init_signals(on)
	int on;
{
	if (on)
	{
		/*
		 * Set signal handlers.
		 */
		(void) SIGNAL(SIGINT, interrupt);
#ifdef SIGTSTP
		(void) SIGNAL(SIGTSTP, stop);
#endif
#ifdef SIGWINCH
		(void) SIGNAL(SIGWINCH, winch);
#else
#ifdef SIGWIND
		(void) SIGNAL(SIGWIND, winch);
#endif
#endif
	} else
	{
		/*
		 * Restore signals to defaults.
		 */
		(void) SIGNAL(SIGINT, SIG_DFL);
#ifdef SIGTSTP
		(void) SIGNAL(SIGTSTP, SIG_DFL);
#endif
#ifdef SIGWINCH
		(void) SIGNAL(SIGWINCH, SIG_IGN);
#endif
#ifdef SIGWIND
		(void) SIGNAL(SIGWIND, SIG_IGN);
#endif
	}
}

/*
 * Process any signals we have received.
 * A received signal cause a bit to be set in "sigs".
 */
	public int
psignals()
{
	register int tsignals;

	if ((tsignals = sigs) == 0)
		return (0);
	sigs = 0;

#ifdef S_WINCH
	if (tsignals & S_WINCH)
	{
		int old_width, old_height;
		/*
		 * Re-execute get_term() to read the new window size.
		 */
		old_width = sc_width;
		old_height = sc_height;
		get_term();
		if (sc_width != old_width || sc_height != old_height)
		{
			scroll = (sc_height + 1) / 2;
			screen_trashed = 1;
		}
	}
#endif
#ifdef SIGTSTP
	if (tsignals & S_STOP)
	{
		/*
		 * Clean up the terminal.
		 */
#ifdef SIGTTOU
		SIGNAL(SIGTTOU, SIG_IGN);
#endif
		lower_left();
		clear_eol();
		deinit();
		flush();
		raw_mode(0);
#ifdef SIGTTOU
		SIGNAL(SIGTTOU, SIG_DFL);
#endif
		SIGNAL(SIGTSTP, SIG_DFL);
		kill(getpid(), SIGTSTP);
		/*
		 * ... Bye bye. ...
		 * Hopefully we'll be back later and resume here...
		 * Reset the terminal and arrange to repaint the
		 * screen when we get back to the main command loop.
		 */
		SIGNAL(SIGTSTP, stop);
		raw_mode(1);
		init();
		screen_trashed = 1;
	}
#endif
	if (tsignals & S_INTERRUPT)
	{
		bell();
		/*
		 * {{ You may wish to replace the bell() with 
		 *    error("Interrupt"); }}
		 */

		/*
		 * If we were interrupted while in the "calculating 
		 * line numbers" loop, turn off line numbers.
		 */
		if (lnloop)
		{
			lnloop = 0;
			linenums = 0;
			error("Line numbers turned off");
		}

	}

	return (1);
}
