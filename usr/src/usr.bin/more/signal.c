/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Mark Nudleman and the University of California, Berkeley.  The
 * name of Mark Nudleman or the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)signal.c	5.4 (Berkeley) %G%";
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

#ifdef SIGTSTP
/*
 * "Stop" (^Z) signal handler.
 */
	static HANDLER
stop()
{
	(void) signal(SIGTSTP, stop);
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
	signal(SIGWINCH, winch);
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
	signal(SIGWIND, winch);
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
		(void) signal(SIGINT, quit);
#ifdef SIGTSTP
		(void) signal(SIGTSTP, stop);
#endif
#ifdef SIGWINCH
		(void) signal(SIGWINCH, winch);
#else
#ifdef SIGWIND
		(void) signal(SIGWIND, winch);
#endif
#endif
	} else
	{
		/*
		 * Restore signals to defaults.
		 */
		(void) signal(SIGINT, SIG_DFL);
#ifdef SIGTSTP
		(void) signal(SIGTSTP, SIG_DFL);
#endif
#ifdef SIGWINCH
		(void) signal(SIGWINCH, SIG_IGN);
#endif
#ifdef SIGWIND
		(void) signal(SIGWIND, SIG_IGN);
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
		return;
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
		signal(SIGTTOU, SIG_IGN);
#endif
		lower_left();
		clear_eol();
		deinit();
		flush();
		raw_mode(0);
#ifdef SIGTTOU
		signal(SIGTTOU, SIG_DFL);
#endif
		signal(SIGTSTP, SIG_DFL);
		kill(getpid(), SIGTSTP);
		/*
		 * ... Bye bye. ...
		 * Hopefully we'll be back later and resume here...
		 * Reset the terminal and arrange to repaint the
		 * screen when we get back to the main command loop.
		 */
		signal(SIGTSTP, stop);
		raw_mode(1);
		init();
		screen_trashed = 1;
	}
#endif
}
