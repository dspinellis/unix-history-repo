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
extern int swindow;
extern int screen_trashed;
extern int lnloop;
extern int linenums;
extern int scroll;
extern int reading;

/*
 * Interrupt signal handler.
 */
	/* ARGSUSED*/
	static HANDLER
u_interrupt(type)
	int type;
{
	SIGNAL(SIGINT, u_interrupt);
	sigs |= S_INTERRUPT;
	if (reading)
		intread();
}

	public void
fake_interrupt()
{
	sigs |= S_INTERRUPT;
}

#ifdef SIGTSTP
/*
 * "Stop" (^Z) signal handler.
 */
	/* ARGSUSED*/
	static HANDLER
stop(type)
	int type;
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
	/* ARGSUSED*/
	public HANDLER
winch(type)
	int type;
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
	/* ARGSUSED*/
	public HANDLER
winch(type)
	int type;
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
		(void) SIGNAL(SIGINT, u_interrupt);
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
	public void
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
		swindow = -1;
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
		 *    error("Interrupt", NULL_PARG); }}
		 */

		/*
		 * If we were interrupted while in the "calculating 
		 * line numbers" loop, turn off line numbers.
		 */
		if (lnloop)
		{
			lnloop = 0;
			if (linenums == 2)
				screen_trashed = 1;
			linenums = 0;
			error("Line numbers turned off", NULL_PARG);
		}

	}
}
