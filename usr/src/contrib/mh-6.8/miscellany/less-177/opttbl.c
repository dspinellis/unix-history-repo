/*
 * The option table.
 */

#include "less.h"
#include "option.h"

#define	toupper(c)	((c)-'a'+'A')

/*
 * Variables controlled by command line options.
 */
public int quiet;		/* Should we suppress the audible bell? */
public int how_search;		/* Where should forward searches start? */
public int top_scroll;		/* Repaint screen from top?
				   (alternative is scroll from bottom) */
public int pr_type;		/* Type of prompt (short, medium, long) */
public int bs_mode;		/* How to process backspaces */
public int know_dumb;		/* Don't complain about dumb terminals */
public int quit_at_eof;		/* Quit after hitting end of file twice */
public int squeeze;		/* Squeeze multiple blank lines into one */
public int tabstop;		/* Tab settings */
public int back_scroll;		/* Repaint screen on backwards movement */
public int forw_scroll;		/* Repaint screen on forward movement */
public int twiddle;		/* Display "~" for lines after EOF */
public int caseless;		/* Do "caseless" searches */
public int linenums;		/* Use line numbers */
public int cbufs;		/* Current number of buffers */
public int autobuf;		/* Automatically allocate buffers as needed */
public int nohelp;		/* Disable the HELP command */
public int ctldisp;		/* Send control chars to screen untranslated */
public int force_open;		/* Open the file even if not regular file */
public int swindow;		/* Size of scrolling window */
public int jump_sline;		/* Screen line of "jump target" */
public int chopline;		/* Truncate displayed lines at screen width */
#if __MSDOS__
public int output_mode;		/* Which screen output method */
public int refresh_on_quit;	/* Repaint screen on quit, if possible */
#endif

/*
 * Table of all options and their semantics.
 */
static struct option option[] =
{
	{ 'a', BOOL, 0, &how_search, NULL,
		"Search includes displayed screen",
		"Search skips displayed screen",
		NULL
	},
	{ 'b', NUMBER, 10, &cbufs, opt_b, 
		"Buffers: ",
		"%d buffers",
		NULL
	},
	{ 'B', BOOL, 1, &autobuf, NULL,
		"Don't automatically allocate buffers",
		"Automatically allocate buffers when needed",
		NULL
	},
	{ 'c', TRIPLE, 0, &top_scroll, NULL,
		"Repaint by scrolling from bottom of screen",
		"Repaint by clearing each line",
		"Repaint by painting from top of screen"
	},
	{ 'd', BOOL|NO_TOGGLE, 0, &know_dumb, NULL,
		"Assume intelligent terminal",
		"Assume dumb terminal",
		NULL
	},
	{ 'e', TRIPLE, 0, &quit_at_eof, NULL,
		"Don't quit at end-of-file",
		"Quit at end-of-file",
		"Quit immediately at end-of-file"
	},
	{ 'f', BOOL, 0, &force_open, NULL,
		"Open only regular files",
		"Open even non-regular files",
		NULL
	},
	{ 'h', NUMBER, -1, &back_scroll, NULL,
		"Backwards scroll limit: ",
		"Backwards scroll limit is %d lines",
		NULL
	},
	{ 'H', BOOL|NO_TOGGLE, 0, &nohelp, NULL,
		"Allow help command",
		"Don't allow help command",
		NULL
	},
	{ 'i', BOOL, 0, &caseless, NULL,
		"Case is significant in searches",
		"Ignore case in searches",
		NULL
	},
	{ 'j', NUMBER, 1, &jump_sline, NULL,
		"Target line: ",
		"Position target at screen line %d",
		NULL
	},
#if USERFILE
	{ 'k', STRING|NO_TOGGLE, 0, NULL, opt_k,
		NULL, NULL, NULL
	},
#endif
#if LOGFILE
	{ 'l', STRING, 0, NULL, opt_l,
		NULL, NULL, NULL
	},
	{ 'L', STRING, 0, NULL, opt__L,
		NULL, NULL, NULL
	},
#endif
	{ 'm', TRIPLE, 0, &pr_type, NULL,
		"Short prompt",
		"Medium prompt",
		"Long prompt"
	},
	{ 'n', TRIPLE|REPAINT, 1, &linenums, NULL,
		"Don't use line numbers",
		"Use line numbers",
		"Constantly display line numbers"
	},
#if LOGFILE
	{ 'o', STRING, 0, NULL, opt_o,
		"log file: ", NULL, NULL
	},
	{ 'O', STRING, 0, NULL, opt__O,
		"Log file: ", NULL, NULL
	},
#endif
	{ 'p', STRING|NO_TOGGLE, 0, NULL, opt_p,
		NULL, NULL, NULL
	},
	{ 'P', STRING, 0, NULL, opt__P,
		"prompt: ", NULL, NULL
	},
	{ 'q', TRIPLE, 0, &quiet, NULL,
		"Ring the bell for errors AND at eof/bof",
		"Ring the bell for errors but not at eof/bof",
		"Never ring the bell"
	},
	{ 'r', BOOL|REPAINT, 1, &ctldisp, NULL,
		"Display control characters directly",
		"Display control characters as ^X",
		NULL
	},
#if __MSDOS__
	{ 'R', BOOL|REPAINT, 0, &refresh_on_quit, NULL,
		"Don't repaint screen on quit",
		"Repaint screen on quit",
		NULL
	},
#endif
	{ 's', BOOL|REPAINT, 0, &squeeze, NULL,
		"Display all blank lines",
		"Squeeze multiple blank lines",
		NULL
	},
	{ 'S', BOOL|REPAINT, 0, &chopline, NULL,
		"Fold long lines",
		"Chop long lines",
		NULL
	},
#if TAGS
	{ 't', STRING, 0, NULL, opt_t,
		"tag: ", NULL, NULL
	},
	{ 'T', STRING, 0, NULL, opt__T,
		"tags file: ", NULL, NULL
	},
#endif
	{ 'u', TRIPLE|REPAINT, 0, &bs_mode, NULL,
		"Display underlined text in underline mode",
		"Backspaces cause overstrike",
		"Print backspace as ^H"
	},
#if __MSDOS__
	{ 'v', TRIPLE|NO_TOGGLE, 0, &output_mode, opt_v,
		"Output is to standard output, using ansi screen control",
		"Output is to video BIOS",
		"Output is directly to memory mapped video"
	},
#endif
	{ 'w', BOOL|REPAINT, 1, &twiddle, NULL,
		"Display nothing for lines after end-of-file",
		"Display ~ for lines after end-of-file",
		NULL
	},
#if __MSDOS__
#if MOVE_WINDOW
#define	W_FLAGS	STRING
#else
#define	W_FLAGS	STRING|NO_TOGGLE
#endif
	{ 'W', W_FLAGS, 0, NULL, opt_W,
		"window boundaries: ", NULL, NULL
	},
#undef W_FLAGS
#endif
	{ 'x', NUMBER|REPAINT, 8, &tabstop, NULL,
		"Tab stops: ",
		"Tab stops every %d spaces", 
		NULL
	},
	{ 'y', NUMBER, -1, &forw_scroll, NULL,
		"Forward scroll limit: ",
		"Forward scroll limit is %d lines",
		NULL
	},
	{ 'z', NUMBER, -1, &swindow, NULL,
		"Scroll window size: ",
		"Scroll window size is %d lines",
		NULL
	},
	{ '?', NOVAR, 0, NULL, opt_query,
		NULL, NULL, NULL
	},
	{ '\0' }
};


/*
 * Initialize each option to its default value.
 */
	public void
init_option()
{
	register struct option *o;

	for (o = option;  o->oletter != '\0';  o++)
	{
		/*
		 * Set each variable to its default.
		 */
		if (o->ovar != NULL)
			*(o->ovar) = o->odefault;
	}
}

/*
 * Find an option in the option table.
 */
	public struct option *
findopt(c)
	int c;
{
	register struct option *o;

	for (o = option;  o->oletter != '\0';  o++)
	{
		if (o->oletter == c)
			return (o);
		if ((o->otype & TRIPLE) && toupper(o->oletter) == c)
			return (o);
	}
	return (NULL);
}
