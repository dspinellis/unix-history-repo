/*
 * Routines which deal with the characteristics of the terminal.
 * Uses termcap to be as terminal-independent as possible.
 *
 * {{ Someday this should be rewritten to use curses. }}
 */

#include "less.h"
#if XENIX
#include <sys/types.h>
#include <sys/ioctl.h>
#endif

#if TERMIO
#include <termio.h>
#else
#include <sgtty.h>
#endif

#if !TERMIO && defined(TIOCGWINSZ)
#include <sys/ioctl.h>
#else
/*
 * For the Unix PC (ATT 7300 & 3B1):
 * Since WIOCGETD is defined in sys/window.h, we can't use that to decide
 * whether to include sys/window.h.  Use SIGPHONE from signal.h instead.
 */
#include <signal.h>
#ifdef SIGPHONE
#include <sys/window.h>
#endif
#endif

#if NEED_PTEM_H && defined(TIOCGWINSZ)
/*
 * All this just to get struct winsize.  Sigh.
 */
#include <sys/types.h>
#include <sys/stream.h>
#include <sys/ptem.h>
#endif

/*
 * Strings passed to tputs() to do various terminal functions.
 */
static char
	*sc_pad,		/* Pad string */
	*sc_home,		/* Cursor home */
	*sc_addline,		/* Add line, scroll down following lines */
	*sc_lower_left,		/* Cursor to last line, first column */
	*sc_move,		/* General cursor positioning */
	*sc_clear,		/* Clear screen */
	*sc_eol_clear,		/* Clear to end of line */
	*sc_s_in,		/* Enter standout (highlighted) mode */
	*sc_s_out,		/* Exit standout mode */
	*sc_u_in,		/* Enter underline mode */
	*sc_u_out,		/* Exit underline mode */
	*sc_b_in,		/* Enter bold mode */
	*sc_b_out,		/* Exit bold mode */
	*sc_bl_in,		/* Enter blink mode */
	*sc_bl_out,		/* Exit blink mode */
	*sc_visual_bell,	/* Visual bell (flash screen) sequence */
	*sc_backspace,		/* Backspace cursor */
	*sc_init,		/* Startup terminal initialization */
	*sc_deinit;		/* Exit terminal de-initialization */

static int init_done = 0;

public int auto_wrap;		/* Terminal does \r\n when write past margin */
public int ignaw;		/* Terminal ignores \n immediately after wrap */
public int erase_char, kill_char; /* The user's erase and line-kill chars */
public int sc_width, sc_height;	/* Height & width of screen */
public int bo_s_width, bo_e_width;	/* Printing width of boldface seq */
public int ul_s_width, ul_e_width;	/* Printing width of underline seq */
public int so_s_width, so_e_width;	/* Printing width of standout seq */
public int bl_s_width, bl_e_width;	/* Printing width of blink seq */

static char *cheaper();

/*
 * These two variables are sometimes defined in,
 * and needed by, the termcap library.
 * It may be necessary on some systems to declare them extern here.
 */
/*extern*/ short ospeed;	/* Terminal output baud rate */
/*extern*/ char PC;		/* Pad character */

extern int quiet;		/* If VERY_QUIET, use visual bell for bell */
extern int know_dumb;		/* Don't complain about a dumb terminal */
extern int back_scroll;
extern int swindow;
extern char *tgetstr();
extern char *tgoto();
extern char *getenv();


/*
 * Change terminal to "raw mode", or restore to "normal" mode.
 * "Raw mode" means 
 *	1. An outstanding read will complete on receipt of a single keystroke.
 *	2. Input is not echoed.  
 *	3. On output, \n is mapped to \r\n.
 *	4. \t is NOT expanded into spaces.
 *	5. Signal-causing characters such as ctrl-C (interrupt),
 *	   etc. are NOT disabled.
 * It doesn't matter whether an input \n is mapped to \r, or vice versa.
 */
	public void
raw_mode(on)
	int on;
{
	static int curr_on = 0;

	if (on == curr_on)
		return;
#if TERMIO
    {
	struct termio s;
	static struct termio save_term;

	if (on)
	{
		/*
		 * Get terminal modes.
		 */
		ioctl(2, TCGETA, &s);

		/*
		 * Save modes and set certain variables dependent on modes.
		 */
		save_term = s;
		ospeed = s.c_cflag & CBAUD;
		erase_char = s.c_cc[VERASE];
		kill_char = s.c_cc[VKILL];

		/*
		 * Set the modes to the way we want them.
		 */
		s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
		s.c_oflag |=  (OPOST|ONLCR|TAB3);
		s.c_oflag &= ~(OCRNL|ONOCR|ONLRET);
		s.c_cc[VMIN] = 1;
		s.c_cc[VTIME] = 0;
	} else
	{
		/*
		 * Restore saved modes.
		 */
		s = save_term;
	}
	ioctl(2, TCSETAW, &s);
    }
#else
    {
	struct sgttyb s;
	static struct sgttyb save_term;

	if (on)
	{
		/*
		 * Get terminal modes.
		 */
		ioctl(2, TIOCGETP, &s);

		/*
		 * Save modes and set certain variables dependent on modes.
		 */
		save_term = s;
		ospeed = s.sg_ospeed;
		erase_char = s.sg_erase;
		kill_char = s.sg_kill;

		/*
		 * Set the modes to the way we want them.
		 */
		s.sg_flags |= CBREAK;
		s.sg_flags &= ~(ECHO|XTABS);
	} else
	{
		/*
		 * Restore saved modes.
		 */
		s = save_term;
	}
	ioctl(2, TIOCSETN, &s);
    }
#endif
	curr_on = on;
}

	static void
cannot(s)
	char *s;
{
	PARG parg;

	if (know_dumb)
		/* 
		 * User knows this is a dumb terminal, so don't tell him.
		 */
		return;

	parg.p_string = s;
	error("WARNING: terminal cannot %s", &parg);
}

/*
 * Get size of the output screen.
 */
	public void
scrsize(p_height, p_width)
	int *p_height;
	int *p_width;
{
	register char *s;
#ifdef TIOCGWINSZ
	struct winsize w;
#else
#ifdef WIOCGETD
	struct uwdata w;
#endif
#endif

#ifdef TIOCGWINSZ
	if (ioctl(2, TIOCGWINSZ, &w) == 0 && w.ws_row > 0)
		*p_height = w.ws_row;
	else
#else
#ifdef WIOCGETD
	if (ioctl(2, WIOCGETD, &w) == 0 && w.uw_height > 0)
		*p_height = w.uw_height/w.uw_vs;
	else
#endif
#endif
	if ((s = getenv("LINES")) != NULL)
		*p_height = atoi(s);
	else
 		*p_height = tgetnum("li");

	if (*p_height <= 0)
		*p_height = 24;

#ifdef TIOCGWINSZ
 	if (ioctl(2, TIOCGWINSZ, &w) == 0 && w.ws_col > 0)
		*p_width = w.ws_col;
	else
#ifdef WIOCGETD
	if (ioctl(2, WIOCGETD, &w) == 0 && w.uw_width > 0)
		*p_width = w.uw_width/w.uw_hs;
	else
#endif
#endif
	if ((s = getenv("COLUMNS")) != NULL)
		*p_width = atoi(s);
	else
 		*p_width = tgetnum("co");

 	if (*p_width <= 0)
  		*p_width = 80;
}

/*
 * Get terminal capabilities via termcap.
 */
	public void
get_term()
{
	char *sp;
	register char *t1, *t2;
	register int hard;
	char *term;
	char termbuf[2048];

	static char sbuf[1024];

	/*
	 * Find out what kind of terminal this is.
	 */
 	if ((term = getenv("TERM")) == NULL)
 		term = "unknown";
 	if (tgetent(termbuf, term) <= 0)
 		strcpy(termbuf, "dumb:hc:");

 	hard = tgetflag("hc");

	/*
	 * Get size of the screen.
	 */
	scrsize(&sc_height, &sc_width);
	pos_init();
	if (swindow < 0)
		swindow = sc_height - 1;

	auto_wrap = tgetflag("am");
	ignaw = tgetflag("xn");

	/*
	 * Assumes termcap variable "sg" is the printing width of:
	 * the standout sequence, the end standout sequence,
	 * the underline sequence, the end underline sequence,
	 * the boldface sequence, and the end boldface sequence.
	 */
	if ((so_s_width = tgetnum("sg")) < 0)
		so_s_width = 0;
	so_e_width = so_s_width;

	bo_s_width = bo_e_width = so_s_width;
	ul_s_width = ul_e_width = so_s_width;
	bl_s_width = bl_e_width = so_s_width;

	/*
	 * Get various string-valued capabilities.
	 */
	sp = sbuf;

	sc_pad = tgetstr("pc", &sp);
	if (sc_pad != NULL)
		PC = *sc_pad;

	sc_init = tgetstr("ti", &sp);
	if (sc_init == NULL)
		sc_init = "";

	sc_deinit= tgetstr("te", &sp);
	if (sc_deinit == NULL)
		sc_deinit = "";

	sc_eol_clear = tgetstr("ce", &sp);
	if (hard || sc_eol_clear == NULL || *sc_eol_clear == '\0')
	{
		cannot("clear to end of line");
		sc_eol_clear = "";
	}

	sc_clear = tgetstr("cl", &sp);
	if (hard || sc_clear == NULL || *sc_clear == '\0')
	{
		cannot("clear screen");
		sc_clear = "\n\n";
	}

	sc_move = tgetstr("cm", &sp);
	if (hard || sc_move == NULL || *sc_move == '\0')
	{
		/*
		 * This is not an error here, because we don't 
		 * always need sc_move.
		 * We need it only if we don't have home or lower-left.
		 */
		sc_move = "";
	}

	sc_s_in = tgetstr("so", &sp);
	if (hard || sc_s_in == NULL)
		sc_s_in = "";

	sc_s_out = tgetstr("se", &sp);
	if (hard || sc_s_out == NULL)
		sc_s_out = "";

	sc_u_in = tgetstr("us", &sp);
	if (hard || sc_u_in == NULL)
		sc_u_in = sc_s_in;

	sc_u_out = tgetstr("ue", &sp);
	if (hard || sc_u_out == NULL)
		sc_u_out = sc_s_out;

	sc_b_in = tgetstr("md", &sp);
	if (hard || sc_b_in == NULL)
	{
		sc_b_in = sc_s_in;
		sc_b_out = sc_s_out;
	} else
	{
		sc_b_out = tgetstr("me", &sp);
		if (hard || sc_b_out == NULL)
			sc_b_out = "";
	}

	sc_bl_in = tgetstr("mb", &sp);
	if (hard || sc_bl_in == NULL)
	{
		sc_bl_in = sc_s_in;
		sc_bl_out = sc_s_out;
	} else
	{
		sc_bl_out = sc_b_out;
	}

	sc_visual_bell = tgetstr("vb", &sp);
	if (hard || sc_visual_bell == NULL)
		sc_visual_bell = "";

	if (tgetflag("bs"))
		sc_backspace = "\b";
	else
	{
		sc_backspace = tgetstr("bc", &sp);
		if (sc_backspace == NULL || *sc_backspace == '\0')
			sc_backspace = "\b";
	}

	/*
	 * Choose between using "ho" and "cm" ("home" and "cursor move")
	 * to move the cursor to the upper left corner of the screen.
	 */
	t1 = tgetstr("ho", &sp);
	if (hard || t1 == NULL)
		t1 = "";
	if (*sc_move == '\0')
		t2 = "";
	else
	{
		strcpy(sp, tgoto(sc_move, 0, 0));
		t2 = sp;
		sp += strlen(sp) + 1;
	}
	sc_home = cheaper(t1, t2, "home cursor", "|\b^");

	/*
	 * Choose between using "ll" and "cm"  ("lower left" and "cursor move")
	 * to move the cursor to the lower left corner of the screen.
	 */
	t1 = tgetstr("ll", &sp);
	if (hard || t1 == NULL)
		t1 = "";
	if (*sc_move == '\0')
		t2 = "";
	else
	{
		strcpy(sp, tgoto(sc_move, 0, sc_height-1));
		t2 = sp;
		sp += strlen(sp) + 1;
	}
	sc_lower_left = cheaper(t1, t2,
		"move cursor to lower left of screen", "\r");

	/*
	 * Choose between using "al" or "sr" ("add line" or "scroll reverse")
	 * to add a line at the top of the screen.
	 */
	t1 = tgetstr("al", &sp);
	if (hard || t1 == NULL)
		t1 = "";
	t2 = tgetstr("sr", &sp);
	if (hard || t2 == NULL)
		t2 = "";
	sc_addline = cheaper(t1, t2, "scroll backwards", "");
	if (*sc_addline == '\0')
	{
		/*
		 * Force repaint on any backward movement.
		 */
		back_scroll = 0;
	}
}

/*
 * Return the cost of displaying a termcap string.
 * We use the trick of calling tputs, but as a char printing function
 * we give it inc_costcount, which just increments "costcount".
 * This tells us how many chars would be printed by using this string.
 * {{ Couldn't we just use strlen? }}
 */
static int costcount;

/*ARGSUSED*/
	static void
inc_costcount(c)
	int c;
{
	costcount++;
}

	static int
cost(t)
	char *t;
{
	costcount = 0;
	tputs(t, sc_height, inc_costcount);
	return (costcount);
}

/*
 * Return the "best" of the two given termcap strings.
 * The best, if both exist, is the one with the lower 
 * cost (see cost() function).
 */
	static char *
cheaper(t1, t2, doit, def)
	char *t1, *t2;
	char *doit;
	char *def;
{
	if (*t1 == '\0' && *t2 == '\0')
	{
		cannot(doit);
		return (def);
	}
	if (*t1 == '\0')
		return (t2);
	if (*t2 == '\0')
		return (t1);
	if (cost(t1) < cost(t2))
		return (t1);
	return (t2);
}


/*
 * Below are the functions which perform all the 
 * terminal-specific screen manipulation.
 */


/*
 * Initialize terminal
 */
	public void
init()
{
	tputs(sc_init, sc_height, putchr);
	init_done = 1;
}

/*
 * Deinitialize terminal
 */
	public void
deinit()
{
	if (!init_done)
		return;
	tputs(sc_deinit, sc_height, putchr);
	init_done = 0;
}

/*
 * Home cursor (move to upper left corner of screen).
 */
	public void
home()
{
	tputs(sc_home, 1, putchr);
}

/*
 * Add a blank line (called with cursor at home).
 * Should scroll the display down.
 */
	public void
add_line()
{
	tputs(sc_addline, sc_height, putchr);
}

/*
 * Move cursor to lower left corner of screen.
 */
	public void
lower_left()
{
	tputs(sc_lower_left, 1, putchr);
}

/*
 * Ring the terminal bell.
 */
	public void
bell()
{
	if (quiet == VERY_QUIET)
		vbell();
	else
		putchr('\7');
}

/*
 * Output the "visual bell", if there is one.
 */
	public void
vbell()
{
	if (*sc_visual_bell == '\0')
		return;
	tputs(sc_visual_bell, sc_height, putchr);
}

/*
 * Clear the screen.
 */
	public void
clear()
{
	tputs(sc_clear, sc_height, putchr);
}

/*
 * Clear from the cursor to the end of the cursor's line.
 * {{ This must not move the cursor. }}
 */
	public void
clear_eol()
{
	tputs(sc_eol_clear, 1, putchr);
}

/*
 * Begin "standout" (bold, underline, or whatever).
 */
	public void
so_enter()
{
	tputs(sc_s_in, 1, putchr);
}

/*
 * End "standout".
 */
	public void
so_exit()
{
	tputs(sc_s_out, 1, putchr);
}

/*
 * Begin "underline" (hopefully real underlining, 
 * otherwise whatever the terminal provides).
 */
	public void
ul_enter()
{
	tputs(sc_u_in, 1, putchr);
}

/*
 * End "underline".
 */
	public void
ul_exit()
{
	tputs(sc_u_out, 1, putchr);
}

/*
 * Begin "bold"
 */
	public void
bo_enter()
{
	tputs(sc_b_in, 1, putchr);
}

/*
 * End "bold".
 */
	public void
bo_exit()
{
	tputs(sc_b_out, 1, putchr);
}

/*
 * Begin "blink"
 */
	public void
bl_enter()
{
	tputs(sc_bl_in, 1, putchr);
}

/*
 * End "blink".
 */
	public void
bl_exit()
{
	tputs(sc_bl_out, 1, putchr);
}

/*
 * Erase the character to the left of the cursor 
 * and move the cursor left.
 */
	public void
backspace()
{
	/* 
	 * Try to erase the previous character by overstriking with a space.
	 */
	tputs(sc_backspace, 1, putchr);
	putchr(' ');
	tputs(sc_backspace, 1, putchr);
}

/*
 * Output a plain backspace, without erasing the previous char.
 */
	public void
putbs()
{
	tputs(sc_backspace, 1, putchr);
}
