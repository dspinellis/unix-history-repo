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
static char sccsid[] = "@(#)screen.c	5.1 (Berkeley) %G%";
#endif /* not lint */

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

#ifdef TIOCGWINSZ
#include <sys/ioctl.h>
#else
/*
 * For the Unix PC (ATT 7300 & 3B1):
 * Since WIOCGETD is defined in sys/window.h, we can't use that to decide
 * whether to include sys/window.h.  Use SIGPHONE from sys/signal.h instead.
 */
#include <sys/signal.h>
#ifdef SIGPHONE
#include <sys/window.h>
#endif
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
	*sc_visual_bell,	/* Visual bell (flash screen) sequence */
	*sc_backspace,		/* Backspace cursor */
	*sc_init,		/* Startup terminal initialization */
	*sc_deinit;		/* Exit terminal de-intialization */

public int auto_wrap;		/* Terminal does \r\n when write past margin */
public int ignaw;		/* Terminal ignores \n immediately after wrap */
public int erase_char, kill_char; /* The user's erase and line-kill chars */
public int sc_width, sc_height;	/* Height & width of screen */
public int sc_window = -1;	/* window size for forward and backward */
public int bo_width, be_width;	/* Printing width of boldface sequences */
public int ul_width, ue_width;	/* Printing width of underline sequences */
public int so_width, se_width;	/* Printing width of standout sequences */

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
char *tgetstr();
char *tgoto();

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
#if TERMIO
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
#else
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
#endif
}

	static void
cannot(s)
	char *s;
{
	char message[100];

	if (know_dumb)
		/* 
		 * He knows he has a dumb terminal, so don't tell him. 
		 */
		return;

	sprintf(message, "WARNING: terminal cannot \"%s\"", s);
	error(message);
}

/*
 * Get terminal capabilities via termcap.
 */
	public void
get_term()
{
	char termbuf[2048];
	char *sp;
	char *term;
	int hard;
#ifdef TIOCGWINSZ
	struct winsize w;
#else
#ifdef WIOCGETD
	struct uwdata w;
#endif
#endif
	static char sbuf[1024];

	char *getenv();

	/*
	 * Find out what kind of terminal this is.
	 */
 	if ((term = getenv("TERM")) == NULL)
 		term = "unknown";
 	if (tgetent(termbuf, term) <= 0)
 		strcpy(termbuf, "dumb:co#80:hc:");

	/*
	 * Get size of the screen.
	 */
#ifdef TIOCGWINSZ
	if (ioctl(2, TIOCGWINSZ, &w) == 0 && w.ws_row > 0)
		sc_height = w.ws_row;
	else
#else
#ifdef WIOCGETD
	if (ioctl(2, WIOCGETD, &w) == 0 && w.uw_height > 0)
		sc_height = w.uw_height/w.uw_vs;
	else
#endif
#endif
 		sc_height = tgetnum("li");
 	hard = (sc_height < 0 || tgetflag("hc"));
	if (hard)
	{
		/* Oh no, this is a hardcopy terminal. */
		sc_height = 24;
	}

#ifdef TIOCGWINSZ
 	if (ioctl(2, TIOCGWINSZ, &w) == 0 && w.ws_col > 0)
		sc_width = w.ws_col;
	else
#ifdef WIOCGETD
	if (ioctl(2, WIOCGETD, &w) == 0 && w.uw_width > 0)
		sc_width = w.uw_width/w.uw_hs;
	else
#endif
#endif
 		sc_width = tgetnum("co");
 	if (sc_width < 0)
  		sc_width = 80;

	auto_wrap = tgetflag("am");
	ignaw = tgetflag("xn");

	/*
	 * Assumes termcap variable "sg" is the printing width of
	 * the standout sequence, the end standout sequence,
	 * the underline sequence, the end underline sequence,
	 * the boldface sequence, and the end boldface sequence.
	 */
	if ((so_width = tgetnum("sg")) < 0)
		so_width = 0;
	be_width = bo_width = ue_width = ul_width = se_width = so_width;

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

	sc_visual_bell = tgetstr("vb", &sp);
	if (hard || sc_visual_bell == NULL)
		sc_visual_bell = "";

	sc_home = tgetstr("ho", &sp);
	if (hard || sc_home == NULL || *sc_home == '\0')
	{
		if (*sc_move == '\0')
		{
			cannot("home cursor");
			/*
			 * This last resort for sc_home is supposed to
			 * be an up-arrow suggesting moving to the 
			 * top of the "virtual screen". (The one in
			 * your imagination as you try to use this on
			 * a hard copy terminal.)
			 */
			sc_home = "|\b^";		
		} else
		{
			/* 
			 * No "home" string,
			 * but we can use "move(0,0)".
			 */
			strcpy(sp, tgoto(sc_move, 0, 0));
			sc_home = sp;
			sp += strlen(sp) + 1;
		}
	}

	sc_lower_left = tgetstr("ll", &sp);
	if (hard || sc_lower_left == NULL || *sc_lower_left == '\0')
	{
		if (*sc_move == '\0')
		{
			cannot("move cursor to lower left of screen");
			sc_lower_left = "\r";
		} else
		{
			/*
			 * No "lower-left" string, 
			 * but we can use "move(0,last-line)".
			 */
			strcpy(sp, tgoto(sc_move, 0, sc_height-1));
			sc_lower_left = sp;
			sp += strlen(sp) + 1;
		}
	}

	/*
	 * To add a line at top of screen and scroll the display down,
	 * we use "al" (add line) or "sr" (scroll reverse).
	 */
	if ((sc_addline = tgetstr("al", &sp)) == NULL || 
		 *sc_addline == '\0')
		sc_addline = tgetstr("sr", &sp);

	if (hard || sc_addline == NULL || *sc_addline == '\0')
	{
		cannot("scroll backwards");
		sc_addline = "";
		/* Force repaint on any backward movement */
		back_scroll = 0;
	}

	if (tgetflag("bs"))
		sc_backspace = "\b";
	else
	{
		sc_backspace = tgetstr("bc", &sp);
		if (sc_backspace == NULL || *sc_backspace == '\0')
			sc_backspace = "\b";
	}
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
}

/*
 * Deinitialize terminal
 */
	public void
deinit()
{
	tputs(sc_deinit, sc_height, putchr);
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
