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
static char sccsid[] = "@(#)option.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * Process command line options.
 * Each option is a single letter which controls a program variable.
 * The options have defaults which may be changed via
 * the command line option, or toggled via the "-" command.
 */

#include "less.h"

#define	toupper(c)	((c)-'a'+'A')

#define	END_OPTION_STRING	('$')

/*
 * Types of options.
 */
#define	BOOL		01	/* Boolean option: 0 or 1 */
#define	TRIPLE		02	/* Triple-valued option: 0, 1 or 2 */
#define	NUMBER		04	/* Numeric option */
#define	REPAINT		040	/* Repaint screen after toggling option */
#define	NO_TOGGLE	0100	/* Option cannot be toggled with "-" cmd */

/*
 * Variables controlled by command line options.
 */
public int clean_data;		/* Can we assume the data is "clean"? 
				   (That is, free of nulls, etc) */
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
public int twiddle;		/* Display "~" for lines after EOF */
public int caseless;		/* Do "caseless" searches */
public int linenums;		/* Use line numbers */
public int cbufs;		/* Current number of buffers */
public int autobuf;
public int plusoption;

extern char *prproto[];
extern char *eqproto;
extern int nbufs;
extern int sc_window;
extern int ispipe;
extern char *first_cmd;
extern char *every_first_cmd;
extern char *tagfile;
extern char *tagpattern;
public int tagoption = 0;

static char *opt_P();

static struct option
{
	char oletter;		/* The controlling letter (a-z) */
	char otype;		/* Type of the option */
	int odefault;		/* Default value */
	int *ovar;		/* Pointer to the associated variable */
	char *odesc[3];		/* Description of each value */
} option[] =
{
	{ 'a', TRIPLE, 0, &how_search,
		{ "Forward search starts at second REAL line displayed",
		  "Forward search starts at bottom of screen",
		  "Forward search starts at second SCREEN line displayed"
		}
	},
	{ 'b', NUMBER, 10, &cbufs,
		{ "%d buffers",
		  NULL, NULL
		}
	},
	{ 'B', BOOL, 1, &autobuf,
		{ "Don't automatically allocate buffers",
		  "Automatically allocate buffers when needed",
		  NULL
		}
	},
	{ 'c', TRIPLE, 0, &top_scroll,
		{ "Repaint by scrolling from bottom of screen",
		  "Repaint by clearing each line",
		  "Repaint by painting from top of screen"
		}
	},
	{ 'd', BOOL|NO_TOGGLE, 0, &know_dumb,
		{ NULL, NULL, NULL}
	},
	{ 'e', TRIPLE, 0, &quit_at_eof,
		{ "Don't quit at end-of-file",
		  "Quit at end-of-file",
		  "Quit immediately at end-of-file"
		}
	},
	{ 'h', NUMBER, -1, &back_scroll,
		{ "Backwards scroll limit is %d lines",
		  NULL, NULL
		}
	},
	{ 'i', BOOL, 0, &caseless,
		{ "Case is significant in searches",
		  "Ignore case in searches",
		  NULL
		}
	},
	{ 'm', TRIPLE, 0, &pr_type,
		{ "Short prompt",
		  "Medium prompt",
		  "Long prompt"
		}
	},
	{ 'n', BOOL, 1, &linenums,
		{ "Don't use line numbers",
		  "Use line numbers",
		  NULL
		}
	},
	{ 'q', TRIPLE, 0, &quiet,
		{ "Ring the bell for errors AND at eof/bof",
		  "Ring the bell for errors but not at eof/bof",
		  "Never ring the bell"
		}
	},
	{ 's', BOOL|REPAINT, 0, &squeeze,
		{ "Don't squeeze multiple blank lines",
		  "Squeeze multiple blank lines",
		  NULL
		}
	},
	{ 'u', TRIPLE|REPAINT, 0, &bs_mode,
		{ "Underlined text displayed in underline mode",
		  "Backspaces cause overstrike",
		  "Backspaces print as ^H"
		}
	},
	{ 'w', BOOL|REPAINT, 1, &twiddle,
		{ "Display nothing for lines after end-of-file",
		  "Display ~ for lines after end-of-file",
		  NULL
		}
	},
	{ 'x', NUMBER|REPAINT, 8, &tabstop,
		{ "Tab stops every %d spaces", 
		  NULL, NULL 
		}
	},
	{ 'z', NUMBER|REPAINT, -1, &sc_window,
		{ "Scroll window size is %d lines",
		  NULL, NULL
		}
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

	first_cmd = every_first_cmd = NULL;

	for (o = option;  o->oletter != '\0';  o++)
	{
		/*
		 * Set each variable to its default.
		 */
		*(o->ovar) = o->odefault;
	}
}

/*
 * Toggle command line flags from within the program.
 * Used by the "-" and "_" commands.
 * If do_toggle is zero, just report the current setting, without changing it.
 */
	public void
toggle_option(s, do_toggle)
	char *s;
	int do_toggle;
{
	int c;
	register struct option *o;
	char *msg;
	int n;
	int dorepaint;
	char message[100];

	c = *s++;

	switch (c)
	{
	case 'P':
		/*
		 * Special case for -P.
		 */
		if (*s == '\0')
			error(prproto[pr_type]);
		else
			(void) opt_P(s);
		return;
	case 't':
		/*
		 * Special case for -t.
		 */
		if (*s == '\0')
		{
			error("no tag");
			return;
		}
		findtag(s);
		if (tagfile != NULL)
		{
			edit(tagfile);
			(void) tagsearch();
		}
		return;
	}

	msg = NULL;
	for (o = option;  o->oletter != '\0';  o++)
	{
		if (o->otype & NO_TOGGLE)
			continue;
		dorepaint = (o->otype & REPAINT);
		if ((o->otype & BOOL) && (o->oletter == c))
		{
			/*
			 * Boolean option: 
			 * just toggle it.
			 */
			if (do_toggle)
				*(o->ovar) = ! *(o->ovar);
		} else if ((o->otype & TRIPLE) && (o->oletter == c))
		{
			/*
			 * Triple-valued option with lower case letter:
			 * make it 1 unless already 1, then make it 0.
			 */
			if (do_toggle)
				*(o->ovar) = (*(o->ovar) == 1) ? 0 : 1;
		} else if ((o->otype & TRIPLE) && (toupper(o->oletter) == c))
		{
			/*
			 * Triple-valued option with upper case letter:
			 * make it 2 unless already 2, then make it 0.
			 */
			if (do_toggle)
				*(o->ovar) = (*(o->ovar) == 2) ? 0 : 2;
		} else if ((o->otype & NUMBER) && (o->oletter == c))
		{
			n = getnum(&s, '\0');
			if (n < 0)
			{
				/*
				 * No number; just a query.
				 * No need to repaint screen.
				 */
				dorepaint = 0;
			} else
			{
				/*
				 * Number follows the option letter.
				 * Set the variable to that number.
				 */
				if (do_toggle)
					*(o->ovar) = n;
			}

			/*
			 * Special case for -b.
			 * Call ch_init to set new number of buffers.
			 */
			if (o->ovar == &cbufs)
				ch_init(cbufs, 1);

			sprintf(message, o->odesc[0], 
				(o->ovar == &back_scroll) ? 
				get_back_scroll() : *(o->ovar));
			msg = message;
		} else
			continue;

		/*
		 * Print a message describing the new setting.
		 */
		if (msg == NULL)
			msg = o->odesc[*(o->ovar)];
		error(msg);

		if (do_toggle && dorepaint)
			repaint();
		return;
	}

	if (control_char(c))
		sprintf(message, "-^%c", carat_char(c));
	else
		sprintf(message, "-%c", c);
	strcat(message, ": no such flag.");
	error(message);
}

/*
 * Determine if an option is a single character option (BOOL or TRIPLE),
 * or if it a multi-character option (NUMBER).
 */
	public int
single_char_option(c)
	int c;
{
	register struct option *o;

	if (c == 'P')
		return (0);
	if (c == 't')
		return (0);
	for (o = option;  o->oletter != '\0';  o++)
		if (o->oletter == c)
			return (o->otype & (BOOL|TRIPLE));
	return (1);
}

/*
 * Scan to end of string or to an END_OPTION_STRING character.
 * In the latter case, replace the char with a null char.
 * Return a pointer to the remainder of the string, if any.
 */
	static char *
optstring(s, c)
	char *s;
	int c;
{
	register char *p;
	char message[80];

	if (*s == '\0')
	{
		sprintf(message, "string is required after -%c", c);
		error(message);
		exit(1);
	}
	for (p = s;  *p != '\0';  p++)
		if (*p == END_OPTION_STRING)
		{
			*p = '\0';
			return (p+1);
		}
	return (p);
}

/* 
 * Scan an argument (either from command line or from LESS environment 
 * variable) and process it.
 */
	public void
scan_option(s)
	char *s;
{
	register struct option *o;
	register int c;
	int set_default;
	char message[80];

	if (s == NULL)
		return;

	set_default = 0;
    next:
	if (*s == '\0')
		return;
	switch (c = *s++)
	{
	case ' ':
	case '\t':
	case END_OPTION_STRING:
		goto next;
	case '-':
		if (set_default = (*s == '+'))
			s++;
		goto next;
	case '+':
		plusoption = 1;
		if (*s == '+')
			every_first_cmd = save(++s);
		first_cmd = s;
		s = optstring(s, c);
		goto next;
	case 't':
	{
		char *p;
		tagoption = 1;
		p = s;
		s = optstring(s, c);
		findtag(p);
		goto next;
	}
	case 'P':
		s = opt_P(s);
		goto next;
	case '0':  case '1':  case '2':  case '3':  case '4':
	case '5':  case '6':  case '7':  case '8':  case '9':
		/*
		 * Handle special "more" compatibility form "-number"
		 * (instead of -znumber) to set the scrolling window size.
		 */
		s--;
		c = 'z';
		break;
	}

	for (o = option;  o->oletter != '\0';  o++)
	{
		if ((o->otype & BOOL) && (o->oletter == c))
		{
			if (set_default)
				*(o->ovar) = o->odefault;
			else
				*(o->ovar) = ! o->odefault;
			goto next;
		} else if ((o->otype & TRIPLE) && (o->oletter == c))
		{
			if (set_default)
				*(o->ovar) = o->odefault;
			else
				*(o->ovar) = (o->odefault == 1) ? 0 : 1;
			goto next;
		} else if ((o->otype & TRIPLE) && (toupper(o->oletter) == c))
		{
			if (set_default)
				*(o->ovar) = o->odefault;
			else
				*(o->ovar) = (o->odefault == 2) ? 0 : 2;
			goto next;
		} else if ((o->otype & NUMBER) && (o->oletter == c))
		{
			*(o->ovar) = getnum(&s, c);
			goto next;
		}
	}

	sprintf(message, "\"-%c\": invalid flag", c);
	error(message);
	exit(1);
}

/*
 * Special case for -P.
 */
	static char *
opt_P(s)
	register char *s;
{
	register char *es;
	register char **proto;

	es = optstring(s, 'P');

	/*
	 * Figure out which prototype string should be changed.
	 */
	switch (*s)
	{
	case 'm':  proto = &prproto[PR_MEDIUM];	s++;	break;
	case 'M':  proto = &prproto[PR_LONG];	s++;	break;
	case '=':  proto = &eqproto;		s++;	break;
	default:   proto = &prproto[pr_type];		break;
	}

	free(*proto);
	*proto = save(s);

	return (es);
}

/*
 * Translate a string into a number.
 * Like atoi(), but takes a pointer to a char *, and updates
 * the char * to point after the translated number.
 */
	public int
getnum(sp, c)
	char **sp;
	int c;
{
	register char *s;
	register int n;
	char message[80];

	s = *sp;
	if (*s < '0' || *s > '9')
	{
		if (c == '\0')
			return (-1);
		sprintf(message, "number is required after -%c", c);
		error(message);
		exit(1);
	}

	n = 0;
	while (*s >= '0' && *s <= '9')
		n = 10 * n + *s++ - '0';
	*sp = s;
	return (n);
}
