/*
 * Process command line options.
 * Each option is a single letter which controls a program variable.
 * The options have defaults which may be changed via
 * the command line option, or toggled via the "-" command.
 */

#include "less.h"

#define	toupper(c)	((c)-'a'+'A')

/*
 * Types of options.
 */
#define	BOOL		01	/* Boolean option: 0 or 1 */
#define	TRIPLE		02	/* Triple-valued option: 0, 1 or 2 */
#define	NUMBER		04	/* Numeric option */
#define	NO_TOGGLE	0100	/* Option cannot be toggled with "-" cmd */

/*
 * Variables controlled by command line options.
 */
public int p_nbufs, f_nbufs;	/* Number of buffers.  There are two values,
				   one used for input from a pipe and 
				   the other for input from a file. */
public int clean_data;		/* Can we assume the data is "clean"? 
				   (That is, free of nulls, etc) */
public int quiet;		/* Should we suppress the audible bell? */
public int top_search;		/* Should forward searches start at the top 
				   of the screen? (alternative is bottom) */
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

extern int nbufs;
extern char *first_cmd;
extern char *every_first_cmd;

#define	DEF_F_NBUFS	5	/* Default for f_nbufs */
#define	DEF_P_NBUFS	12	/* Default for p_nbufs */

static struct option
{
	char oletter;		/* The controlling letter (a-z) */
	char otype;		/* Type of the option */
	int odefault;		/* Default value */
	int *ovar;		/* Pointer to the associated variable */
	char *odesc[3];		/* Description of each value */
} option[] =
{
	{ 'c', BOOL, 0, &clean_data,
		{ "Don't assume data is clean",
		  "Assume data is clean",
		  NULL
		}
	},
#ifndef	NRTC
	{ 'd', BOOL|NO_TOGGLE, 0, &know_dumb,
		{ NULL, NULL, NULL}
	},
#else	NRTC
	{ 'd', BOOL|NO_TOGGLE, 1, &know_dumb,
		{ NULL, NULL, NULL}
	},
#endif	NRTC
	{ 'e', BOOL, 0, &quit_at_eof,
		{ "Don't quit at end-of-file",
		  "Quit at end-of-file",
		  NULL
		}
	},
	{ 'h', NUMBER, -1, &back_scroll,
		{ "Backwards scroll limit is %d lines",
		  NULL, NULL
		}
	},
	{ 'p', BOOL, 0, &top_scroll,
		{ "Repaint by scrolling from bottom of screen",
		  "Repaint by painting from top of screen",
		  NULL
		}
	},
	{ 'x', NUMBER, 8, &tabstop,
		{ "Tab stops every %d spaces", 
		  NULL, NULL 
		}
	},
	{ 's', BOOL, 0, &squeeze,
		{ "Don't squeeze multiple blank lines",
		  "Squeeze multiple blank lines",
		  NULL
		}
	},
	{ 't', BOOL, 1, &top_search,
		{ "Forward search starts from bottom of screen",
		  "Forward search starts from top of screen",
		  NULL
		}
	},
	{ 'w', BOOL, 1, &twiddle,
		{ "Display nothing for lines after end-of-file",
		  "Display ~ for lines after end-of-file",
		  NULL
		}
	},
	{ 'm', TRIPLE, 0, &pr_type,
		{ "Prompt with a colon",
		  "Prompt with a message",
		  "Prompt with a verbose message"
		}
	},
	{ 'q', TRIPLE, 0, &quiet,
		{ "Ring the bell for errors AND at eof/bof",
		  "Ring the bell for errors but not at eof/bof",
		  "Never ring the bell"
		}
	},
	{ 'u', TRIPLE, 0, &bs_mode,
		{ "Underlined text displayed in underline mode",
		  "All backspaces cause overstrike",
		  "Backspaces print as ^H"
		}
	},
	{ '\0' }
};

public char all_options[64];	/* List of all valid options */

/*
 * Initialize each option to its default value.
 */
	public void
init_option()
{
	register struct option *o;
	register char *p;

	/*
	 * First do special cases, not in option table.
	 */
	first_cmd = every_first_cmd = NULL;
	f_nbufs = DEF_F_NBUFS;		/* -bf */
	p_nbufs = DEF_P_NBUFS;		/* -bp */

	p = all_options;
	*p++ = 'b';

	for (o = option;  o->oletter != '\0';  o++)
	{
		/*
		 * Set each variable to its default.
		 * Also make a list of all options, in "all_options".
		 */
		*(o->ovar) = o->odefault;
		*p++ = o->oletter;
		if (o->otype & TRIPLE)
			*p++ = toupper(o->oletter);
	}
	*p = '\0';
}

/*
 * Toggle command line flags from within the program.
 * Used by the "-" command.
 */
	public void
toggle_option(c)
	int c;
{
	register struct option *o;
	char message[100];
	char buf[5];

	/*
	 * First check for special cases not handled by the option table.
	 */
	switch (c)
	{
	case 'b':
		sprintf(message, "%d buffers", nbufs);
		error(message);
		return;
	}


	for (o = option;  o->oletter != '\0';  o++)
	{
		if ((o->otype & BOOL) && (o->oletter == c) &&
			(o->otype & NO_TOGGLE) == 0)
		{
			/*
			 * Boolean option: 
			 * just toggle it.
			 */
			*(o->ovar) = ! *(o->ovar);
			error(o->odesc[*(o->ovar)]);
			return;
		} else if ((o->otype & TRIPLE) && (o->oletter == c) &&
			(o->otype & NO_TOGGLE) == 0)
		{
			/*
			 * Triple-valued option with lower case letter:
			 * make it 1 unless already 1, then make it 0.
			 */
			*(o->ovar) = (*(o->ovar) == 1) ? 0 : 1;
			error(o->odesc[*(o->ovar)]);
			return;
		} else if ((o->otype & TRIPLE) && (toupper(o->oletter) == c) &&
			(o->otype & NO_TOGGLE) == 0)
		{
			/*
			 * Triple-valued option with upper case letter:
			 * make it 2 unless already 2, then make it 0.
			 */
			*(o->ovar) = (*(o->ovar) == 2) ? 0 : 2;
			error(o->odesc[*(o->ovar)]);
			return;
		} else if ((o->otype & NUMBER) && (o->oletter == c) &&
			(o->otype & NO_TOGGLE) == 0)
		{
			sprintf(message, o->odesc[0], *(o->ovar));
			error(message);
			return;
		}
	}

	if (control_char(c))
		sprintf(buf, "^%c", carat_char(c));
	else
		sprintf(buf, "%c", c);
	sprintf(message, "\"-%s\": no such flag.  Use one of \"%s\"", 
		buf, all_options);
	error(message);
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

	if (s == NULL)
		return;

    next:
	if (*s == '\0')
		return;
	switch (c = *s++)
	{
	case '-':
	case ' ':
	case '\t':
		goto next;
	case '+':
		if (*s == '+')
			every_first_cmd = ++s;
		first_cmd = s;
		return;
	case 'b':
		switch (*s)
		{
		case 'f':
			s++;
			f_nbufs = getnum(&s, 'b');
			break;
		case 'p':
			s++;
			p_nbufs = getnum(&s, 'b');
			break;
		default:
			f_nbufs = p_nbufs = getnum(&s, 'b');
			break;
		}
		goto next;
	}

	for (o = option;  o->oletter != '\0';  o++)
	{
		if ((o->otype & BOOL) && (o->oletter == c))
		{
			*(o->ovar) = ! o->odefault;
			goto next;
		} else if ((o->otype & TRIPLE) && (o->oletter == c))
		{
			*(o->ovar) = (o->odefault == 1) ? 0 : 1;
			goto next;
		} else if ((o->otype & TRIPLE) && (toupper(o->oletter) == c))
		{
			*(o->ovar) = (o->odefault == 2) ? 0 : 2;
			goto next;
		} else if ((o->otype & NUMBER) && (o->oletter == c))
		{
			*(o->ovar) = getnum(&s, c);
			goto next;
		}
	}

	printf("\"-%c\": invalid flag\n", c);
	exit(1);
}

/*
 * Translate a string into a number.
 * Like atoi(), but takes a pointer to a char *, and updates
 * the char * to point after the translated number.
 */
	static int
getnum(sp, c)
	char **sp;
	int c;
{
	register char *s;
	register int n;

	s = *sp;
	if (*s < '0' || *s > '9')
	{
		printf("number is required after -%c\n", c);
		exit(1);
	}

	n = 0;
	while (*s >= '0' && *s <= '9')
		n = 10 * n + *s++ - '0';
	*sp = s;
	return (n);
}
