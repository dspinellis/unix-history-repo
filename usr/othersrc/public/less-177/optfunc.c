/*
 * Handling functions for command line options.
 *
 * Most options are handled by the generic code in option.c.
 * But all string options, and a few non-string options, require
 * special handling specific to the particular option.
 * This special processing is done by the "handling functions" in this file.
 *
 * Each handling function is passed a "type" and, if it is a string
 * option, the string which should be "assigned" to the option.
 * The type may be one of:
 *	INIT	The option is being initialized from the command line.
 *	TOGGLE	The option is being changed from within the program.
 *	QUERY	The setting of the option is merely being queried.
 */

#include "less.h"
#include "option.h"

extern int nbufs;
extern int ispipe;
extern int cbufs;
extern int pr_type;
extern int nohelp;
extern int plusoption;
extern char *prproto[];
extern char *eqproto;
extern IFILE curr_ifile;
#if LOGFILE
extern char *namelogfile;
extern int force_logfile;
extern int logfile;
extern char *glob();
#endif
#if TAGS
public int tagoption = 0;
extern char *tagfile;
extern char *tagpattern;
extern char *tags;
#endif
#if __MSDOS__
public char *window_box = NULL;
extern int  directvideo;
extern int  output_mode;
#endif


#if LOGFILE
/*
 * Handler for -o option.
 */
	public void
opt_o(type, s)
	int type;
	char *s;
{
	PARG parg;

	switch (type)
	{
	case INIT:
		namelogfile = s;
		break;
	case TOGGLE:
		if (!ispipe)
		{
			error("Input is not a pipe", NULL_PARG);
			return;
		}
		if (logfile >= 0)
		{
			error("Log file is already in use", NULL_PARG);
			return;
		}
		s = skipsp(s);
		namelogfile = glob(s);
		if (namelogfile == NULL)
			namelogfile = save(s);
		use_logfile(s);
		sync_logfile();
		break;
	case QUERY:
		if (logfile < 0)
			error("No log file", NULL_PARG);
		else
		{
			parg.p_string = namelogfile;
			error("Log file \"%s\"", &parg);
		}
		break;
	}
}

/*
 * Handler for -O option.
 */
	public void
opt__O(type, s)
	int type;
	char *s;
{
	force_logfile = 1;
	opt_o(type, s);
}

/*
 * Handlers for obsolete -l and -L options.
 */
	public void
opt_l(type, s)
	int type;
	char *s;
{
	error("The -l option is obsolete.  Use -o", NULL_PARG);
}

	public void
opt__L(type, s)
	int type;
	char *s;
{
	error("The -L option is obsolete.  Use -O", NULL_PARG);
}
#endif

#if USERFILE
	public void
opt_k(type, s)
	int type;
	char *s;
{
	PARG parg;

	switch (type)
	{
	case INIT:
		if (add_cmdtable(s))
		{
			parg.p_string = s;
			error("Cannot use lesskey file \"%s\"", &parg);
		}
		break;
	case QUERY:
	case TOGGLE:
		error("Cannot query the -k flag", NULL_PARG);
		break;
	}
}
#endif

#if TAGS
/*
 * Handler for -t option.
 */
	public void
opt_t(type, s)
	int type;
	char *s;
{
	char *curr_filename;

	switch (type)
	{
	case INIT:
		tagoption = 1;
		findtag(s);
		break;
	case TOGGLE:
		findtag(skipsp(s));
		if (tagfile != NULL)
		{
			curr_filename = get_filename(curr_ifile);
			if (edit(tagfile, 0) == 0)
				if (tagsearch())
					(void) edit(curr_filename, 0);
		}
		break;
	case QUERY:
		error("Tag is required after -t", NULL_PARG);
		break;
	}
}

/*
 * Handler for -T option.
 */
	public void
opt__T(type, s)
	int type;
	char *s;
{
	PARG parg;

	switch (type)
	{
	case INIT:
		tags = s;
		break;
	case TOGGLE:
		s = skipsp(s);
		tags = glob(s);
		if (tags == NULL)
			tags = save(s);
		break;
	case QUERY:
		parg.p_string = tags;
		error("Tags file \"%s\"", &parg);
		break;
	}
}
#endif

/*
 * Handler for -p option.
 */
	public void
opt_p(type, s)
	int type;
	register char *s;
{
	switch (type)
	{
	case INIT:
		/*
		 * Unget a search command for the specified string.
		 * {{ This won't work if the "/" command is
		 *    changed or invalidated by a .lesskey file. }}
		 */
		plusoption = 1;
		ungetsc(s);
		ungetsc("/");
		break;
	case QUERY:
		error("Pattern is required after -p", NULL_PARG);
		break;
	}
}

/*
 * Handler for -P option.
 */
	public void
opt__P(type, s)
	int type;
	register char *s;
{
	register char **proto;
	PARG parg;

	switch (type)
	{
	case INIT:
	case TOGGLE:
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
		break;
	case QUERY:
		parg.p_string = prproto[pr_type];
		error("%s", &parg);
		break;
	}
}

/*
 * Handler for the -b option.
 */
	/*ARGSUSED*/
	public void
opt_b(type, s)
	int type;
	char *s;
{
	switch (type)
	{
	case TOGGLE:
	case QUERY:
		/*
		 * Allocate the new number of buffers.
		 */
		cbufs = ch_nbuf(cbufs);
		break;
	case INIT:
		break;
	}
}

#if __MSDOS__
/*
 * Handler for -v option. (use BIOS or direct video)
 */
	public void
opt_v(type, s)
	int type;
	register char *s;
{
	switch (type)
	{
	case INIT:
	case TOGGLE:
		if (output_mode == 2)
			directvideo = 1;
		else
			directvideo = 0;
		break;
	case QUERY:
		break;
	}
}

/*
 * Handler for -W option. (set/modify window boundaries)
 */
	public void
opt_W(type, s)
	int type;
	register char *s;
{
	PARG parg;

	switch (type)
	{
	case INIT:
		window_box = save(s);
		break;		/* get_term will take care of actually setting window */
#ifdef MOVE_WINDOW
	case TOGGLE:
		if (window_box != NULL)
			free(window_box);
		window_box = save(s);
		reset_window();
		break;
#endif
	case QUERY:
		parg.p_string = window_box;
		error("%s", &parg);
		break;
	}
}
#endif

/*
 * "-?" means display a help message.
 * If from the command line, exit immediately.
 */
	/*ARGSUSED*/
	public void
opt_query(type, s)
	int type;
	char *s;
{
	if (nohelp)
		return;
	switch (type)
	{
	case QUERY:
	case TOGGLE:
		error("Use \"h\" for help", NULL_PARG);
		break;
	case INIT:
		raw_mode(1);
		init();
		help();
		quit(0);
		/*NOTREACHED*/
	}
}
