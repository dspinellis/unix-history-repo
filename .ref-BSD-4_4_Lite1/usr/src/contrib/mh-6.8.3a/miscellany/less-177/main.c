/*
 * Entry point, initialization, miscellaneous routines.
 */

#include "less.h"
#include "position.h"

public int	ispipe;
public char *	every_first_cmd = NULL;
public int	new_file;
public int	is_tty;
public IFILE	curr_ifile = NULL_IFILE;
public IFILE	old_ifile = NULL_IFILE;
public struct scrpos initial_scrpos;
public int	any_display = 0;
public int	scroll;
public char *	progname;
public int	quitting;

extern int	file;
extern int	quit_at_eof;
extern int	cbufs;
extern int	errmsgs;
extern int	screen_trashed;
extern int	force_open;

#if LOGFILE
public int	logfile = -1;
public int	force_logfile = 0;
public char *	namelogfile = NULL;
#endif

#if EDITOR
public char *	editor;
public char *	editproto;
#endif

#if TAGS
extern char *	tagfile;
extern char *	tagpattern;
extern int	tagoption;
#endif



/*
 * Entry point.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	IFILE h;
	int nofiles;
	extern char *getenv();

	progname = *argv++;

	/*
	 * Process command line arguments and LESS environment arguments.
	 * Command line arguments override environment arguments.
	 */
	init_prompt();
	init_charset();
	init_option();
	scan_option(getenv("LESS"));

#define	isoptstring(s)	(((s)[0] == '-' || (s)[0] == '+') && (s)[1] != '\0')
	while (--argc > 0 && (isoptstring(argv[0]) || isoptpending()))
		scan_option(*argv++);
#undef isoptstring

	if (isoptpending())
	{
		/*
		 * Last command line option was a flag requiring a
		 * following string, but there was no following string.
		 */
		nopendopt();
		quit(0);
	}

#if USERFILE
	/*
	 * Try to use the lesskey file "$HOME/.less".
	 */
	add_hometable();
#endif
#if EDITOR
	editor = getenv("EDITOR");
	if (editor == NULL || *editor == '\0')
		editor = EDIT_PGM;
	editproto = getenv("LESSEDIT");
	if (editproto == NULL || *editproto == '\0')
		editproto = "%E ?lm+%lm. %f";
#endif

	/*
	 * Set up terminal, etc.
	 */
	is_tty = isatty(1);
	if (!is_tty)
	{
		/*
		 * Output is not a tty.
		 * Just copy the input file(s) to output.
		 */
		if (argc <= 0)
		{
			if (edit("-", 0) == 0)
				cat_file();
		} else
		{
			while (--argc >= 0)
			{
				if (edit(*argv++, 0) == 0)
					cat_file();
			}
		}
		quit(0);
	}

	/*
	 * Call get_ifile with all the command line filenames
	 * to "register" them with the ifile system.
	 */
	h = NULL_IFILE;
	while (--argc >= 0)
		h = get_ifile(*argv++, h);

	init_mark();
	raw_mode(1);
	get_term();
	open_getchr();

	init_signals(1);

	/*
	 * Select the first file to examine.
	 */
#if TAGS
	if (tagoption)
	{
		/*
		 * A -t option was given.
		 * Verify that no filenames were also given.
		 * Edit the file selected by the "tags" search,
		 * and search for the proper line in the file.
		 */
		if (nifile() > 0)
		{
			error("No filenames allowed with -t option", NULL_PARG);
			quit(1);
		}
		if (tagfile == NULL)
			quit(1);
		if (edit(tagfile, 0) || tagsearch())
			quit(1);
		nofiles = 0;
	} else
#endif
	if (nifile() == 0)
		nofiles = edit("-", 0);	/* Standard input */
	else 
		nofiles = edit_first();

	if (nofiles)
	{
		quit(1);
		/*NOTREACHED*/
	}

	init();
	commands();
	quit(0);
	/*NOTREACHED*/
}

/*
 * Copy a string, truncating to the specified length if necessary.
 * Unlike strncpy(), the resulting string is guaranteed to be null-terminated.
 */
	public void
strtcpy(to, from, len)
	char *to;
	char *from;
	unsigned int len;
{
	strncpy(to, from, len);
	to[len-1] = '\0';
}

/*
 * Copy a string to a "safe" place
 * (that is, to a buffer allocated by calloc).
 */
	public char *
save(s)
	char *s;
{
	register char *p;

	p = (char *) ecalloc(strlen(s)+1, sizeof(char));
	strcpy(p, s);
	return (p);
}

	public VOID_POINTER
ecalloc(count, size)
	int count;
	unsigned int size;
{
	register VOID_POINTER p;

	p = calloc(count, size);
	if (p != NULL)
		return (p);
	error("Cannot allocate memory", NULL_PARG);
	quit(1);
	/*NOTREACHED*/
}

/*
 * Skip leading spaces in a string.
 */
	public char *
skipsp(s)
	register char *s;
{
	while (*s == ' ' || *s == '\t')	
		s++;
	return (s);
}

/*
 * Exit the program.
 */
	public void
quit(status)
	int status;
{
	static int save_status;

	/*
	 * Put cursor at bottom left corner, clear the line,
	 * reset the terminal modes, and exit.
	 */
	if (status < 0)
		status = save_status;
	else
		save_status = status;
	quitting = 1;
#if LOGFILE
	end_logfile();
#endif
	if (any_display)
	{
		lower_left();
		clear_eol();
	}
	deinit();
	flush();
	raw_mode(0);
#if __MSDOS__
	restore_screen();
	/* 
	 * If we don't close 2, we get some garbage from
	 * 2's buffer when it flushes automatically.
	 * I cannot track this one down  RB
	 * The same bug shows up if we use ^C^C to abort.
	 */
	close(2);
#endif
	exit(status);
}
