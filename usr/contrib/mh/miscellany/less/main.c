/*
 * Entry point, initialization, miscellaneous routines.
 */

#include "less.h"
#include "position.h"
#include <setjmp.h>

public int	ispipe;
public jmp_buf	main_loop;
public char *	first_cmd;
public char *	every_first_cmd;
public int	new_file;
public int	is_tty;
public char 	current_file[128];
public int ac;
public char **av;
public int curr_ac;
#if EDITOR
public char *	editor;
#endif

extern int file;
extern int nbufs;
extern int sigs;
extern int quit_at_eof;
extern int p_nbufs, f_nbufs;
extern int back_scroll;
extern int top_scroll;
extern int sc_height;


/*
 * Edit a new file.
 * Filename "-" means standard input.
 * No filename means the "current" file, from the command line.
 */
	public void
edit(filename)
	char *filename;
{
	register int f;
	char message[100];
	static int any_edited = 0;
	static int hold_scroll = 0;

	if (filename == NULL || *filename == '\0')
	{
		if (curr_ac >= ac)
		{
			error("No current file");
			return;
		}
		filename = av[curr_ac];
	}
	if (strcmp(filename, "-") == 0)
		f = 0;	/* Standard input */
	else if ((f = open(filename, 0)) < 0)
	{
		sprintf(message, "Cannot open %.*s", 
			error_width()-13, filename);
		if (any_edited)
			error(message);
		else
		{
			puts(message);
			hold_scroll = 1;
		}
		return;
	}

	if (isatty(f))
	{
		/*
		 * Not really necessary to call this an error,
		 * but if the control terminal (for commands)
		 * and the input file (for data) are the same,
		 * we get weird results at best.
		 */
		error("Can't take input from a terminal");
		if (f > 0)
			close(f);
		return;
	}

	/*
	 * Close the current input file and set up to use the new one.
	 */
	if (file > 0)
		close(file);
	new_file = 1;
	strcpy(current_file, filename);
	ispipe = (f == 0);
	file = f;
	ch_init( (ispipe) ? p_nbufs : f_nbufs );
	init_mark();
	if (every_first_cmd != NULL)
		first_cmd = every_first_cmd;
	if (is_tty)
	{
		any_edited = 1;
		if (hold_scroll)
		{
			/*
			 * Before erasing the screen contents,
			 * display the file name and ask for a keystroke.
			 */
			error(filename);
			hold_scroll = 0;
		}
		if (first_cmd == NULL || *first_cmd == '\0')
		{
			/* 
			 * Display the first screen. 
			 */
			jump_back(1);
		} else
		{
			/* 
			 * The first_cmd will hopefully redisplay the
			 * screen, so we need not display anything yet.
			 * Indicate there is nothing yet on the screen. 
			 */
			pos_clear();
		}
	}
}

/*
 * Edit the next file in the command line list.
 */
	public void
next_file(n)
	int n;
{
	if (curr_ac + n >= ac)
	{
		if (quit_at_eof)
			quit();
		error("No (N-th) next file");
	} else
		edit(av[curr_ac += n]);
}

/*
 * Edit the previous file in the command line list.
 */
	public void
prev_file(n)
	int n;
{
	if (curr_ac - n < 0)
		error("No (N-th) previous file");
	else
		edit(av[curr_ac -= n]);
}

/*
 * Copy a file directly to standard output.
 * Used if standard output is not a tty.
 */
	static void
cat_file()
{
	register int c;

	while ((c = ch_forw_get()) != EOF)
		putc(c);
	flush();
}

/*
 * Entry point.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	char *getenv();


	/*
	 * Process command line arguments and LESS environment arguments.
	 * Command line arguments override environment arguments.
	 */
	init_option();
	scan_option(getenv("LESS"));
	argv++;
	while ( (--argc > 0) && 
		(argv[0][0] == '-' || argv[0][0] == '+') && 
		argv[0][1] != '\0')
		scan_option(*argv++);

#if EDITOR
	editor = getenv("EDITOR");
	if (editor == NULL || *editor == '\0')
		editor = EDIT_PGM;
#endif

	/*
	 * Set up list of files to be examined.
	 */
	ac = argc;
	av = argv;
	curr_ac = 0;

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
		if (ac < 1)
		{
			edit("-");
			cat_file();
		} else
		{
			do
			{
				edit((char *)NULL);
				if (file >= 0)
					cat_file();
			} while (++curr_ac < ac);
		}
		exit(0);
	}

	raw_mode(1);
	get_term();
	open_getc();
	init();

	if (back_scroll < 0)
	{
		/* {{ KLUDGE }} */
		back_scroll = sc_height-1;
		if (top_scroll)
			back_scroll--;
	}

	if (setjmp(main_loop))
		quit();
	init_signals();

	/*
	 * Select the first file to examine.
	 */
	if (ac < 1)
		edit("-");	/* Standard input */
	else 
	{
		/*
		 * Try all the files named as command arguments.
		 * We are simply looking for one which can be
		 * opened without error.
		 */
		do
		{
			edit((char *)NULL);
			if (file >= 0)
				/* We can open this file. */
				break;
			putc('\n');  flush();
		} while (++curr_ac < ac);
	}

	if (file >= 0)
		commands();
	quit();
}

/*
 * Exit the program.
 */
	public void
quit()
{
	/*
	 * Put cursor at bottom left corner, clear the line,
	 * reset the terminal modes, and exit.
	 */
	lower_left();
	clear_eol();
	deinit();
	flush();
	raw_mode(0);
	exit(0);
}
