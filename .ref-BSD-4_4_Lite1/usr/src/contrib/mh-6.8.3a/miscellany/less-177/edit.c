#include "less.h"

#if __MSDOS__
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#endif

#define	ISPIPE(fd)	((fd)==0)
extern int ispipe;
extern int new_file;
extern int errmsgs;
extern int quit_at_eof;
extern int file;
extern int cbufs;
extern char *every_first_cmd;
extern int any_display;
extern int force_open;
extern int is_tty;
extern IFILE curr_ifile;
extern IFILE old_ifile;
extern struct scrpos initial_scrpos;

#if LOGFILE
extern int logfile;
extern int force_logfile;
extern char *namelogfile;
#endif


/*
 * Edit a new file.
 * Filename == "-" means standard input.
 * Filename == NULL means just close the current file.
 */
	public int
edit(filename, just_looking)
	register char *filename;
	int just_looking;
{
	register int f;
	char *s;
	int answer;
	int no_display;
	struct scrpos scrpos;
	PARG parg;

	if (filename == NULL)
	{
		/*
		 * Close the current file, but don't open a new one.
		 */
		f = -1;
	} else if (strcmp(filename, "-") == 0)
	{
		/* 
		 * Use standard input.
		 */
		f = 0;
	} else if ((parg.p_string = bad_file(filename)) != NULL)
	{
		error("%s", &parg);
		free(parg.p_string);
		return (1);
#if __MSDOS__
	} else if ((f = open(filename, O_RDONLY|O_BINARY)) < 0)
#else
	} else if ((f = open(filename, 0)) < 0)
#endif
	{
		parg.p_string = errno_message(filename);
		error("%s", &parg);
		free(parg.p_string);
		return (1);
	} else if (!force_open && !just_looking && bin_file(f))
	{
		parg.p_string = filename;
		answer = query("\"%s\" may be a binary file.  Continue? ",
			&parg);
		if (answer != 'y' && answer != 'Y')
		{
			close(f);
			return (1);
		}
	}

	if (f >= 0 && isatty(f))
	{
		/*
		 * Not really necessary to call this an error,
		 * but if the control terminal (for commands)
		 * and the input file (for data) are the same,
		 * we get weird results at best.
		 */
#if __MSDOS__
		parg.p_string = "less -?";
#else
		parg.p_string = "less -\\?";
#endif
		error("Cannot take input from a terminal (\"%s\" for help)", 
			&parg);
		if (!ISPIPE(f))
			close(f);
		return (1);
	}

#if LOGFILE
	s = namelogfile;
	end_logfile();
	if (f >= 0 && ISPIPE(f) && s != NULL && is_tty)
		use_logfile(s);
#endif

	/*
	 * We are now committed to using the new file.
	 * Close the current input file and set up to use the new one.
	 */
	if (curr_ifile != NULL_IFILE)
	{
		/*
		 * Save the current position so that we can return to
		 * the same position if we edit this file again.
		 */
		get_scrpos(&scrpos);
		if (scrpos.pos != NULL_POSITION)
		{
			store_pos(curr_ifile, &scrpos);
			lastmark();
		}
	}

	/*
	 * Close the current file, unless it is a pipe.
	 */
	if (!ISPIPE(file))
		close(file);
	file = f;

	if (f < 0)
		return (1);

	/*
	 * Get the new ifile.
	 * Get the saved position for that file.
	 */
	old_ifile = curr_ifile;
	curr_ifile = get_ifile(filename, curr_ifile);
	get_pos(curr_ifile, &initial_scrpos);

	ispipe = ISPIPE(f);
	if (ispipe)
		ch_pipe();
	else
		ch_nonpipe();
	(void) ch_nbuf(cbufs);
	ch_flush();

	new_file = 1;

#if  __MSDOS__
	top_filename();
#endif

	if (every_first_cmd != NULL)
		ungetsc(every_first_cmd);

	no_display = !any_display;
	flush();
	any_display = 1;

	if (is_tty)
	{
		/*
		 * Output is to a real tty.
		 */

		/*
		 * Indicate there is nothing displayed yet.
		 */
		pos_clear();
		clr_linenum();
		if (no_display && errmsgs > 0)
		{
			/*
			 * We displayed some messages on error output
			 * (file descriptor 2; see error() function).
			 * Before erasing the screen contents,
			 * display the file name and wait for a keystroke.
			 */
			parg.p_string = filename;
			error("%s", &parg);
		}
	}
	return (0);
}

/*
 * Edit a space-separated list of files.
 * For each filename in the list, enter it into the ifile list.
 * Then edit the first one.
 */
	public void
edit_list(list)
	char *list;
{
	register char *s;
	register char *es;
	register char *filename;
	char *good_filename;
	IFILE save_curr_ifile;

	/*
	 * good_filename keeps track of the first valid filename.
	 */
	good_filename = NULL;
	s = list;
	es = s + strlen(s);
	save_curr_ifile = curr_ifile;
	while ((s = skipsp(s)) < es)
	{
		/*
		 * Get the next filename and null terminate it.
		 */
		filename = s;
		while (*s != ' ' && *s != '\0')
			s++;
		if (*s != '\0')
			*s++ = '\0';
		/*
		 * Try to edit the file.
		 * This enters it into the command line list (if it is good).
		 * If it is the first good file we've seen, remember it.
		 * {{ A little weirdness here: if any of the filenames
		 *    are already in the list, subsequent ones get
		 *    entered after the position where that one already
		 *    was, instead of at the end. }}
		 */
		if (edit(filename, 1) == 0 && good_filename == NULL)
			good_filename = filename;
	}

	/*
	 * Edit the first valid filename in the list.
	 */
	if (good_filename != NULL)
	{
		curr_ifile = save_curr_ifile;
		(void) edit(good_filename, 0);
	}
}

/*
 * Edit the first file in the command line (ifile) list.
 */
	public int
edit_first()
{
	curr_ifile = NULL_IFILE;
	return (edit_next(1));
}

/*
 * Edit the last file in the command line (ifile) list.
 */
	public int
edit_last()
{
	curr_ifile = NULL_IFILE;
	return (edit_prev(1));
}


/*
 * Edit the next file in the command line (ifile) list.
 */
	public int
edit_next(n)
	int n;
{
	IFILE h;

	h = curr_ifile;
	while (--n >= 0 || edit(get_filename(h), 0))
	{
		if ((h = next_ifile(h)) == NULL_IFILE)
			/*
			 * Reached end of the ifile list.
			 */
			return (1);
	} 
	/*
	 * Found a file that we can edit.
	 */
	return (0);
}

/*
 * Edit the previous file in the command line list.
 */
	public int
edit_prev(n)
	int n;
{
	IFILE h;

	h = curr_ifile;
	while (--n >= 0 || edit(get_filename(h), 0))
	{
		if ((h = prev_ifile(h)) == NULL_IFILE)
			/*
			 * Reached beginning of the ifile list.
			 */
			return (1);
	} 
	/*
	 * Found a file that we can edit.
	 */
	return (0);
}

/*
 * Edit a specific file in the command line (ifile) list.
 */
	public int
edit_index(n)
	int n;
{
	IFILE h;

	h = NULL_IFILE;
	do
	{
		if ((h = next_ifile(h)) == NULL_IFILE)
		{
			/*
			 * Reached end of the list without finding it.
			 */
			return (1);
		}
	} while (get_index(h) != n);

	return (edit(get_filename(h), 0));
}

/*
 * Copy a file directly to standard output.
 * Used if standard output is not a tty.
 */
	public void
cat_file()
{
	register int c;

	while ((c = ch_forw_get()) != EOI)
		putchr(c);
	flush();
}

#if LOGFILE

/*
 * If the user asked for a log file and our input file
 * is standard input, create the log file.  
 * We take care not to blindly overwrite an existing file.
 */
	public void
use_logfile(filename)
	char *filename;
{
	register int exists;
	register int answer;
	PARG parg;

	/*
	 * {{ We could use access() here. }}
	 */
	exists = open(filename, 0);
	close(exists);
	exists = (exists >= 0);

	/*
	 * Decide whether to overwrite the log file or append to it.
	 * (If it doesn't exist we "overwrite" it.
	 */
	if (!exists || force_logfile)
	{
		/*
		 * Overwrite (or create) the log file.
		 */
		answer = 'O';
	} else
	{
		/*
		 * Ask user what to do.
		 */
		parg.p_string = filename;
		answer = query("Warning: \"%s\" exists; Overwrite, Append or Don't log? ", &parg);
	}

loop:
	switch (answer)
	{
	case 'O': case 'o':
		/*
		 * Overwrite: create the file.
		 */
		logfile = creat(filename, 0644);
		break;
	case 'A': case 'a':
		/*
		 * Append: open the file and seek to the end.
		 */
#if __MSDOS__
		logfile = open(filename, O_APPEND|O_WRONLY);
#else
		logfile = open(filename, 1);
#endif
		if (lseek(logfile, (offset_t)0, 2) == BAD_LSEEK)
		{
			close(logfile);
			logfile = -1;
		}
		break;
	case 'D': case 'd':
		/*
		 * Don't do anything.
		 */
		return;
	case 'q':
		quit(0);
		/*NOTREACHED*/
	default:
		/*
		 * Eh?
		 */
		answer = query("Overwrite, Append, or Don't log? ", NULL_PARG);
		goto loop;
	}

	if (logfile < 0)
	{
		/*
		 * Error in opening logfile.
		 */
		parg.p_string = filename;
		error("Cannot write to \"%s\"", &parg);
	}
}

#endif
