/* cmd1.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains some of the EX commands - mostly ones that deal with
 * files, options, etc. -- anything except text.
 */

#include "config.h"
#include <ctype.h>
#include "vi.h"
#include "regexp.h"

#if MSDOS
#define	DATE __DATE__
#endif

#ifdef DEBUG
/* print the selected lines with info on the blocks */
/*ARGSUSED*/
void cmd_debug(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	REG char	*scan;
	REG long	l;
	REG int		i;
	int		len;

	/* scan lnum[] to determine which block its in */
	l = markline(frommark);
	for (i = 1; l > lnum[i]; i++)
	{
	}

	do
	{
		/* fetch text of the block containing that line */
		scan = blkget(i)->c;

		/* calculate its length */
		if (scan[BLKSIZE - 1])
		{
			len = BLKSIZE;
		}
		else
		{
			len = strlen(scan);
		}

		/* print block stats */
		msg("##### hdr[%d]=%d, lnum[%d-1]=%ld, lnum[%d]=%ld (%ld lines)",
			i, hdr.n[i], i, lnum[i-1], i, lnum[i], lnum[i] - lnum[i - 1]);
		msg("##### len=%d, buf=0x%lx, %sdirty",
			len, scan, ((int *)scan)[MAXBLKS + 1] ? "" : "not ");
		if (bang)
		{
			while (--len >= 0)
			{
				addch(*scan);
				scan++;
			}
		}
		exrefresh();

		/* next block */
		i++;
	} while (i < MAXBLKS && lnum[i] && lnum[i - 1] < markline(tomark));
}


/* This function checks a lot of conditions to make sure they aren't screwy */
/*ARGSUSED*/
void cmd_validate(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	char	*scan;
	int	i;
	int	nlcnt;	/* used to count newlines */
	int	len;	/* counts non-NUL characters */

	/* check lnum[0] */
	if (lnum[0] != 0L)
	{
		msg("lnum[0] = %ld", lnum[0]);
	}

	/* check each block */
	for (i = 1; lnum[i] <= nlines; i++)
	{
		scan = blkget(i)->c;
		if (scan[BLKSIZE - 1])
		{
			msg("block %d has no NUL at the end", i);
		}
		else
		{
			for (nlcnt = len = 0; *scan; scan++, len++)
			{
				if (*scan == '\n')
				{
					nlcnt++;
				}
			}
			if (scan[-1] != '\n')
			{
				msg("block %d doesn't end with '\\n' (length %d)", i, len);
			}
			if (bang || nlcnt != lnum[i] - lnum[i - 1])
			{
				msg("block %d (line %ld?) has %d lines, but should have %ld",
					i, lnum[i - 1] + 1L, nlcnt, lnum[i] - lnum[i - 1]);
			}
		}
		exrefresh();
	}

	/* check lnum again */
	if (lnum[i] != INFINITY)
	{
		msg("hdr.n[%d] = %d, but lnum[%d] = %ld",
			i, hdr.n[i], i, lnum[i]);
	}

	msg("# = \"%s\", %% = \"%s\"", prevorig, origname);
}
#endif /* DEBUG */


/*ARGSUSED*/
void cmd_mark(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	/* validate the name of the mark */
	if (!extra || *extra < 'a' || *extra > 'z' || extra[1])
	{
		msg("Invalid mark name");
		return;
	}

	mark[*extra - 'a'] = tomark;
}

/*ARGSUSED*/
void cmd_write(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	int		fd;
	int		append;	/* boolean: write in "append" mode? */
	REG long	l;
	REG char	*scan;
	REG int		i;

	/* if all lines are to be written, use tmpsave() */
	if (frommark == MARK_FIRST && tomark == MARK_LAST)
	{
		tmpsave(extra, bang);
		return;
	}

	/* see if we're going to do this in append mode or not */
	append = FALSE;
	if (extra[0] == '>' && extra[1] == '>')
	{
		extra += 2;
		append = TRUE;
	}

	/* either the file must not exist, or we must have a ! or be appending */
	if (access(extra, 0) == 0 && !bang && !append)
	{
		msg("File already exists - Use :w! to overwrite");
		return;
	}

	/* else do it line-by-line, like cmd_print() */
	if (append)
	{
#ifdef O_APPEND
		fd = open(extra, O_WRONLY|O_APPEND);
#else
		fd = open(extra, O_WRONLY);
		if (fd >= 0)
		{
			lseek(fd, 0L, 2);
		}
#endif
	}
	else
	{
		fd = -1; /* so we know the file isn't open yet */
	}

	if (fd < 0)
	{
		fd = creat(extra, FILEPERMS);
		if (fd < 0)
		{
			msg("Can't write to \"%s\"", extra);
			return;
		}
	}
	for (l = markline(frommark); l <= markline(tomark); l++)
	{
		/* get the next line */
		scan = fetchline(l);
		i = strlen(scan);
		scan[i++] = '\n';

		/* print the line */
		twrite(fd, scan, i);
	}
	close(fd);
}	


/*ARGSUSED*/
void cmd_shell(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	static char	prevextra[80];

	/* special case: ":sh" means ":!sh" */
	if (cmd == CMD_SHELL)
	{
		extra = o_shell;
		frommark = tomark = 0L;
	}

	/* if extra is "!", substute previous command */
	if (*extra == '!')
	{
		if (!*prevextra)
		{
			msg("No previous shell command to substitute for '!'");
			return;
		}
		extra = prevextra;
	}
	else if (cmd == CMD_BANG && strlen(extra) < sizeof(prevextra) - 1)
	{
		strcpy(prevextra, extra);
	}

	/* if no lines were specified, just run the command */
	suspend_curses();
	if (frommark == 0L)
	{
		system(extra);
	}
	else /* pipe lines from the file through the command */
	{
		filter(frommark, tomark, extra);
	}

	/* resume curses quietly for MODE_EX, but noisily otherwise */
	resume_curses(mode == MODE_EX);
}


/*ARGSUSED*/
void cmd_global(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;	/* rest of the command line */
{
	char	*cmdptr;	/* the command from the command line */
	char	cmdln[100];	/* copy of the command from the command line */
	char	*line;		/* a line from the file */
	long	l;		/* used as a counter to move through lines */
	long	lqty;		/* quantity of lines to be scanned */
	long	nchanged;	/* number of lines changed */
	regexp	*re;		/* the compiled search expression */

	/* can't nest global commands */
	if (doingglobal)
	{
		msg("Can't nest global commands.");
		rptlines = -1L;
		return;
	}

	/* ":g! ..." is the same as ":v ..." */
	if (bang)
	{
		cmd = CMD_VGLOBAL;
	}

	/* make sure we got a search pattern */
	if (*extra != '/' && *extra != '?')
	{
		msg("Usage: %c /regular expression/ command", cmd == CMD_GLOBAL ? 'g' : 'v');
		return;
	}

	/* parse & compile the search pattern */
	cmdptr = parseptrn(extra);
	if (!extra[1])
	{
		msg("Can't use empty regular expression with '%c' command", cmd == CMD_GLOBAL ? 'g' : 'v');
		return;
	}
	re = regcomp(extra + 1);
	if (!re)
	{
		/* regcomp found & described an error */
		return;
	}

	/* for each line in the range */
	doingglobal = TRUE;
	ChangeText
	{
		/* NOTE: we have to go through the lines in a forward order,
		 * otherwise "g/re/p" would look funny.  *BUT* for "g/re/d"
		 * to work, simply adding 1 to the line# on each loop won't
		 * work.  The solution: count lines relative to the end of
		 * the file.  Think about it.
		 */
		for (l = nlines - markline(frommark),
			lqty = markline(tomark) - markline(frommark) + 1L,
			nchanged = 0L;
		     lqty > 0 && nlines - l >= 0 && nchanged >= 0L;
		     l--, lqty--)
		{
			/* fetch the line */
			line = fetchline(nlines - l);

			/* if it contains the search pattern... */
			if ((!regexec(re, line, 1)) == (cmd != CMD_GLOBAL))
			{
				/* move the cursor to that line */
				cursor = MARK_AT_LINE(nlines - l);

				/* do the ex command (without mucking up
				 * the original copy of the command line)
				 */
				strcpy(cmdln, cmdptr);
				rptlines = 0L;
				doexcmd(cmdln);
				nchanged += rptlines;
			}
		}
	}
	doingglobal = FALSE;

	/* free the regexp */
	free(re);

	/* Reporting...*/
	rptlines = nchanged;
}


/*ARGSUSED*/
void cmd_file(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
#ifndef CRUNCH
	/* if we're given a new filename, use it as this file's name */
	if (extra && *extra)
	{
		strcpy(origname, extra);
	}
#endif
	if (cmd == CMD_FILE)
	{
		msg("\"%s\" %s%s %ld lines,  line %ld [%ld%%]",
			*origname ? origname : "[NO FILE]",
			tstflag(file, MODIFIED) ? "[MODIFIED]" : "",
			tstflag(file, READONLY) ? "[READONLY]" : "",
			nlines,
			markline(frommark),
			markline(frommark) * 100 / nlines);
	}
	else if (markline(frommark) == markline(tomark))
	{
		msg("%ld", markline(frommark));
	}
	else
	{
		msg("range \"%ld,%ld\" contains %ld lines",
			markline(frommark),
			markline(tomark),
			markline(tomark) - markline(frommark) + 1L);
	}
}


/*ARGSUSED*/
void cmd_edit(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	long	line = 1L;	/* might be set to prevline */

	/* Editing previous file?  Then start at previous line */
	if (!strcmp(extra, prevorig))
	{
		line = prevline;
	}

#ifndef CRUNCH
	/* if we were given an explicit starting line, then start there */
	if (*extra == '+')
	{
		for (extra++, line = 0L; *extra >= '0' && *extra <= '9'; extra++)
		{
			line *= 10L;
			line += (*extra - '0');
		}
		while (isascii(*extra) && isspace(*extra))
		{
			extra++;
		}
	}
#endif /* not CRUNCH */

	/* switch files */
	if (tmpabort(bang))
	{
		tmpstart(extra);
		if (line <= nlines && line >= 1L)
		{
			cursor = MARK_AT_LINE(line);
		}
	}
	else
	{
		msg("Use edit! to abort changes, or w to save changes");

		/* so we can say ":e!#" next time... */
		strcpy(prevorig, extra);
		prevline = 1L;
	}
}

/* This code is also used for rewind -- GB */

/*ARGSUSED*/
void cmd_next(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	int	i, j;
	char	*scan;
	char	*build;

	/* if extra stuff given, use ":args" to define a new args list */
	if (cmd == CMD_NEXT && extra && *extra)
	{
		cmd_args(frommark, tomark, cmd, bang, extra);
	}

	/* move to the next arg */
	if (cmd == CMD_NEXT)
	{
		i = argno + 1;
	}
	else if (cmd == CMD_PREVIOUS)
	{
		i = argno - 1;
	}
	else /* cmd == CMD_REWIND */
	{
		i = 0;
	}	
	if (i < 0 || i >= nargs)
	{
		msg("No %sfiles to edit", cmd == CMD_REWIND ? "" : "more ");
		return;
	}

	/* find & isolate the name of the file to edit */
	for (j = i, scan = args; j > 0; j--)
	{
		while(!isascii(*scan) || !isspace(*scan))
		{
			scan++;
		}
		while (isascii(*scan) && isspace(*scan))
		{
			scan++;
		}
	}
	for (build = tmpblk.c; *scan && (!isascii(*scan) || !isspace(*scan)); )
	{
		*build++ = *scan++;
	}
	*build = '\0';

	/* switch to the next file */
	if (tmpabort(bang))
	{
		tmpstart(tmpblk.c);
		argno = i;
	}
	else
	{
		msg("Use :%s! to abort changes, or w to save changes",
			cmd == CMD_NEXT ? "next" :
			cmd == CMD_PREVIOUS ? "previous" :
					"rewind");
	}
}

/* also called from :wq -- always writes back in this case */

/*ARGSUSED*/
void cmd_xit(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	static long	whenwarned;	/* when the user was last warned of extra files */
	int		oldflag;

	/* if there are more files to edit, then warn user */
	if (argno + 1 < nargs && whenwarned != changes && (!bang || cmd != CMD_QUIT))
	{
		msg("More files to edit -- Use \":n\" to go to next file");
		whenwarned = changes;
		return;
	}

	if (cmd == CMD_QUIT)
	{
		if (tmpabort(bang))
		{
			mode = MODE_QUIT;
		}
		else
		{
			msg("Use q! to abort changes, or wq to save changes");
		}
	}
	else
	{
		/* else try to save this file */
		oldflag = tstflag(file, MODIFIED);
		if (cmd == CMD_WQUIT)
			setflag(file, MODIFIED);
		if (tmpend(bang))
		{
			mode = MODE_QUIT;
		}
		else
		{
			msg("Could not save file -- use quit! to abort changes, or w filename");
		}
		if (!oldflag)
			clrflag(file, MODIFIED);
	}
}


/*ARGSUSED*/
void cmd_args(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	char	*scan;
	char	*eow;
	int	col;
	int	arg;
	int	addcols;
	int	scrolled = 0;

	/* if no extra names given, or just current name, then report the args
	 * we have now.
	 */
	if (!extra || !*extra)
	{
		for (scan = args, col=arg=0; *scan; )
		{
			while (*scan && isascii(*scan) && isspace(*scan))
				scan++;
			eow = scan;
			while (*eow && (!isascii(*++eow) || !isspace(*eow)))
				;
			if (arg == argno)
				addcols = 2;
			else
				addcols = 0;	
			if (col+addcols+(int)(eow-scan)+1>=COLS)
			{
				addch('\n');
				scrolled=1;
				col=0;
			}
			else if (arg)
			{	qaddch(' ');
				col++;
			}
			if (arg == argno)
				qaddch('[');
			while (scan < eow)
			{	qaddch(*scan++);
				col++;
			}
			if (arg == argno)
				qaddch(']');	
			arg++;	
			col+=addcols;
		}
		/* write a trailing newline */
		if ((mode == MODE_EX || mode == MODE_COLON || scrolled) && col)
			addch('\n');
		exrefresh();	
	}
	else /* new args list given */
	{
		strcpy(args, extra);
		argno = -1; /* before the first, so :next will go to first */

		/* count the names */
		for (nargs = 0, scan = args; *scan; nargs++)
		{
			while (*scan && (!isascii(*scan) || !isspace(*scan)))
			{
				scan++;
			}
			while (isascii(*scan) && isspace(*scan))
			{
				scan++;
			}
		}
		msg("%d files to edit", nargs);
	}
}


/*ARGSUSED*/
void cmd_cd(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	char	*getenv();

	/* default directory name is $HOME */
	if (!*extra)
	{
		extra = getenv("HOME");
		if (!extra)
		{
			msg("environment variable $HOME not set");
			return;
		}
	}

	/* go to the directory */
	if (chdir(extra) < 0)
	{
		perror(extra);
	}
}


/*ARGSUSED*/
void cmd_map(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	char	*mapto;

	/* "map" with no extra will dump the map table contents */
	if (!*extra)
	{
		dumpkey(bang ? WHEN_VIINP|WHEN_VIREP : WHEN_VICMD);
	}
	else
	{
		/* "extra" is key to map, followed my what it maps to */
		for (mapto = extra; *mapto && *mapto != ' ' && *mapto!= '\t'; mapto++)
		{
		}
		while (*mapto == ' ' || *mapto == '\t')
		{
			*mapto++ = '\0';
		}

		mapkey(extra, mapto, bang ? WHEN_VIINP|WHEN_VIREP : WHEN_VICMD, (char *)0);
	}
}


/*ARGSUSED*/
void cmd_set(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	if (!*extra)
	{
		dumpopts(FALSE);/* "FALSE" means "don't dump all" - only set */
	}
	else if (!strcmp(extra, "all"))
	{
		dumpopts(TRUE);	/* "TRUE" means "dump all" - even unset vars */
	}
	else
	{
		setopts(extra);

		/* That option may have affected the appearence of text */
		changes++;
	}
}

/*ARGSUSED*/
void cmd_tag(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	char	*scan;	/* used to scan through the tmpblk.c */
	char	*cmp;	/* char of tag name we're comparing, or NULL */
	char	*end;	/* marks the end of chars in tmpblk.c */
	int	fd;	/* file descriptor used to read the file */
#ifndef NO_MAGIC
	char	wasmagic; /* preserves the original state of o_magic */
#endif
	static char prevtag[30];

	/* if no tag is given, use the previous tag */
	if (!extra || !*extra)
	{
		if (!*prevtag)
		{
			msg("No previous tag");
			return;
		}
		extra = prevtag;
	}
	else
	{
		strncpy(prevtag, extra, sizeof prevtag);
	}

	/* open the tags file */
	fd = open(TAGS, O_RDONLY);
	if (fd < 0)
	{
		msg("No tags file");
		return;
	}

	/* Hmmm... this would have been a lot easier with <stdio.h> */

	/* find the line with our tag in it */
	for(scan = end = tmpblk.c, cmp = extra; ; scan++)
	{
		/* read a block, if necessary */
		if (scan >= end)
		{
			end = tmpblk.c + tread(fd, tmpblk.c, BLKSIZE);
			scan = tmpblk.c;
			if (scan >= end)
			{
				msg("tag \"%s\" not found", extra);
				close(fd);
				return;
			}
		}

		/* if we're comparing, compare... */
		if (cmp)
		{
			/* matched??? wow! */
			if (!*cmp && *scan == '\t')
			{
				break;
			}
			if (*cmp++ != *scan)
			{
				/* failed! skip to newline */
				cmp = (char *)0;
			}
		}

		/* if we're skipping to newline, do it fast! */
		if (!cmp)
		{
			while (scan < end && *scan != '\n')
			{
				scan++;
			}
			if (scan < end)
			{
				cmp = extra;
			}
		}
	}

	/* found it! get the rest of the line into memory */
	for (cmp = tmpblk.c, scan++; scan < end && *scan != '\n'; )
	{
		*cmp++ = *scan++;
	}
	if (scan == end)
	{
		tread(fd, cmp, BLKSIZE - (cmp - tmpblk.c));
	}

	/* we can close the tags file now */
	close(fd);

	/* extract the filename from the line, and edit the file */
	for (cmp = tmpblk.c; *cmp != '\t'; cmp++)
	{
	}
	*cmp++ = '\0';
	if (strcmp(origname, tmpblk.c) != 0)
	{
		if (!tmpabort(bang))
		{
			msg("Use :tag! to abort changes, or :w to save changes");
			return;
		}
		tmpstart(tmpblk.c);
	}

	/* move to the desired line (or to line 1 if that fails) */
#ifndef NO_MAGIC
	wasmagic = *o_magic;
	*o_magic = FALSE;
#endif
	cursor = MARK_FIRST;
	linespec(cmp, &cursor);
	if (cursor == MARK_UNSET)
	{
		cursor = MARK_FIRST;
	}
#ifndef NO_MAGIC
	*o_magic = wasmagic;
#endif
}



/*ARGSUSED*/
void cmd_visual(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	mode = MODE_VI;
	msg("");
}





/* describe this version of the program */
/*ARGSUSED*/
void cmd_version(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
#ifndef DATE
	msg("%s", VERSION);
#else
	msg("%s  (%s)", VERSION, DATE);
#endif
#ifdef COMPILED_BY
	msg("Compiled by %s", COMPILED_BY);
#endif
#ifdef CREDIT
	msg("%s", CREDIT);
#endif
#ifdef COPYING
	msg("%s", COPYING);
#endif
}


#ifndef NO_MKEXRC
/* make a .exrc file which describes the current configuration */
/*ARGSUSED*/
void cmd_mkexrc(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	int	fd;

	/* the default name for the .exrc file EXRC */
	if (!*extra)
	{
		extra = EXRC;
	}

	/* create the .exrc file */
	fd = creat(extra, FILEPERMS);
	if (fd < 0)
	{
		msg("Couldn't create a new \"%s\" file", extra);
		return;
	}

	/* save stuff */
	savekeys(fd);
	saveopts(fd);
#ifndef NO_DIGRAPH
	savedigs(fd);
#endif
#ifndef	NO_ABBR
	saveabbr(fd);
#endif

	/* close the file */
	close(fd);
	msg("Created a new \"%s\" file", extra);
}
#endif

#ifndef NO_DIGRAPH
/*ARGSUSED*/
void cmd_digraph(frommark, tomark, cmd, bang, extra)
	MARK	frommark;
	MARK	tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	do_digraph(bang, extra);
}
#endif


#ifndef NO_ERRLIST 
static char	errfile[256];	/* the name of a file containing an error */
static long	errline;	/* the line number for an error */

/* This static function tries to parse an error message.
 *
 * For most compilers, the first word is taken to be the name of the erroneous
 * file, and the first number after that is taken to be the line number where
 * the error was detected.  The description of the error follows, possibly
 * preceded by an "error ... :" or "warning ... :" label which is skipped.
 *
 * For Coherent, error messages look like "line#: filename: message".
 *
 * For non-error lines, or unparsable error lines, this function returns NULL.
 * Normally, though, it alters errfile and errline, and returns a pointer to
 * the description.
 */
static char *parse_errmsg(text)
	REG char	*text;
{
	REG char	*cpy;
	long		atol();
# if COHERENT || TOS /* any Mark Williams compiler */
	/* Get the line number.  If no line number, then ignore this line. */
	errline = atol(text);
	if (errline == 0L)
		return (char *)0;

	/* Skip to the start of the filename */
	while (*text && *text++ != ':')
	{
	}
	if (!*text++)
		return (char *)0;

	/* copy the filename to errfile */
	for (cpy = errfile; *text && (*cpy++ = *text++) != ':'; )
	{
	}
	if (!*text++)
		return (char *)0;
	cpy[-1] = '\0';

	return text;
# else /* not a Mark Williams compiler */
	char		*errmsg;

	/* the error message is the whole line, by default */
	errmsg = text;

	/* skip leading garbage */
	while (*text && !(isascii(*text) && isalnum(*text)))
	{
		text++;
	}

	/* copy over the filename */
	cpy = errfile;
	while(isascii(*text) && isalnum(*text) || *text == '.')
	{
		*cpy++ = *text++;
	}
	*cpy = '\0';

	/* ignore the name "Error" and filenames that contain a '/' */
	if (*text == '/' || !strcmp(errfile + 1, "rror") || access(errfile, 0) < 0)
	{
		return (char *)0;
	}

	/* skip garbage between filename and line number */
	while (*text && !(isascii(*text) && isdigit(*text)))
	{
		text++;
	}

	/* if the number is part of a larger word, then ignore this line */
	if (*text && isascii(text[-1]) && isalpha(text[-1]))
	{
		return (char *)0;
	}

	/* get the error line */
	errline = 0L;
	while (isascii(*text) && isdigit(*text))
	{
		errline *= 10;
		errline += (*text - '0');
		text++;
	}

	/* any line which lacks a filename or line number should be ignored */
	if (!errfile[0] || !errline)
	{
		return (char *)0;
	}

	/* locate the beginning of the error description */
	while (*text && isascii(*text) && !isspace(*text))
	{
		text++;
	}
	while (*text)
	{
#  ifndef CRUNCH
		/* skip "error #:" and "warning #:" clauses */
		if (!strncmp(text + 1, "rror ", 5)
		 || !strncmp(text + 1, "arning ", 7)
		 || !strncmp(text + 1, "atal error", 10))
		{
			do
			{
				text++;
			} while (*text && *text != ':');
			continue;
		}
#  endif

		/* anything other than whitespace or a colon is important */
		if (!isascii(*text) || (!isspace(*text) && *text != ':'))
		{
			errmsg = text;
			break;
		}

		/* else keep looking... */
		text++;
	}

	return errmsg;
# endif /* not COHERENT */
}

/*ARGSUSED*/
void cmd_errlist(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	static long	endline;/* original number of lines in this file */
	static long	offset;	/* offset of the next line in the errlist file */
	static int	fd = -2;/* fd of the errlist file */
	int		i;
	char		*errmsg;

	/* if a new errlist file is named, open it */
	if (extra && extra[0])
	{
		/* close the old one */
		if (fd >= 0)
		{
			close(fd);
		}

		fd = open(extra, O_RDONLY);
		offset = 0L;
	}
	else if (fd < 0)
	{
		fd = open(ERRLIST, O_RDONLY);
		offset = 0L;
	}

	/* do we have an errlist file now? */
	if (fd < 0)
	{
		msg("There is no errlist file");
		beep();
		return;
	}

	/* find the next error message in the file */
	do
	{
		/* read the next line from the errlist */
		lseek(fd, offset, 0);
		if (tread(fd, tmpblk.c, (unsigned)BLKSIZE) <= 0)
		{
			msg("No more errors");
			beep();
			close(fd);
			return;
		}
		for (i = 0; tmpblk.c[i] != '\n'; i++)
		{
		}
		tmpblk.c[i++] = 0;

		/* look for an error message in the line */
		errmsg = parse_errmsg(tmpblk.c);
		if (!errmsg)
		{
			offset += i;
		}

	} while (!errmsg);

	/* switch to the file containing the error, if this isn't it */
	if (strcmp(origname, errfile))
	{
		if (!tmpabort(bang))
		{
			msg("Use :er! to abort changes, or :w to save changes");
			beep();
			return;
		}
		tmpstart(errfile);
		endline = nlines;
	}
	else if (endline == 0L)
	{
		endline = nlines;
	}

	/* go to the line where the error was detected */
	cursor = MARK_AT_LINE(errline + (nlines - endline));
	if (cursor > MARK_LAST)
	{
		cursor = MARK_LAST;
	}
	if (mode == MODE_VI)
	{
		redraw(cursor, FALSE);
	}

	/* display the error message */
	if (nlines > endline)
	{
		msg("line %ld(+%ld): %.60s", errline, nlines - endline, errmsg);
	}
	else if (nlines < endline)
	{
		msg("line %ld(-%ld): %.60s", errline, endline - nlines, errmsg);
	}
	else
	{
		msg("line %ld: %.65s", errline, errmsg);
	}

	/* remember where the NEXT error line will start */
	offset += i;
}


/*ARGSUSED*/
void cmd_make(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	BLK	buf;

	/* if the file hasn't been saved, then complain unless ! */
	if (tstflag(file, MODIFIED) && !bang)
	{
		msg("\"%s\" not saved yet", origname);
		return;
	}

	/* build the command */
	sprintf(buf.c, "%s %s %s%s", (cmd == CMD_CC ? o_cc : o_make), extra, REDIRECT, ERRLIST);
	qaddstr(buf.c);
	addch('\n');

	/* run the command, with curses temporarily disabled */
	suspend_curses();
	system(buf.c);
	resume_curses(mode == MODE_EX);
	if (mode == MODE_COLON)
		mode = MODE_VI;

	/* run the "errlist" command */
	cmd_errlist(MARK_UNSET, MARK_UNSET, cmd, bang, ERRLIST);
}
#endif


#ifndef NO_ABBR
/*ARGSUSED*/
void cmd_abbr(frommark, tomark, cmd, bang, extra)
	MARK	frommark, tomark;
	CMD	cmd;
	int	bang;
	char	*extra;
{
	do_abbr(extra);
}
#endif
