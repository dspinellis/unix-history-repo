/*
 * Primitives for displaying the file on the screen.
 */

#include "less.h"
#include "position.h"

public int hit_eof;	/* Keeps track of how many times we hit end of file */

extern int quiet;
extern int top_search;
extern int top_scroll;
extern int back_scroll;
extern int sc_width, sc_height;
extern int sigs;
extern char *line;
extern char *first_cmd;

/*
 * Sound the bell to indicate he is trying to move past end of file.
 */
	static void
eof_bell()
{
	if (quiet == NOT_QUIET)
		bell();
	else
		vbell();
}

/*
 * Check to see if the end of file is currently "displayed".
 */
	static void
eof_check()
{
	POSITION pos;

	/*
	 * If the bottom line is empty, we are at EOF.
	 * If the bottom line ends at the file length,
	 * we must be just at EOF.
	 */
	pos = position(BOTTOM_PLUS_ONE);
	if (pos == NULL_POSITION || pos == ch_length())
		hit_eof++;
}

/*
 * Display n lines, scrolling forward, 
 * starting at position pos in the input file.
 * "force" means display the n lines even if we hit end of file.
 * "only_last" means display only the last screenful if n > screen size.
 */
	static void
forw(n, pos, force, only_last)
	register int n;
	POSITION pos;
	int force;
	int only_last;
{
	int eof = 0;
	int nlines = 0;
	int repaint_flag;

	/*
	 * repaint_flag tells us not to display anything till the end, 
	 * then just repaint the entire screen.
	 */
	repaint_flag = (only_last && n > sc_height-1);

	if (!repaint_flag)
	{
		if (top_scroll && n >= sc_height - 1)
		{
			/*
			 * Start a new screen.
			 * {{ This is not really desirable if we happen
			 *    to hit eof in the middle of this screen,
			 *    but we don't know if that will happen now. }}
			 */
			clear();
			home();
			force = 1;
		} else
		{
			lower_left();
			clear_eol();
		}

		if (pos != position(BOTTOM_PLUS_ONE))
		{
			/*
			 * This is not contiguous with what is
			 * currently displayed.  Clear the screen image 
			 * (position table) and start a new screen.
			 */
			pos_clear();
			add_forw_pos(pos);
			force = 1;
			if (top_scroll)
			{
				clear();
				home();
			} else
			{
				puts("...skipping...\n");
			}
		}
	}

	while (--n >= 0)
	{
		/*
		 * Read the next line of input.
		 */
		pos = forw_line(pos);
		if (pos == NULL_POSITION)
		{
			/*
			 * End of file: stop here unless the top line 
			 * is still empty, or "force" is true.
			 */
			eof = 1;
			if (!force && position(TOP) != NULL_POSITION)
				break;
			line = NULL;
		}
		/*
		 * Add the position of the next line to the position table.
		 * Display the current line on the screen.
		 */
		add_forw_pos(pos);
		nlines++;
		if (!repaint_flag)
			put_line();
	}

	if (eof)
		hit_eof++;
	else
		eof_check();
	if (nlines == 0)
		eof_bell();
	else if (repaint_flag)
		repaint();
}

/*
 * Display n lines, scrolling backward.
 */
	static void
back(n, pos, force, only_last)
	register int n;
	POSITION pos;
	int force;
	int only_last;
{
	int nlines = 0;
	int repaint_flag;

	repaint_flag = (n > back_scroll || (only_last && n > sc_height-1));
	hit_eof = 0;
	while (--n >= 0)
	{
		/*
		 * Get the previous line of input.
		 */
		pos = back_line(pos);
		if (pos == NULL_POSITION)
		{
			/*
			 * Beginning of file: stop here unless "force" is true.
			 */
			if (!force)
				break;
			line = NULL;
		}
		/*
		 * Add the position of the previous line to the position table.
		 * Display the line on the screen.
		 */
		add_back_pos(pos);
		nlines++;
		if (!repaint_flag)
		{
			home();
			add_line();
			put_line();
		}
	}

	eof_check();
	if (nlines == 0)
		eof_bell();
	else if (repaint_flag)
		repaint();
}

/*
 * Display n more lines, forward.
 * Start just after the line currently displayed at the bottom of the screen.
 */
	public void
forward(n, only_last)
	int n;
	int only_last;
{
	POSITION pos;

	pos = position(BOTTOM_PLUS_ONE);
	if (pos == NULL_POSITION)
	{
		eof_bell();
		hit_eof++;
		return;
	}
	forw(n, pos, 0, only_last);
}

/*
 * Display n more lines, backward.
 * Start just before the line currently displayed at the top of the screen.
 */
	public void
backward(n, only_last)
	int n;
	int only_last;
{
	POSITION pos;

	pos = position(TOP);
	if (pos == NULL_POSITION)
	{
		/* 
		 * This will almost never happen,
		 * because the top line is almost never empty. 
		 */
		eof_bell();
		return;   
	}
	back(n, pos, 0, only_last);
}

/*
 * Repaint the screen, starting from a specified position.
 */
	static void
prepaint(pos)	
	POSITION pos;
{
	hit_eof = 0;
	forw(sc_height-1, pos, 0, 0);
}

/*
 * Repaint the screen.
 */
	public void
repaint()
{
	/*
	 * Start at the line currently at the top of the screen
	 * and redisplay the screen.
	 */
	prepaint(position(TOP));
}

/*
 * Jump to the end of the file.
 * It is more convenient to paint the screen backward,
 * from the end of the file toward the beginning.
 */
	public void
jump_forw()
{
	POSITION pos;

	if (ch_end_seek())
	{
		error("Cannot seek to end of file");
		return;
	}
	pos = ch_tell();
	clear();
	pos_clear();
	add_back_pos(pos);
	back(sc_height - 1, pos, 0, 0);
}

/*
 * Jump to line n in the file.
 */
	public void
jump_back(n)
	register int n;
{
	register int c;

	/*
	 * This is done the slow way, by starting at the beginning
	 * of the file and counting newlines.
	 */
	if (ch_seek((POSITION)0))
	{
		/* 
		 * Probably a pipe with beginning of file no longer buffered. 
		 */
		error("Cannot get to beginning of file");
		return;
	}

	/*
	 * Start counting lines.
	 */
	while (--n > 0)
	{
		while ((c = ch_forw_get()) != '\n')
			if (c == EOF)
			{
				error("File is not that long");
				/* {{ Maybe tell him how long it is? }} */
				return;
			}
	}

	/*
	 * Finally found the place to start.
	 * Clear and redisplay the screen from there.
	 *
	 * {{ We *could* figure out if the new position is 
	 *    close enough to just scroll there without clearing
	 *    the screen, but it's not worth it. }}
	 */
	prepaint(ch_tell());
}

/*
 * Jump to a specified percentage into the file.
 * This is a poor compensation for not being able to
 * quickly jump to a specific line number.
 */
	public void
jump_percent(percent)
	int percent;
{
	POSITION pos, len;

	/*
	 * Determine the position in the file
	 * (the specified percentage of the file's length).
	 */
	if ((len = ch_length()) == NULL_POSITION)
	{
		error("Don't know length of file");
		return;
	}
	pos = (percent * len) / 100;
	jump_loc(pos);
}

	public void
jump_loc(pos)
	POSITION pos;
{
	register int c;
	register int nline;
	POSITION tpos;

	/*
	 * See if the desired line is BEFORE the currently
	 * displayed screen.  If so, see if it is close enough 
	 * to scroll backwards to it.
	 */
	tpos = position(TOP);
	if (pos < tpos)
	{
		for (nline = 1;  nline <= back_scroll;  nline++)
		{
			tpos = back_line(tpos);
			if (tpos == NULL_POSITION || tpos <= pos)
			{
				back(nline, position(TOP), 1, 0);
				return;
			}
		}
	} else if ((nline = onscreen(pos)) >= 0)
	{
		/*
		 * The line is currently displayed.  
		 * Just scroll there.
		 */
		forw(nline, position(BOTTOM_PLUS_ONE), 1, 0);
		return;
	}

	/*
	 * Line is not on screen.
	 * Back up to the beginning of the current line.
	 */
	if (ch_seek(pos))
	{
		error("Cannot seek to that position");
		return;
	}
	while ((c = ch_back_get()) != '\n' && c != EOF)
		;
	if (c == '\n')
		(void) ch_forw_get();

	/*
	 * Clear and paint the screen.
	 */
	prepaint(ch_tell());
}

/*
 * The table of marks.
 * A mark is simply a position in the file.
 */
static POSITION marks[26];

/*
 * Initialize the mark table to show no marks are set.
 */
	public void
init_mark()
{
	int i;

	for (i = 0;  i < 26;  i++)
		marks[i] = NULL_POSITION;
}

/*
 * See if a mark letter is valid (between a and z).
 */
	static int
badmark(c)
	int c;
{
	if (c < 'a' || c > 'z')
	{
		error("Choose a letter between 'a' and 'z'");
		return (1);
	}
	return (0);
}

/*
 * Set a mark.
 */
	public void
setmark(c)
	int c;
{
	if (badmark(c))
		return;
	marks[c-'a'] = position(TOP);
}

/*
 * Go to a previously set mark.
 */
	public void
gomark(c)
	int c;
{
	POSITION pos;

	if (badmark(c))
		return;
	if ((pos = marks[c-'a']) == NULL_POSITION)
		error("mark not set");
	else
		jump_loc(pos);
}

/*
 * Search for the n-th occurence of a specified pattern, 
 * either forward (direction == '/'), or backwards (direction == '?').
 */
	public void
search(direction, pattern, n)
	int direction;
	char *pattern;
	register int n;
{
	register int search_forward = (direction == '/');
	POSITION pos, linepos;

#if RECOMP
	char *re_comp();
	char *errmsg;

	/*
	 * (re_comp handles a null pattern internally, 
	 *  so there is no need to check for a null pattern here.)
	 */
	if ((errmsg = re_comp(pattern)) != NULL)
	{
		error(errmsg);
		return;
	}
#else
#if REGCMP
	char *regcmp();
	static char *cpattern = NULL;

	if (pattern == NULL || *pattern == '\0')
	{
		/*
		 * A null pattern means use the previous pattern.
		 * The compiled previous pattern is in cpattern, so just use it.
		 */
		if (cpattern == NULL)
		{
			error("No previous regular expression");
			return;
		}
	} else
	{
		/*
		 * Otherwise compile the given pattern.
		 */
		char *s;
		if ((s = regcmp(pattern, 0)) == NULL)
		{
			error("Invalid pattern");
			return;
		}
		if (cpattern != NULL)
			free(cpattern);
		cpattern = s;
	}
#else
	static char lpbuf[100];
	static char *last_pattern = NULL;

	if (pattern == NULL || *pattern == '\0')
	{
		/*
		 * Null pattern means use the previous pattern.
		 */
		if (last_pattern == NULL)
		{
			error("No previous regular expression");
			return;
		}
		pattern = last_pattern;
	} else
	{
		strcpy(lpbuf, pattern);
		last_pattern = lpbuf;
	}
#endif
#endif

	/*
	 * Figure out where to start the search.
	 */

	if (position(TOP) == NULL_POSITION)
	{
		/*
		 * Nothing is currently displayed.
		 * Start at the beginning of the file.
		 * (This case is mainly for first_cmd searches,
		 * for example, "+/xyz" on the command line.)
		 */
		pos = (POSITION)0;
	} else if (!search_forward)
	{
		/*
		 * Backward search: start just before the top line
		 * displayed on the screen.
		 */
		pos = position(TOP);
	} else if (top_search)
	{
		/*
		 * Forward search and "start from top".
		 * Start at the second line displayed on the screen.
		 */
		pos = position(TOP_PLUS_ONE);
	} else
	{
		/*
		 * Forward search but don't "start from top".
		 * Start just after the bottom line displayed on the screen.
		 */
		pos = position(BOTTOM_PLUS_ONE);
	}

	if (pos == NULL_POSITION)
	{
		/*
		 * Can't find anyplace to start searching from.
		 */
		error("Nothing to search");
		return;
	}

	for (;;)
	{
		/*
		 * Get lines until we find a matching one or 
		 * until we hit end-of-file (or beginning-of-file 
		 * if we're going backwards).
		 */
		if (sigs)
			/*
			 * A signal aborts the search.
			 */
			return;

		if (search_forward)
		{
			/*
			 * Read the next line, and save the 
			 * starting position of that line in linepos.
			 */
			linepos = pos;
			pos = forw_raw_line(pos);
		} else
		{
			/*
			 * Read the previous line and save the
			 * starting position of that line in linepos.
			 */
			pos = back_raw_line(pos);
			linepos = pos;
		}

		if (pos == NULL_POSITION)
		{
			/*
			 * We hit EOF/BOF without a match.
			 */
			error("Pattern not found");
			return;
		}

		/*
		 * Test the next line to see if we have a match.
		 * This is done in a variety of ways, depending
		 * on what pattern matching functions are available.
		 */
#if REGCMP
		if ( (regex(cpattern, line) != NULL)
#else
#if RECOMP
		if ( (re_exec(line) == 1)
#else
		if ( (match(pattern, line))
#endif
#endif
				&& (--n <= 0) )
			/*
			 * Found the matching line.
			 */
			break;
	}
	jump_loc(linepos);
}

#if (!REGCMP) && (!RECOMP)
/*
 * We have neither regcmp() nor re_comp().
 * We use this function to do simple pattern matching.
 * It supports no metacharacters like *, etc.
 */
	static int
match(pattern, buf)
	char *pattern, *buf;
{
	register char *pp, *lp;

	for ( ;  *buf != '\0';  buf++)
	{
		for (pp = pattern, lp = buf;  *pp == *lp;  pp++, lp++)
			if (*pp == '\0' || *lp == '\0')
				break;
		if (*pp == '\0')
			return (1);
	}
	return (0);
}
#endif
