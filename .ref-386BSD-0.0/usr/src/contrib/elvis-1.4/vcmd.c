/* vcmd.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains the functions that handle VI commands */


#include "config.h"
#include "vi.h"
#if MSDOS
#include <process.h>
#include <string.h>
#endif
#if TOS
#include <osbind.h>
#include <string.h>
#endif
#if OSK
# include <stdio.h>
#endif


/* This function puts the editor in EX mode */
MARK v_quit()
{
	move(LINES - 1, 0);
	mode = MODE_EX;
	return cursor;
}

/* This function causes the screen to be redrawn */
MARK v_redraw()
{
	redraw(MARK_UNSET, FALSE);
	return cursor;
}

/* This function executes a single EX command, and waits for a user keystroke
 * before returning to the VI screen.  If that keystroke is another ':', then
 * another EX command is read and executed.
 */
/*ARGSUSED*/
MARK v_1ex(m, text)
	MARK	m;	/* the current line */
	char	*text;	/* the first command to execute */
{
	/* run the command.  be careful about modes & output */
	exwrote = (mode == MODE_COLON);
	doexcmd(text);
	exrefresh();

	/* if mode is no longer MODE_VI, then we should quit right away! */
	if (mode != MODE_VI && mode != MODE_COLON)
		return cursor;

	/* The command did some output.  Wait for a keystoke. */
	if (exwrote)
	{
		mode = MODE_VI;	
		msg("[Hit <RETURN> to continue]");
		if (getkey(0) == ':')
		{	mode = MODE_COLON;
			addch('\n');
		}
		else
			redraw(MARK_UNSET, FALSE);
	}

	return cursor;
}

/* This function undoes the last change */
/*ARGSUSED*/
MARK v_undo(m)
	MARK	m;	/* (ignored) */
{
	if (undo())
	{
		redraw(MARK_UNSET, FALSE);
	}
	return cursor;
}

/* This function deletes the character(s) that the cursor is on */
MARK v_xchar(m, cnt, cmd)
	MARK	m;	/* where to start deletions */
	long	cnt;	/* number of chars to delete */
	int	cmd;	/* either 'x' or 'X' */
{
	DEFAULT(1);

	/* for 'X', adjust so chars are deleted *BEFORE* cursor */
	if (cmd == 'X')
	{
		if (markidx(m) < cnt)
			return MARK_UNSET;
		m -= cnt;
	}

	/* make sure we don't try to delete more thars than there are */
	pfetch(markline(m));
	if (markidx(m + cnt) > plen)
	{
		cnt = plen - markidx(m);
	}
	if (cnt == 0L)
	{
		return MARK_UNSET;
	}

	/* do it */
	ChangeText
	{
		cut(m, m + cnt);
		delete(m, m + cnt);
	}
	return m;
}

/* This function defines a mark */
/*ARGSUSED*/
MARK v_mark(m, count, key)
	MARK	m;	/* where the mark will be */
	long	count;	/* (ignored) */
	int	key;	/* the ASCII label of the mark */
{
	if (key < 'a' || key > 'z')
	{
		msg("Marks must be from a to z");
	}
	else
	{
		mark[key - 'a'] = m;
	}
	return m;
}

/* This function toggles upper & lower case letters */
MARK v_ulcase(m, cnt)
	MARK	m;	/* where to make the change */
	long	cnt;	/* number of chars to flip */
{
	REG char 	*pos;
	REG int		i, j;
	static char	flip[] =
		"aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ[](){}<>";

	DEFAULT(1);

	/* fetch the current version of the line */
	pfetch(markline(m));

	/* for each position in the line */
	for (j = 0, i = markidx(m); j < cnt && ptext[i]; j++, i++)
	{
		tmpblk.c[j] = 0;

		/* one of the standard chars? */
		for (pos = flip; *pos && *pos != ptext[i]; pos++)
		{
		}
		if (*pos)
		{
			tmpblk.c[j] = flip[(int)(pos - flip) ^ 1];
		}
#ifndef NO_DIGRAPH
		else /* one of the non-standard chars? */
		{
			for (pos = o_flipcase; *pos && *pos != ptext[i]; pos++)
			{
			}
			if (*pos)
			{
				tmpblk.c[j] = o_flipcase[(int)(pos - o_flipcase) ^ 1];
			}
		}
#endif

		/* if nothing special, then don't change it */
		if (tmpblk.c[j] == 0)
		{
			tmpblk.c[j] = ptext[i];
		}
	}

	/* if the new text is different from the old, then change it */
	if (strncmp(tmpblk.c, &ptext[markidx(m)], j))
	{
		ChangeText
		{
			tmpblk.c[j] = '\0';
			change(m, m + j, tmpblk.c);
		}
	}

	return m + j;
}


MARK v_replace(m, cnt, key)
	MARK	m;	/* first char to be replaced */
	long	cnt;	/* number of chars to replace */
	int	key;	/* what to replace them with */
{
	REG char	*text;
	REG int		i;

	DEFAULT(1);

	/* map ^M to '\n' */
	if (key == '\r')
	{
		key = '\n';
	}

	/* make sure the resulting line isn't too long */
	if (cnt > BLKSIZE - 2 - markidx(m))
	{
		cnt = BLKSIZE - 2 - markidx(m);
	}

	/* build a string of the desired character with the desired length */
	for (text = tmpblk.c, i = cnt; i > 0; i--)
	{
		*text++ = key;
	}
	*text = '\0';

	/* make sure cnt doesn't extend past EOL */
	pfetch(markline(m));
	key = markidx(m);
	if (key + cnt > plen)
	{
		cnt = plen - key;
	}

	/* do the replacement */
	ChangeText
	{
		change(m, m + cnt, tmpblk.c);
	}

	if (*tmpblk.c == '\n')
	{
		return (m & ~(BLKSIZE - 1)) + cnt * BLKSIZE;
	}
	else
	{
		return m + cnt - 1;
	}
}

MARK v_overtype(m)
	MARK		m;	/* where to start overtyping */
{
	MARK		end;	/* end of a substitution */
	static long	width;	/* width of a single-line replace */

	/* the "doingdot" version of replace is really a substitution */
	if (doingdot)
	{
		/* was the last one really repeatable? */
		if (width < 0)
		{
			msg("Can't repeat a multi-line overtype command");
			return MARK_UNSET;
		}

		/* replacing nothing by nothing?  Don't bother */
		if (width == 0)
		{
			return m;
		}

		/* replace some chars by repeated text */
		return v_subst(m, width);
	}

	/* Normally, we input starting here, in replace mode */
	ChangeText
	{
		end = input(m, m, WHEN_VIREP);
	}

	/* if we ended on the same line we started on, then this
	 * overtype is repeatable via the dot key.
	 */
	if (markline(end) == markline(m) && end >= m - 1L)
	{
		width = end - m + 1L;
	}
	else /* it isn't repeatable */
	{
		width = -1L;
	}

	return end;
}


/* This function selects which cut buffer to use */
/*ARGSUSED*/
MARK v_selcut(m, cnt, key)
	MARK	m;
	long	cnt;
	int	key;
{
	cutname(key);
	return m;
}

/* This function pastes text from a cut buffer */
/*ARGSUSED*/
MARK v_paste(m, cnt, cmd)
	MARK	m;	/* where to paste the text */
	long	cnt;	/* (ignored) */
	int	cmd;	/* either 'p' or 'P' */
{
	ChangeText
	{
		m = paste(m, cmd == 'p', FALSE);
	}
	return m;
}

/* This function yanks text into a cut buffer */
MARK v_yank(m, n)
	MARK	m, n;	/* range of text to yank */
{
	cut(m, n);
	return m;
}

/* This function deletes a range of text */
MARK v_delete(m, n)
	MARK	m, n;	/* range of text to delete */
{
	/* illegal to try and delete nothing */
	if (n <= m)
	{
		return MARK_UNSET;
	}

	/* Do it */
	ChangeText
	{
		cut(m, n);
		delete(m, n);
	}
	return m;
}


/* This starts input mode without deleting anything */
MARK v_insert(m, cnt, key)
	MARK	m;	/* where to start (sort of) */
	long	cnt;	/* repeat how many times? */
	int	key;	/* what command is this for? {a,A,i,I,o,O} */
{
	int	wasdot;
	long	reps;
	int	after;	/* are we appending or inserting? */

	DEFAULT(1);

	ChangeText
	{
		/* tweak the insertion point, based on command key */
		switch (key)
		{
		  case 'i':
			after = FALSE;
			break;

		  case 'a':
			pfetch(markline(m));
			if (plen > 0)
			{
				m++;
			}
			after = TRUE;
			break;

		  case 'I':
			m = m_front(m, 1L);
			after = FALSE;
			break;

		  case 'A':
			pfetch(markline(m));
			m = (m & ~(BLKSIZE - 1)) + plen;
			after = TRUE;
			break;

		  case 'O':
			m &= ~(BLKSIZE - 1);
			add(m, "\n");
			after = FALSE;
			break;

		  case 'o':
			m = (m & ~(BLKSIZE - 1)) + BLKSIZE;
			add(m, "\n");
			after = FALSE;
			break;
		}

		/* insert the same text once or more */
		for (reps = cnt, wasdot = doingdot; reps > 0; reps--, doingdot = TRUE)
		{
			m = input(m, m, WHEN_VIINP);
			if (after)
			{
				m++;
			}
		}
		if (after)
		{
			m--;
		}

		doingdot = wasdot;
	}

#ifndef CRUNCH
# ifndef NO_EXTENSIONS
	if (key == 'i' && *o_inputmode && mode == MODE_VI)
	{
		msg("Now in visual command mode!  To return to input mode, hit <i>.");
	}
# endif
#endif

	return m;
}

/* This starts input mode with some text deleted */
MARK v_change(m, n)
	MARK	m, n;	/* the range of text to change */
{
	int	lnmode;	/* is this a line-mode change? */

	/* swap them if they're in reverse order */
	if (m > n)
	{
		MARK	tmp;
		tmp = m;
		m = n;
		n = tmp;
	}

	/* for line mode, retain the last newline char */
	lnmode = (markidx(m) == 0 && markidx(n) == 0 && m != n);
	if (lnmode)
	{
		n -= BLKSIZE;
		pfetch(markline(n));
		n = (n & ~(BLKSIZE - 1)) + plen;
	}

	ChangeText
	{
		cut(m, n);
		m = input(m, n, WHEN_VIINP);
	}

	return m;
}

/* This function replaces a given number of characters with input */
MARK v_subst(m, cnt)
	MARK	m;	/* where substitutions start */
	long	cnt;	/* number of chars to replace */
{
	DEFAULT(1);

	/* make sure we don't try replacing past EOL */
	pfetch(markline(m));
	if (markidx(m) + cnt > plen)
	{
		cnt = plen - markidx(m);
	}

	/* Go for it! */
	ChangeText
	{
		cut(m, m + cnt);
		m = input(m, m + cnt, WHEN_VIINP);
	}
	return m;
}

/* This calls the ex "join" command to join some lines together */
MARK v_join(m, cnt)
	MARK	m;	/* the first line to be joined */
	long	cnt;	/* number of other lines to join */
{
	MARK	joint;	/* where the lines were joined */

	DEFAULT(1);

	/* figure out where the joint will be */
	pfetch(markline(m));
	joint = (m & ~(BLKSIZE - 1)) + plen;

	/* join the lines */
	cmd_join(m, m + MARK_AT_LINE(cnt), CMD_JOIN, 0, "");
	mustredraw = TRUE;

	/* the cursor should be left at the joint */
	return joint;
}

/* This calls the ex shifter command to shift some lines */
static MARK shift_help(m, n, excmd)
	MARK	m, n;	/* range of lines to shift */
	CMD	excmd;	/* which way do we shift? */
{
	/* adjust for inclusive endmarks in ex */
	n -= BLKSIZE;

	cmd_shift(m, n, excmd, 0, "");
	return m;
}

/* This calls the ex "<" command to shift some lines left */
MARK v_lshift(m, n)
	MARK	m, n;	/* range of lines to shift */
{
	return shift_help(m, n, CMD_SHIFTL);
}

/* This calls the ex ">" command to shift some lines right */
MARK v_rshift(m, n)
	MARK	m, n;	/* range of lines to shift */
{
	return shift_help(m, n, CMD_SHIFTR);
}

/* This runs some lines through a filter program */
MARK v_filter(m, n)
	MARK	m, n;	/* range of lines to shift */
{
	char	cmdln[100];	/* a shell command line */

	/* adjust for inclusive endmarks in ex */
	n -= BLKSIZE;

	if (vgets('!', cmdln, sizeof(cmdln)) > 0)
	{
		filter(m, n, cmdln);
	}

	redraw(MARK_UNSET, FALSE);
	return m;
}


/* This function runs the ex "file" command to show the file's status */
MARK v_status()
{
	cmd_file(cursor, cursor, CMD_FILE, 0, "");
	return cursor;
}


/* This function runs the ":&" command to repeat the previous :s// */
MARK v_again(m, n)
	MARK	m, n;
{
	cmd_substitute(m, n - BLKSIZE, CMD_SUBAGAIN, TRUE, "");
	return cursor;
}



/* This function switches to the previous file, if possible */
MARK v_switch()
{
	if (!*prevorig)
		msg("No previous file");
	else
	{	strcpy(tmpblk.c, prevorig);
		cmd_edit(cursor, cursor, CMD_EDIT, 0, tmpblk.c);
	}
	return cursor;
}

/* This function does a tag search on a keyword */
/*ARGSUSED*/
MARK v_tag(keyword, m, cnt)
	char	*keyword;
	MARK	m;
	long	cnt;
{
	/* move the cursor to the start of the tag name, where m is */
	cursor = m;

	/* perform the tag search */
	cmd_tag(cursor, cursor, CMD_TAG, 0, keyword);

	return cursor;
}

#ifndef NO_EXTENSIONS
/* This function looks up a keyword by calling the helpprog program */
/*ARGSUSED*/
MARK v_keyword(keyword, m, cnt)
	char	*keyword;
	MARK	m;
	long	cnt;
{
	int	waswarn;
	char	cmdline[130];

	move(LINES - 1, 0);
	addstr("---------------------------------------------------------\n");
	clrtoeol();
	refresh();
	sprintf(cmdline, "%s %s", o_keywordprg, keyword);
	waswarn = *o_warn;
	*o_warn = FALSE;
	suspend_curses();
	if (system(cmdline))
	{
		addstr("<<< failed >>>\n");
	}
	resume_curses(FALSE);
	mode = MODE_VI;
	redraw(MARK_UNSET, FALSE);
	*o_warn = waswarn;

	return m;
}



MARK v_increment(keyword, m, cnt)
	char	*keyword;
	MARK	m;
	long	cnt;
{
	static	sign;
	char	newval[12];
	long	atol();

	DEFAULT(1);

	/* get one more keystroke, unless doingdot */
	if (!doingdot)
	{
		sign = getkey(0);
	}

	/* adjust the number, based on that second keystroke */
	switch (sign)
	{
	  case '+':
	  case '#':
		cnt = atol(keyword) + cnt;
		break;

	  case '-':
		cnt = atol(keyword) - cnt;
		break;

	  case '=':
		break;

	  default:
		return MARK_UNSET;
	}
	sprintf(newval, "%ld", cnt);

	ChangeText
	{
		change(m, m + strlen(keyword), newval);
	}

	return m;
}
#endif


/* This function acts like the EX command "xit" */
/*ARGSUSED*/
MARK v_xit(m, cnt, key)
	MARK	m;	/* ignored */
	long	cnt;	/* ignored */
	int	key;	/* must be a second 'Z' */
{
	/* if second char wasn't 'Z', fail */
	if (key != 'Z')
	{
		return MARK_UNSET;
	}

	/* move the cursor to the bottom of the screen */
	move(LINES - 1, 0);
	clrtoeol();

	/* do the xit command */
	cmd_xit(m, m, CMD_XIT, FALSE, "");

	/* return the cursor */
	return m;
}


/* This function undoes changes to a single line, if possible */
MARK v_undoline(m)
	MARK	m;	/* where we hope to undo the change */
{
	/* make sure we have the right line in the buffer */
	if (markline(m) != U_line)
	{
		return MARK_UNSET;
	}

	/* fix it */
	ChangeText
	{
		strcat(U_text, "\n");
		change(MARK_AT_LINE(U_line), MARK_AT_LINE(U_line + 1), U_text);
	}

	/* nothing in the buffer anymore */
	U_line = -1L;

	/* return, with the cursor at the front of the line */
	return m & ~(BLKSIZE - 1);
}


#ifndef NO_ERRLIST
MARK v_errlist(m)
	MARK	m;
{
	cmd_errlist(m, m, CMD_ERRLIST, FALSE, "");
	return cursor;
}
#endif


#ifndef NO_AT
/*ARGSUSED*/
MARK v_at(m, cnt, key)
	MARK	m;
	long	cnt;
	int	key;
{
	if (!fromcutbuf(key))
	{
		return MARK_UNSET;
	}
	return cursor;
}
#endif
