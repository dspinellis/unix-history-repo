/* tio.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains terminal I/O functions */

#include "config.h"
#if BSD || COHERENT
# include <setjmp.h>
#endif
#include <signal.h>
#include "vi.h"


/* This function reads in a line from the terminal. */
int vgets(prompt, buf, bsize)
	char	prompt;	/* the prompt character, or '\0' for none */
	char	*buf;	/* buffer into which the string is read */
	int	bsize;	/* size of the buffer */
{
	int	len;	/* how much we've read so far */
	int	ch;	/* a character from the user */
	int	quoted;	/* is the next char quoted? */
	int	tab;	/* column position of cursor */
	char	widths[132];	/* widths of characters */
#ifndef NO_DIGRAPH
	int	erased;	/* 0, or first char of a digraph */
#endif

	/* show the prompt */
	move(LINES - 1, 0);
	tab = 0;
	if (prompt)
	{
		addch(prompt);
		tab = 1;
	}
	clrtoeol();
	refresh();

	/* read in the line */
#ifndef NO_DIGRAPH
	erased =
#endif
	quoted = len = 0;
	for (;;)
	{
		ch = getkey(quoted ? 0 : WHEN_EX);

		/* some special conversions */
		if (ch == ctrl('D') && len == 0)
			ch = ctrl('[');
#ifndef NO_DIGRAPH
		if (*o_digraph && erased != 0 && ch != '\b')
		{
			ch = digraph(erased, ch);
			erased = 0;
		}
#endif

		/* inhibit detection of special chars (except ^J) after a ^V */
		if (quoted && ch != '\n')
		{
			ch |= 256;
		}

		/* process the character */
		switch(ch)
		{
		  case ctrl('V'):
			qaddch('^');
			qaddch('\b');
			quoted = TRUE;
			break;

		  case ctrl('['):
			return -1;

		  case '\n':
#if OSK
		  case '\l':
#else
		  case '\r':
#endif
			clrtoeol();
			goto BreakBreak;

		  case '\b':
			if (len > 0)
			{
				len--;
#ifndef NO_DIGRAPH
				erased = buf[len];
#endif
				for (ch = widths[len]; ch > 0; ch--)
					addch('\b');
				if (mode == MODE_EX)
				{
					clrtoeol();
				}
				tab -= widths[len];
			}
			else
			{
				return -1;
			}
			break;

		  default:
			/* strip off quotation bit */
			if (ch & 256)
			{
				ch &= ~256;
				quoted = FALSE;
				qaddch(' ');
				qaddch('\b');
			}
			/* add & echo the char */
			if (len < bsize - 1)
			{
				if (ch == '\t')
				{
					widths[len] = *o_tabstop - (tab % *o_tabstop);
					addstr("        " + 8 - widths[len]);
					tab += widths[len];
				}
				else if (ch > 0 && ch < ' ') /* > 0 by GB */
				{
					addch('^');
					addch(ch + '@');
					widths[len] = 2;
					tab += 2;
				}
				else if (ch == '\177')
				{
					addch('^');
					addch('?');
					widths[len] = 2;
					tab += 2;
				}
				else
				{
					addch(ch);
					widths[len] = 1;
					tab++;
				}
				buf[len++] = ch;
			}
			else
			{
				beep();
			}
		}
	}
BreakBreak:
	refresh();
	buf[len] = '\0';
	return len;
}


/* ring the terminal's bell */
void beep()
{
	if (*o_vbell)
	{
		do_VB();
		refresh();
	}
	else if (*o_errorbells)
	{
		ttywrite("\007", 1);
	}
}

static int	manymsgs; /* This variable keeps msgs from overwriting each other */
static char	pmsg[80]; /* previous message (waiting to be displayed) */


static int showmsg()
{
	/* if there is no message to show, then don't */
	if (!manymsgs)
		return FALSE;

	/* display the message */
	move(LINES - 1, 0);
	if (*pmsg)
	{
		standout();
		qaddch(' ');
		qaddstr(pmsg);
		qaddch(' ');
		standend();
	}
	clrtoeol();

	manymsgs = FALSE;
	return TRUE;
}


void endmsgs()
{
	if (manymsgs)
	{
		showmsg();
		addch('\n');
	}
}

/* Write a message in an appropriate way.  This should really be a varargs
 * function, but there is no such thing as vwprintw.  Hack!!!
 *
 * In MODE_EX or MODE_COLON, the message is written immediately, with a
 * newline at the end.
 *
 * In MODE_VI, the message is stored in a character buffer.  It is not
 * displayed until getkey() is called.  msg() will call getkey() itself,
 * if necessary, to prevent messages from being lost.
 *
 * msg("")		- clears the message line
 * msg("%s %d", ...)	- does a printf onto the message line
 */
/*VARARGS1*/
void msg(fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	char	*fmt;
	long	arg1, arg2, arg3, arg4, arg5, arg6, arg7;
{
	if (mode != MODE_VI)
	{
		sprintf(pmsg, fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		qaddstr(pmsg);
		addch('\n');
		exrefresh();
	}
	else
	{
		/* wait for keypress between consecutive msgs */
		if (manymsgs)
		{
			getkey(WHEN_MSG);
		}

		/* real message */
		sprintf(pmsg, fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		manymsgs = TRUE;
	}
}


/* This function calls refresh() if the option exrefresh is set */
void exrefresh()
{
	char	*scan;

	/* If this ex command wrote ANYTHING set exwrote so vi's  :  command
	 * can tell that it must wait for a user keystroke before redrawing.
	 */
	for (scan=kbuf; scan<stdscr; scan++)
		if (*scan == '\n')
			exwrote = TRUE;

#if MICROSOFT			/* avoid compiler bug */
	scan = stdscr;
#define	stdscr	scan
#endif
	/* now we do the refresh thing */
	if (*o_exrefresh)
	{
		refresh();
	}
	else
	{
		wqrefresh(stdscr);
	}
#if MICROSOFT
#undef	stdscr
	stdscr = scan;
#endif
	if (mode != MODE_VI)
	{
		manymsgs = FALSE;
	}
}



/* These are used for typeahead, and also for fudging the visual @ command */
static char	keybuf[100];	/* array of already-read keys */
static int	nkeys;		/* total number of keys in keybuf */
static int	next;		/* index of next key to return */
#ifndef NO_AT
static int	atnext;		/* index of next key for "@", or 0 normally */
int fromcutbuf(cbname)
	int	cbname;
{
	int	len;

	/* fail if we're already doing an @ macro */
	if (atnext > 0 && keybuf[atnext])
	{
		msg("Can't nest @ commands");
		return FALSE;
	}

	/* use the empty portion of keybuf[] to get chars from the cut buffer */
	len = cb2str(cbname, keybuf + nkeys, sizeof keybuf - nkeys);
	if (len < 0)
	{
		msg("Invalid cut buffer name.  Must be a-z");
		return FALSE;
	}
	if (len == 0)
	{
		msg("Cut buffer \"%c is empty", cbname);
		return FALSE;
	}
	else if (len >= sizeof keybuf - nkeys)
	{
		msg("Cut buffer \"%c is too large to execute", cbname);
		return FALSE;
	}

	/* prepare to "read" those keys on subsequent getkey() calls */
	atnext = nkeys;
	return TRUE;
}
#endif

/* This array describes mapped key sequences */
static struct _keymap
{
	char	*name;		/* name of the key, or NULL */
	char	rawin[LONGKEY];	/* the unmapped version of input */
	char	cooked[80];	/* the mapped version of input */
	int	len;		/* length of the unmapped version */
	int	when;		/* when is this key mapped? */
}
	mapped[MAXMAPS];

#if !MSDOS && !TOS
# if BSD || COHERENT
static jmp_buf env_timeout;
static int dummy()
{
	longjmp(env_timeout, 1);
	return 0;
}
# else 
static int dummy()
{
}
# endif
#endif

/* This function reads in a keystroke for VI mode.  It automatically handles
 * key mapping.
 */
int getkey(when)
	int		when;		/* which bits must be ON? */
{
	static char	*cooked;	/* rawin, or pointer to converted key */ 
	static int	oldwhen;	/* "when" from last time */
	static int	oldleft;
	static long	oldtop;
	static long	oldnlines;
	static char	*cshape;	/* current cursor shape */
	REG char	*kptr;		/* &keybuf[next] */
	REG struct _keymap *km;	/* used to count through keymap */
	REG int		i, j, k;

#ifdef DEBUG
	watch();
#endif

	/* if this key is needed for delay between multiple error messages,
	 * then reset the manymsgs flag and abort any mapped key sequence.
	 */
	if (showmsg())
	{
		if (when == WHEN_MSG)
		{
			qaddstr("[More...]");
			refresh();
			cooked = (char *)0;
		}
		else if (when == WHEN_VIINP || when == WHEN_VIREP)
		{
			redraw(cursor, TRUE);
		}
	}

#ifndef NO_AT
	/* if we're in the middle of a visual @ macro, take atnext */
	if (atnext > 0)
	{
		if (keybuf[atnext])
		{
			return keybuf[atnext++];
		}
		atnext = 0;
	}
#endif

	/* if we're doing a mapped key, get the next char */
	if (cooked && *cooked)
	{
		return *cooked++;
	}

	/* if keybuf is empty, fill it */
	if (next == nkeys)
	{
#ifndef NO_CURSORSHAPE
		/* make sure the cursor is the right shape */
		if (has_CQ)
		{
			cooked = cshape;
			switch (when)
			{
			  case WHEN_EX:		cooked = CX;	break;
			  case WHEN_VICMD:	cooked = CV;	break;
			  case WHEN_VIINP:	cooked = CI;	break;
			  case WHEN_VIREP:	cooked = CR;	break;
			}
			if (cooked != cshape)
			{
				cshape = cooked;
				switch (when)
				{
				  case WHEN_EX:		do_CX();	break;
				  case WHEN_VICMD:	do_CV();	break;
				  case WHEN_VIINP:	do_CI();	break;
				  case WHEN_VIREP:	do_CR();	break;
				}
			}
			cooked = (char *)0;
		}
#endif

#ifndef NO_SHOWMODE
		/* if "showmode" then say which mode we're in */
		if (*o_smd
		 && mode == MODE_VI
		 && (when != oldwhen || topline != oldtop || leftcol != oldleft || nlines != oldnlines))
		{
			oldwhen = when;
			oldtop = topline;
			oldleft = leftcol;
			oldnlines = nlines;

			if (when & WHEN_VICMD)
			{
				redraw(cursor, FALSE);
				move(LINES - 1, COLS - 10);
				standout();
				addstr("Command");
				standend();
				redraw(cursor, FALSE);
			}
			else if (when & WHEN_VIINP)
			{
				redraw(cursor, TRUE);
				move(LINES - 1, COLS - 10);
				standout();
				addstr(" Input ");
				standend();
				redraw(cursor, TRUE);
			}
			else if (when & WHEN_VIREP)
			{
				redraw(cursor, TRUE);
				move(LINES - 1, COLS - 10);
				standout();
				addstr("Replace");
				standend();
				redraw(cursor, TRUE);
			}
		}
		else
#endif

		/* redraw if getting a VI command */
		if (when & WHEN_VICMD)
		{
			redraw(cursor, FALSE);
		}

		/* read the rawin keystrokes */
		refresh();
		while ((nkeys = ttyread(keybuf, sizeof keybuf)) < 0)
		{
			/* terminal was probably resized */
			*o_lines = LINES;
			*o_columns = COLS;
			if (when & (WHEN_VICMD|WHEN_VIINP|WHEN_VIREP))
			{
				redraw(MARK_UNSET, FALSE);
				redraw(cursor, (when & WHEN_VICMD) == 0);
				refresh();
			}
		}
		next = 0;

		/* if nkeys == 0 then we've reached EOF of an ex script. */
		if (nkeys == 0)
		{
			tmpabort(TRUE);
			move(LINES - 1, 0);
			clrtoeol();
			refresh();
			endwin();
			exit(1);
		}
	}

	/* see how many mapped keys this might be */
	kptr = &keybuf[next];
	for (i = j = 0, k = -1, km = mapped; i < MAXMAPS; i++, km++)
	{
		if ((km->when & when) && km->len > 0 && *km->rawin == *kptr)
		{
			if (km->len > nkeys - next)
			{
				if (!strncmp(km->rawin, kptr, nkeys - next))
				{
					j++;
				}
			}
			else
			{
				if (!strncmp(km->rawin, kptr, km->len))
				{
					j++;
					k = i;
				}
			}
		}
	}

	/* if more than one, try to read some more */
	while (j > 1)
	{
#if BSD || COHERENT
		if (setjmp(env_timeout))
		{
			/* we timed out - assume no mapping */
			j = 0;
			break;
		}
#endif
#if ANY_UNIX
		signal(SIGALRM, dummy);
#endif
#if OSK
		signal(SIGQUIT, dummy);
#endif
		alarm((unsigned)*o_keytime);
		i = nkeys;
		if ((k = ttyread(keybuf + nkeys, sizeof keybuf - nkeys)) >= 0)
		{
			nkeys += k;
		}
		alarm(0);
#if OSK
# ifndef DEBUG
		signal(SIGQUIT, SIG_IGN);
# endif
#endif

		/* if we couldn't read any more, pretend 0 mapped keys */
		if (i == nkeys)
		{
			j = 0;
		}
		else /* else we got some more - try again */
		{
			for (i = j = 0, k = -1, km = mapped; i < MAXMAPS; i++, km++)
			{
				if ((km->when & when) && km->len > 0 && *km->rawin == *kptr)
				{
					if (km->len > nkeys - next)
					{
						if (!strncmp(km->rawin, kptr, nkeys - next))
						{
							j++;
						}
					}
					else
					{
						if (!strncmp(km->rawin, kptr, km->len))
						{
							j++;
							k = i;
						}
					}
				}
			}
		}
	}

	/* if unambiguously mapped key, use it! */
	if (j == 1 && k >= 0)
	{
		next += mapped[k].len;
		cooked = mapped[k].cooked;
#ifndef NO_EXTENSIONS
		if ((when & (WHEN_VIINP|WHEN_VIREP))
		 && (mapped[k].when & WHEN_INMV))
		{
			return 0; /* special case, means "a movement char follows" */
		}
		else
#endif
		{
			return *cooked++;
		}
	}
	else
	/* assume key is unmapped, but still translate weird erase key to '\b' */
	if (keybuf[next] == ERASEKEY && when != 0)
	{
		next++;
		return '\b';
	}
	else if (keybuf[next] == '\0')
	{
		next++;
		return ('A' & 0x1f);
	}
	else
	{
		return keybuf[next++];
	}
}


/* This function maps or unmaps a key */
void mapkey(rawin, cooked, when, name)
	char	*rawin;	/* the input key sequence, before mapping */
	char	*cooked;/* after mapping */
	short	when;	/* bitmap of when mapping should happen */
	char	*name;	/* name of the key, if any */
{
	int	i, j;

#ifndef NO_EXTENSIONS
	/* if the mapped version starts with the word "visual" then set WHEN_INMV */
	if (!strncmp(cooked, "visual ", 7))
	{
		when |= WHEN_INMV;
		cooked += 7;
	}
	/* if WHEN_INMV is set, then WHEN_VIINP and WHEN_VIREP must be set */
	if (when & WHEN_INMV)
	{
		when |= (WHEN_VIINP | WHEN_VIREP);
	}
#endif

	/* see if the key sequence was mapped before */
	j = strlen(rawin);
	for (i = 0; i < MAXMAPS; i++)
	{
		if (mapped[i].len == j
		 && !strncmp(mapped[i].rawin, rawin, j)
		 && (mapped[i].when & when))
		{
			break;
		}
	}

	/* if not already mapped, then try to find a new slot to use */
	if (i == MAXMAPS)
	{
		for (i = 0; i < MAXMAPS && mapped[i].len > 0; i++)
		{
		}
	}

	/* no room for the new key? */
	if (i == MAXMAPS)
	{
		msg("No room left in the key map table");
		return;
	}

	/* map the key */
	if (cooked && *cooked)
	{
		/* Map the key */
		mapped[i].len = j;
		strncpy(mapped[i].rawin, rawin, j);
		strcpy(mapped[i].cooked, cooked);
		mapped[i].when = when;
		mapped[i].name = name;
	}
	else /* unmap the key */
	{
		mapped[i].len = 0;
	}
}

/* Dump keys of a given type - WHEN_VICMD dumps the ":map" keys, and
 * WHEN_VIINP|WHEN_VIREP dumps the ":map!" keys
 */
void dumpkey(when)
	int	when;	/* WHEN_XXXX of mappings to be dumped */
{
	int	i, len, mlen;
	char	*scan;
	char	*mraw;

	for (i = 0; i < MAXMAPS; i++)
	{
		/* skip unused entries, or entries that don't match "when" */
		if (mapped[i].len <= 0 || !(mapped[i].when & when))
		{
			continue;
		}

		/* dump the key label, if any */
		len = 8;
		if (mapped[i].name)
		{
			qaddstr(mapped[i].name);
			len -= strlen(mapped[i].name);
		}
		do
		{
			qaddch(' ');
		} while (len-- > 0);

		/* dump the raw version */
		len = 0;
		mlen = mapped[i].len;
		mraw = mapped[i].rawin;
		for (scan = mraw; scan < mraw + mlen; scan++)
		{
			if (UCHAR(*scan) < ' ' || *scan == '\177')
			{
				qaddch('^');
				qaddch(*scan ^ '@');
				len += 2;
			}
			else
			{
				qaddch(*scan);
				len++;
			}
		}
		do
		{
			qaddch(' ');
		} while (++len < 8);

		/* dump the mapped version */
#ifndef NO_EXTENSIONS
		if ((mapped[i].when & WHEN_INMV) && (when & (WHEN_VIINP|WHEN_VIREP)))
		{
			qaddstr("visual ");
		}
#endif
		for (scan = mapped[i].cooked; *scan; scan++)
		{
			if (UCHAR(*scan) < ' ' || *scan == '\177')
			{
				qaddch('^');
				qaddch(*scan ^ '@');
			}
			else
			{
				qaddch(*scan);
			}
		}

		addch('\n');
		exrefresh();
	}
}



#ifndef MKEXRC
/* This function saves the current configuration of mapped keys to a file */
void savekeys(fd)
	int	fd;	/* file descriptor to save them to */
{
	int	i;
	char	buf[80];

	/* now write a map command for each key other than the arrows */
	for (i = 0; i < MAXMAPS; i++)
	{
		/* ignore keys that came from termcap */
		if (mapped[i].name)
		{
			continue;
		}

		/* If this isn't used, ignore it */
		if (mapped[i].len <= 0)
		{
			continue;
		}

		/* write the map command */
#ifndef NO_EXTENSIONS
		if (mapped[i].when & WHEN_INMV)
		{
#if OSK
			char	fmt[80];
			sprintf(fmt, "map%%s %%.%ds %%s\n", mapped[i].len);
			sprintf(buf, fmt,
				(mapped[i].when & WHEN_VICMD) ? "" : "!",
#else
			sprintf(buf, "map%s %.*s visual %s\n",
				(mapped[i].when & WHEN_VICMD) ? "" : "!",
				mapped[i].len,
#endif
				mapped[i].rawin,
				mapped[i].cooked);
			twrite(fd, buf, strlen(buf));
		}
		else
#endif
		{
			if (mapped[i].when & WHEN_VICMD)
			{
#if OSK
				char	fmt[80];
				sprintf(fmt, "map %%.%ds %%s\n", mapped[i].len);
				sprintf(buf, fmt,
#else
				sprintf(buf, "map %.*s %s\n", mapped[i].len,
#endif
					mapped[i].rawin,
					mapped[i].cooked);
				twrite(fd, buf, strlen(buf));
			}
			if (mapped[i].when & (WHEN_VIINP | WHEN_VIREP))
			{
#if OSK
				char	fmt[80];
				sprintf(fmt, "map! %%.%ds %%s\n", mapped[i].len);
				sprintf(buf, fmt,
#else
				sprintf(buf, "map! %.*s %s\n", mapped[i].len,
#endif
					mapped[i].rawin,
					mapped[i].cooked);
				twrite(fd, buf, strlen(buf));
			}
		}
	}
}
#endif
