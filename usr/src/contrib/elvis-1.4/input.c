/* input.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains the input() function, which implements vi's INPUT mode.
 * It also contains the code that supports digraphs.
 */

#include <ctype.h>
#include "config.h"
#include "vi.h"


#ifndef NO_DIGRAPH
static struct _DIG
{
	struct _DIG	*next;
	char		key1;
	char		key2;
	char		dig;
	char		save;
} *digs;

char digraph(key1, key2)
	char	key1;	/* the underlying character */
	char	key2;	/* the second character */
{
	int		newkey;
	REG struct _DIG	*dp;

	/* if digraphs are disabled, then just return the new char */
	if (!*o_digraph)
	{
		return key2;
	}

	/* remember the new key, so we can return it if this isn't a digraph */
	newkey = key2;

	/* sort key1 and key2, so that their original order won't matter */
	if (key1 > key2)
	{
		key2 = key1;
		key1 = newkey;
	}

	/* scan through the digraph chart */
	for (dp = digs;
	     dp && (dp->key1 != key1 || dp->key2 != key2);
	     dp = dp->next)
	{
	}

	/* if this combination isn't in there, just use the new key */
	if (!dp)
	{
		return newkey;
	}

	/* else use the digraph key */
	return dp->dig;
}

/* this function lists or defines digraphs */
void do_digraph(bang, extra)
	int	bang;
	char	extra[];
{
	int		dig;
	REG struct _DIG	*dp;
	struct _DIG	*prev;
	static int	user_defined = FALSE; /* boolean: are all later digraphs user-defined? */
	char		listbuf[8];

	/* if "extra" is NULL, then we've reached the end of the built-ins */
	if (!extra)
	{
		user_defined = TRUE;
		return;
	}

	/* if no args, then display the existing digraphs */
	if (*extra < ' ')
	{
		listbuf[0] = listbuf[1] = listbuf[2] = listbuf[5] = ' ';
		listbuf[7] = '\0';
		for (dig = 0, dp = digs; dp; dp = dp->next)
		{
			if (dp->save || bang)
			{
				dig += 7;
				if (dig >= COLS)
				{
					addch('\n');
					exrefresh();
					dig = 7;
				}
				listbuf[3] = dp->key1;
				listbuf[4] = dp->key2;
				listbuf[6] = dp->dig;
				qaddstr(listbuf);
			}
		}
		addch('\n');
		exrefresh();
		return;
	}

	/* make sure we have at least two characters */
	if (!extra[1])
	{
		msg("Digraphs must be composed of two characters");
		return;
	}

	/* sort key1 and key2, so that their original order won't matter */
	if (extra[0] > extra[1])
	{
		dig = extra[0];
		extra[0] = extra[1];
		extra[1] = dig;
	}

	/* locate the new digraph character */
	for (dig = 2; extra[dig] == ' ' || extra[dig] == '\t'; dig++)
	{
	}
	dig = extra[dig];
	if (!bang && dig)
	{
		dig |= 0x80;
	}

	/* search for the digraph */
	for (prev = (struct _DIG *)0, dp = digs;
	     dp && (dp->key1 != extra[0] || dp->key2 != extra[1]);
	     prev = dp, dp = dp->next)
	{
	}

	/* deleting the digraph? */
	if (!dig)
	{
		if (!dp)
		{
#ifndef CRUNCH
			msg("%c%c not a digraph", extra[0], extra[1]);
#endif
			return;
		}
		if (prev)
			prev->next = dp->next;
		else
			digs = dp->next;
		free(dp);
		return;
	}

	/* if necessary, create a new digraph struct for the new digraph */
	if (dig && !dp)
	{
		dp = (struct _DIG *)malloc(sizeof *dp);
		if (!dp)
		{
			msg("Out of space in the digraph table");
			return;
		}
		if (prev)
			prev->next = dp;
		else
			digs = dp;
		dp->next = (struct _DIG *)0;
	}

	/* assign it the new digraph value */
	dp->key1 = extra[0];
	dp->key2 = extra[1];
	dp->dig = dig;
	dp->save = user_defined;
}

# ifndef NO_MKEXRC
void savedigs(fd)
	int		fd;
{
	static char	buf[] = "digraph! XX Y\n";
	REG struct _DIG	*dp;

	for (dp = digs; dp; dp = dp->next)
	{
		if (dp->save)
		{
			buf[9] = dp->key1;
			buf[10] = dp->key2;
			buf[12] = dp->dig;
			write(fd, buf, (unsigned)14);
		}
	}
}
# endif
#endif


#ifndef NO_ABBR
static struct _AB
{
	struct _AB	*next;
	char		*large;		/* the expanded form */
	char		small[1];	/* the abbreviated form (appended to struct) */
}
	*abbrev;

/* This functions lists or defines abbreviations */
void do_abbr(extra)
	char	*extra;
{
	int		smlen;	/* length of the small form */
	int		lrg;	/* index of the start of the large form */
	REG struct _AB	*ab;	/* used to move through the abbrev list */
	struct _AB	*prev;

	/* no arguments? */
	if (!*extra)
	{
		/* list all current abbreviations */
		for (ab = abbrev; ab; ab = ab->next)
		{
			qaddstr("abbr ");
			qaddstr(ab->small);
			qaddch(' ');
			qaddstr(ab->large);
			addch('\n');
			exrefresh();
		}
		return;
	}

	/* else one or more arguments.  Parse the first & look up in abbrev[] */
	for (smlen = 0; extra[smlen] && isalnum(extra[smlen]); smlen++)
	{
	}
	for (prev = (struct _AB *)0, ab = abbrev; ab; prev = ab, ab = ab->next)
	{
		if (!strncmp(extra, ab->small, smlen) && !ab->small[smlen])
		{
			break;
		}
	}

	/* locate the start of the large form, if any */
	for (lrg = smlen; extra[lrg] && isascii(extra[lrg]) && isspace(extra[lrg]); lrg++)
	{
	}

	/* only one arg? */
	if (!extra[lrg])
	{
		/* trying to undo an abbreviation which doesn't exist? */
		if (!ab)
		{
#ifndef CRUNCH
			msg("\"%s\" not an abbreviation", extra);
#endif
			return;
		}

		/* undo the abbreviation */
		if (prev)
			prev->next = ab->next;
		else
			abbrev = ab->next;
		free(ab->large);
		free(ab);

		return;
	}

	/* multiple args - [re]define an abbreviation */
	if (ab)
	{
		/* redefining - free the old large form */
		free(ab->large);
	}
	else
	{
		/* adding a new definition - make a new struct */
		ab = (struct _AB *)malloc((unsigned)(smlen + sizeof *ab));
#ifndef CRUNCH
		if (!ab)
		{
			msg("Out of memory -- Sorry");
			return;
		}
#endif
		strncpy(ab->small, extra, smlen);
		ab->small[smlen] = '\0';
		ab->next = (struct _AB *)0;
		if (prev)
			prev->next = ab;
		else
			abbrev = ab;
	}

	/* store the new form */
	ab->large = (char *)malloc((unsigned)(strlen(&extra[lrg]) + 1));
	strcpy(ab->large, &extra[lrg]);
}


# ifndef NO_MKEXRC
/* This function is called from cmd_mkexrc() to save the abbreviations */
void saveabbr(fd)
	int	fd;	/* fd to which the :abbr commands should be written */
{
	REG struct _AB	*ab;

	for (ab = abbrev; ab; ab = ab->next)
	{
		twrite(fd, "abbr ", 5);
		twrite(fd, ab->small, strlen(ab->small));
		twrite(fd, " ", 1);
		twrite(fd, ab->large, strlen(ab->large));
		twrite(fd, "\n", 1);
	}
}
# endif

/* This function should be called before each char is inserted.  If the next
 * char is non-alphanumeric and we're at the end of a word, then that word
 * is checked against the abbrev[] array and expanded, if appropriate.  Upon
 * returning from this function, the new char still must be inserted.
 */
static MARK expandabbr(m, ch)
	MARK		m;	/* the cursor position */
	int		ch;	/* the character to insert */
{
	char		*word;	/* where the word starts */
	int		len;	/* length of the word */
	REG struct _AB	*ab;

	/* if no abbreviations are in effect, or ch is aphanumeric, then
	 * don't do anything
	 */
	if (!abbrev || !isascii(ch) || isalnum(ch))
	{
		return m;
	}

	/* see where the preceding word starts */
	pfetch(markline(m));
	for (word = ptext + markidx(m), len = 0;
	     --word >= ptext && (!isascii(*word) || isalnum(*word));
	     len++)
	{
	}
	word++;

	/* if zero-length, then it isn't a word, really -- so nothing */
	if (len == 0)
	{
		return m;
	}

	/* look it up in the abbrev list */
	for (ab = abbrev; ab; ab = ab->next)
	{
		if (!strncmp(ab->small, word, len) && !ab->small[len])
		{
			break;
		}
	}

	/* not an abbreviation? then do nothing */
	if (!ab)
	{
		return m;
	}

	/* else replace the small form with the large form */
	add(m, ab->large);
	delete(m - len, m);

	/* return with the cursor after the end of the large form */
	return m - len + strlen(ab->large);
}
#endif

		
/* This function allows the user to replace an existing (possibly zero-length)
 * chunk of text with typed-in text.  It returns the MARK of the last character
 * that the user typed in.
 */
MARK input(from, to, when)
	MARK	from;	/* where to start inserting text */
	MARK	to;	/* extent of text to delete */
	int	when;	/* either WHEN_VIINP or WHEN_VIREP */
{
	char	key[2];	/* key char followed by '\0' char */
	char	*build;	/* used in building a newline+indent string */
	char	*scan;	/* used while looking at the indent chars of a line */
	MARK	m;	/* some place in the text */
#ifndef NO_EXTENSIONS
	int	quit = FALSE;	/* boolean: are we exiting after this? */
#endif

#ifdef DEBUG
	/* if "from" and "to" are reversed, complain */
	if (from > to)
	{
		msg("ERROR: input(%ld:%d, %ld:%d)",
			markline(from), markidx(from),
			markline(to), markidx(to));
		return MARK_UNSET;
	}
#endif

	key[1] = 0;

	/* if we're replacing text with new text, save the old stuff */
	/* (Alas, there is no easy way to save text for replace mode) */
	if (from != to)
	{
		cut(from, to);
	}

	ChangeText
	{
		/* if doing a dot command, then reuse the previous text */
		if (doingdot)
		{
			/* delete the text that's there now */
			if (from != to)
			{
				delete(from, to);
			}

			/* insert the previous text */
			cutname('.');
			cursor = paste(from, FALSE, TRUE) + 1L;
		}
		else /* interactive version */
		{
			/* if doing a change within the line... */
			if (from != to && markline(from) == markline(to))
			{
				/* mark the end of the text with a "$" */
				change(to - 1, to, "$");
			}
			else
			{
				/* delete the old text right off */
				if (from != to)
				{
					delete(from, to);
				}
				to = from;
			}

			/* handle autoindent of the first line, maybe */
			cursor = from;
			if (*o_autoindent && markline(cursor) > 1L && markidx(cursor) == 0)
			{
				/* Only autoindent blank lines. */
				pfetch(markline(cursor));
				if (plen == 0)
				{
					/* Okay, we really want to autoindent */
					pfetch(markline(cursor) - 1L);
					for (scan = ptext, build = tmpblk.c;
					     *scan == ' ' || *scan == '\t';
					     )
					{
						*build++ = *scan++;
					}
					if (build > tmpblk.c)
					{
						*build = '\0';
						add(cursor, tmpblk.c);
						cursor += (build - tmpblk.c);
					}
				}
			}

			/* repeatedly add characters from the user */
			for (;;)
			{
				/* Get a character */
				redraw(cursor, TRUE);
#ifdef DEBUG
				msg("cursor=%ld.%d, to=%ld.%d",
					markline(cursor), markidx(cursor),
					markline(to), markidx(to));
#endif
				key[0] = getkey(when);

				/* if whitespace & wrapmargin is set & we're
				 * past the warpmargin, then change the
				 * whitespace character into a newline
				 */
				if ((*key == ' ' || *key == '\t')
				 && *o_wrapmargin != 0)
				{
					pfetch(markline(cursor));
					if (idx2col(cursor, ptext, TRUE) > COLS - (*o_wrapmargin & 0xff))
					{
						*key = '\n';
					}
				}

				/* process it */
				switch (*key)
				{
#ifndef NO_EXTENSIONS
				  case 0: /* special movement mapped keys */
					*key = getkey(0);
					switch (*key)
					{
					  case 'h':	m = m_left(cursor, 0L);		break;
					  case 'j':
					  case 'k':	m = m_updnto(cursor, 0L, *key);	break;
					  case 'l':	m = cursor + 1;			break;
					  case 'b':	m = m_bword(cursor, 0L);	break;
					  case 'w':	m = m_fword(cursor, 0L);	break;
					  case '^':	m = m_front(cursor, 0L);	break;
					  case '$':	m = m_rear(cursor, 0L);		break;
					  case ctrl('B'):
					  case ctrl('F'):
							m = m_scroll(cursor, 0L, *key); break;
					  case 'x':	m = v_xchar(cursor, 0L);	break;
					  case 'i':	m = to = from = cursor;		break;
					  default:	m = MARK_UNSET;			break;
					}
					/* adjust the moved cursor */
					m = adjmove(cursor, m, (*key == 'j' || *key == 'k' ? 0x20 : 0));
					if (*key == '$' || (*key == 'l' && m <= cursor))
					{
						m++;
					}
					/* if the cursor is reasonable, use it */
					if (m == MARK_UNSET)
					{
						beep();
					}
					else
					{
						if (to > cursor)
						{
							delete(cursor, to);
							redraw(cursor, TRUE);
						}
						from = to = cursor = m;
					}
					break;

				  case ctrl('Z'):
					if (getkey(0) == ctrl('Z'))
					{
						quit = TRUE;
						goto BreakBreak;
					}
					break;
#endif

				  case ctrl('['):
#ifndef NO_ABBR
					cursor = expandabbr(cursor, ctrl('['));
#endif
					goto BreakBreak;

				  case ctrl('U'):
					if (markline(cursor) == markline(from))
					{
						cursor = from;
					}
					else
					{
						cursor &= ~(BLKSIZE - 1);
					}
					break;

				  case ctrl('D'):
				  case ctrl('T'):
					if (to > cursor)
					{
						delete(cursor, to);
					}
					mark[27] = cursor;
					cmd_shift(cursor, cursor, *key == ctrl('D') ? CMD_SHIFTL : CMD_SHIFTR, TRUE, "");
					if (mark[27])
					{
						cursor = mark[27];
					}
					else
					{
						cursor = m_front(cursor, 0L);
					}
					to = cursor;
					break;

				  case '\b':
					if (cursor <= from)
					{
						beep();
					}
					else if (markidx(cursor) == 0)
					{
						cursor -= BLKSIZE;
						pfetch(markline(cursor));
						cursor += plen;
					}
					else
					{
						cursor--;
					}
					break;

				  case ctrl('W'):
					m = m_bword(cursor, 1L);
					if (markline(m) == markline(cursor) && m >= from)
					{
						cursor = m;
						if (from > cursor)
						{
							from = cursor;
						}
					}
					else
					{
						beep();
					}
					break;

				  case '\n':
#if OSK
				  case '\l':
#else				  
				  case '\r':
#endif
#ifndef NO_ABBR
					cursor = expandabbr(cursor, '\n');
#endif
					build = tmpblk.c;
					*build++ = '\n';
					if (*o_autoindent)
					{
						/* figure out indent for next line */
						pfetch(markline(cursor));
						for (scan = ptext; *scan == ' ' || *scan == '\t'; )
						{
							*build++ = *scan++;
						}

						/* remove indent from this line, if blank */
						if (!*scan && plen > 0)
						{
							to = cursor &= ~(BLKSIZE - 1);
							delete(cursor, cursor + plen);
						}
					}
					*build = 0;
					if (cursor >= to && when != WHEN_VIREP)
					{
						add(cursor, tmpblk.c);
					}
					else
					{
						change(cursor, to, tmpblk.c);
					}
					redraw(cursor, TRUE);
					to = cursor = (cursor & ~(BLKSIZE - 1))
							+ BLKSIZE
							+ (int)(build - tmpblk.c) - 1;
					break;

				  case ctrl('A'):
				  case ctrl('P'):
					if (cursor < to)
					{
						delete(cursor, to);
					}
					if (*key == ctrl('A'))
					{
						cutname('.');
					}
					to = cursor = paste(cursor, FALSE, TRUE) + 1L;
					break;

				  case ctrl('V'):
					if (cursor >= to && when != WHEN_VIREP)
					{
						add(cursor, "^");
					}
					else
					{
						change(cursor, to, "^");
						to = cursor + 1;
					}
					redraw(cursor, TRUE);
					*key = getkey(0);
					if (*key == '\n')
					{
						/* '\n' too hard to handle */
#if OSK
						*key = '\l';
#else
						*key = '\r';
#endif
					}
					change(cursor, cursor + 1, key);
					cursor++;
					if (cursor > to)
					{
						to = cursor;
					}
					break;

				  case ctrl('L'):
				  case ctrl('R'):
					redraw(MARK_UNSET, FALSE);
					break;

				  default:
					if (cursor >= to && when != WHEN_VIREP)
					{
#ifndef NO_ABBR
						cursor = expandabbr(cursor, *key);
#endif
						add(cursor, key);
						cursor++;
						to = cursor;
					}
					else
					{
						pfetch(markline(cursor));
						if (markidx(cursor) == plen)
						{
#ifndef NO_ABBR
							cursor = expandabbr(cursor, *key);
#endif
							add(cursor, key);
						}
						else
						{
#ifndef NO_DIGRAPH
							*key = digraph(ptext[markidx(cursor)], *key);
#endif
#ifndef NO_ABBR
							cursor = expandabbr(cursor, *key);
#endif
							change(cursor, cursor + 1, key);
						}
						cursor++;
					}
#ifndef NO_SHOWMATCH
					/* show matching "({[" if neceesary */
					if (*o_showmatch && strchr(")}]", *key))
					{
						redraw(cursor, TRUE);
						m = m_match(cursor - 1, 0L);
						if (markline(m) >= topline
						 && markline(m) <= botline)
						{
							redraw(m, TRUE);
							refresh();
							sleep(1);
						}
					}
#endif
				} /* end switch(*key) */
			} /* end for(;;) */
BreakBreak:;

			/* delete any excess characters */
			if (cursor < to)
			{
				delete(cursor, to);
			}

		} /* end if doingdot else */

	} /* end ChangeText */

	/* put the new text into a cut buffer for possible reuse */
	if (!doingdot)
	{
		blksync();
		cutname('.');
		cut(from, cursor);
	}

	/* move to last char that we inputted, unless it was newline */
	if (markidx(cursor) != 0)
	{
		cursor--;
	}
	redraw(cursor, FALSE);

#ifndef NO_EXTENSIONS
	if (quit)
	{
		/* if this is a nested "do", then cut it short */
		abortdo();

		/* exit, unless we can't write out the file */
		cursor = v_xit(cursor, 0L, 'Z');
	}
#endif

	rptlines = 0L;
	return cursor;
}
