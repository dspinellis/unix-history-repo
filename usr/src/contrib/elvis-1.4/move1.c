/* move1.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains most movement functions */

#include "config.h"
#include <ctype.h>
#include "vi.h"

#ifndef isascii
# define isascii(c)	!((c) & ~0x7f)
#endif

MARK	m_updnto(m, cnt, cmd)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	DEFAULT(cmd == 'G' ? nlines : 1L);

	/* move up or down 'cnt' lines */
	switch (cmd)
	{
	  case ('P'&0x1f):
	  case '-':
	  case 'k':
		m -= MARK_AT_LINE(cnt);
		break;

	  case 'G':
		if (cnt < 1L || cnt > nlines)
		{
			msg("Only %ld lines", nlines);
			return MARK_UNSET;
		}
		m = MARK_AT_LINE(cnt);
		break;

	  default:
		m += MARK_AT_LINE(cnt);
	}

	/* if that left us screwed up, then fail */
	if (m < MARK_FIRST || markline(m) > nlines)
	{
		return MARK_UNSET;
	}

	return m;
}

/*ARGSUSED*/
MARK	m_right(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	int		idx;	/* index of the new cursor position */

	DEFAULT(1);

	/* move to right, if that's OK */
	pfetch(markline(m));
	idx = markidx(m) + cnt;
	if (idx < plen)
	{
		m += cnt;
	}
	else
	{
		return MARK_UNSET;
	}

	return m;
}

/*ARGSUSED*/
MARK	m_left(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	DEFAULT(1);

	/* move to the left, if that's OK */
	if (markidx(m) >= cnt)
	{
		m -= cnt;
	}
	else
	{
		return MARK_UNSET;
	}

	return m;
}

/*ARGSUSED*/
MARK	m_tocol(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	char	*text;	/* text of the line */
	int	col;	/* column number */
	int	idx;	/* index into the line */

	DEFAULT(1);

	/* internally, columns are numbered 0..COLS-1, not 1..COLS */
	cnt--;

	/* if 0, that's easy */
	if (cnt == 0)
	{
		m &= ~(BLKSIZE - 1);
		return m;
	}

	/* find that column within the line */
	pfetch(markline(m));
	text = ptext;
	for (col = idx = 0; col < cnt && *text; text++, idx++)
	{
		if (*text == '\t' && !*o_list)
		{
			col += *o_tabstop;
			col -= col % *o_tabstop;
		}
		else if (UCHAR(*text) < ' ' || *text == '\177')
		{
			col += 2;
		}
#ifndef NO_CHARATTR
		else if (text[0] == '\\' && text[1] == 'f' && text[2] && *o_charattr)
		{
			text += 2; /* plus one more as part of for loop */
		}
#endif
		else
		{
			col++;
		}
	}
	if (!*text)
	{
		return MARK_UNSET;
	}
	else
	{
		m = (m & ~(BLKSIZE - 1)) + idx;
	}
	return m;
}

/*ARGSUSED*/
MARK	m_front(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument (ignored) */
{
	char	*scan;

	/* move to the first non-whitespace character */
	pfetch(markline(m));
	scan = ptext;
	m &= ~(BLKSIZE - 1);
	while (*scan == ' ' || *scan == '\t')
	{
		scan++;
		m++;
	}

	return m;
}

/*ARGSUSED*/
MARK	m_rear(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument (ignored) */
{
	/* Try to move *EXTREMELY* far to the right.  It is fervently hoped
	 * that other code will convert this to a more reasonable MARK before
	 * anything tries to actually use it.  (See adjmove() in vi.c)
	 */
	return m | (BLKSIZE - 1);
}

#ifndef NO_SENTENCE
/*ARGSUSED*/
MARK	m_fsentence(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	REG char	*text;
	REG long	l;

	DEFAULT(1);

	/* get the current line */
	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);

	/* for each requested sentence... */
	while (cnt-- > 0)
	{
		/* search forward for one of [.?!] followed by spaces or EOL */
		do
		{
			/* wrap at end of line */
			if (!text[0])
			{
				if (l >= nlines)
				{
					return MARK_UNSET;
				}
				l++;
				pfetch(l);
				text = ptext;
			}
			else
			{
				text++;
			}
		} while (text[0] != '.' && text[0] != '?' && text[0] != '!'
			|| text[1] && (text[1] != ' ' || text[2] && text[2] != ' '));
	}

	/* construct a mark for this location */
	m = buildmark(text);

	/* move forward to the first word of the next sentence */
	m = m_fword(m, 1L);

	return m;
}

/*ARGSUSED*/
MARK	m_bsentence(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	REG char	*text;	/* used to scan thru text */
	REG long	l;	/* current line number */
	int		flag;	/* have we passed at least one word? */

	DEFAULT(1);

	/* get the current line */
	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);

	/* for each requested sentence... */
	flag = TRUE;
	while (cnt-- > 0)
	{
		/* search backward for one of [.?!] followed by spaces or EOL */
		do
		{
			/* wrap at beginning of line */
			if (text == ptext)
			{
				do
				{
					if (l <= 1)
					{
						return MARK_UNSET;
					}
					l--;
					pfetch(l);
				} while (!*ptext);
				text = ptext + plen - 1;
			}
			else
			{
				text--;
			}

			/* are we moving past a "word"? */
			if (text[0] >= '0')
			{
				flag = FALSE;
			}
		} while (flag || text[0] != '.' && text[0] != '?' && text[0] != '!'
			|| text[1] && (text[1] != ' ' || text[2] && text[2] != ' '));
	}

	/* construct a mark for this location */
	m = buildmark(text);

	/* move to the front of the following sentence */
	m = m_fword(m, 1L);

	return m;
}
#endif

/*ARGSUSED*/
MARK	m_fparagraph(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	char	*text;
	char	*pscn;	/* used to scan thru value of "paragraphs" option */
	long	l;

	DEFAULT(1);

	for (l = markline(m); cnt > 0 && l++ < nlines; )
	{
		text = fetchline(l);
		if (!*text)
		{
			cnt--;
		}
#ifndef NO_SENTENCE
		else if (*text == '.')
		{
			for (pscn = o_paragraphs; pscn[0] && pscn[1]; pscn += 2)
			{
				if (pscn[0] == text[1] && pscn[1] == text[2])
				{
					cnt--;
					break;
				}
			}
		}
#endif
	}
	if (l <= nlines)
	{
		m = MARK_AT_LINE(l);
	}
	else
	{
		m = MARK_LAST;
	}
	return m;
}

/*ARGSUSED*/
MARK	m_bparagraph(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
{
	char	*text;
	char	*pscn;	/* used to scan thru value of "paragraph" option */
	long	l;

	DEFAULT(1);

	for (l = markline(m); cnt > 0 && l-- > 1; )
	{
		text = fetchline(l);
		if (!*text)
		{
			cnt--;
		}
#ifndef NO_SENTENCE
		else if (*text == '.')
		{
			for (pscn = o_paragraphs; pscn[0] && pscn[1]; pscn += 2)
			{
				if (pscn[0] == text[1] && pscn[1] == text[2])
				{
					cnt--;
					break;
				}
			}
		}
#endif
	}
	if (l >= 1)
	{
		m = MARK_AT_LINE(l);
	}
	else
	{
		m = MARK_FIRST;
	}
	return m;
}

/*ARGSUSED*/
MARK	m_fsection(m, cnt, key)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* (ignored) */
	int	key;	/* second key stroke - must be ']' */
{
	char	*text;
	char	*sscn;	/* used to scan thru value of "sections" option */
	long	l;

	/* make sure second key was ']' */
	if (key != ']')
	{
		return MARK_UNSET;
	}

	for (l = markline(m); l++ < nlines; )
	{
		text = fetchline(l);
		if (*text == '{')
		{
			break;
		}
#ifndef NO_SENTENCE
		else if (*text == '.')
		{
			for (sscn = o_sections; sscn[0] && sscn[1]; sscn += 2)
			{
				if (sscn[0] == text[1] && sscn[1] == text[2])
				{
					goto BreakBreak;
				}
			}
		}
#endif
	}
BreakBreak:
	if (l <= nlines)
	{
		m = MARK_AT_LINE(l);
	}
	else
	{
		m = MARK_LAST;
	}
	return m;
}

/*ARGSUSED*/
MARK	m_bsection(m, cnt, key)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* (ignored) */
	int	key;	/* second key stroke - must be '[' */
{
	char	*text;
	char	*sscn;	/* used to scan thru value of "sections" option */
	long	l;

	/* make sure second key was '[' */
	if (key != '[')
	{
		return MARK_UNSET;
	}

	for (l = markline(m); l-- > 1; )
	{
		text = fetchline(l);
		if (*text == '{')
		{
			break;
		}
#ifndef NO_SENTENCE
		else if (*text == '.')
		{
			for (sscn = o_sections; sscn[0] && sscn[1]; sscn += 2)
			{
				if (sscn[0] == text[1] && sscn[1] == text[2])
				{
					goto BreakBreak;
				}
			}
		}
#endif
	}
BreakBreak:
	if (l >= 1)
	{
		m = MARK_AT_LINE(l);
	}
	else
	{
		m = MARK_FIRST;
	}
	return m;
}


/*ARGSUSED*/
MARK	m_match(m, cnt)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument (ignored) */
{
	long	l;
	REG char	*text;
	REG char	match;
	REG char	nest;
	REG int		count;

	/* get the current line */
	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);

	/* search forward within line for one of "[](){}" */
	for (match = '\0'; !match && *text; text++)
	{
		/* tricky way to recognize 'em in ASCII */
		nest = *text;
		if ((nest & 0xdf) == ']' || (nest & 0xdf) == '[')
		{
			match = nest ^ ('[' ^ ']');
		}
		else if ((nest & 0xfe) == '(')
		{
			match = nest ^ ('(' ^ ')');
		}
		else
		{
			match = 0;
		}
	}
	if (!match)
	{
		return MARK_UNSET;
	}
	text--;

	/* search forward or backward for match */
	if (match == '(' || match == '[' || match == '{')
	{
		/* search backward */
		for (count = 1; count > 0; )
		{
			/* wrap at beginning of line */
			if (text == ptext)
			{
				do
				{
					if (l <= 1L)
					{
						return MARK_UNSET;
					}
					l--;
					pfetch(l);
				} while (!*ptext);
				text = ptext + plen - 1;
			}
			else
			{
				text--;
			}

			/* check the char */
			if (*text == match)
				count--;
			else if (*text == nest)
				count++;
		}
	}
	else
	{
		/* search forward */
		for (count = 1; count > 0; )
		{
			/* wrap at end of line */
			if (!*text)
			{
				if (l >= nlines)
				{
					return MARK_UNSET;
				}
				l++;
				pfetch(l);
				text = ptext;
			}
			else
			{
				text++;
			}

			/* check the char */
			if (*text == match)
				count--;
			else if (*text == nest)
				count++;
		}
	}

	/* construct a mark for this place */
	m = buildmark(text);
	return m;
}

/*ARGSUSED*/
MARK	m_tomark(m, cnt, key)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* (ignored) */
	int	key;	/* keystroke - the mark to move to */
{
	/* mark '' is a special case */
	if (key == '\'' || key == '`')
	{
		if (mark[26] == MARK_UNSET)
		{
			return MARK_FIRST;
		}
		else
		{
			return mark[26];
		}
	}

	/* if not a valid mark number, don't move */
	if (key < 'a' || key > 'z')
	{
		return MARK_UNSET;
	}

	/* return the selected mark -- may be MARK_UNSET */
	if (!mark[key - 'a'])
	{
		msg("mark '%c is unset", key);
	}
	return mark[key - 'a'];
}

