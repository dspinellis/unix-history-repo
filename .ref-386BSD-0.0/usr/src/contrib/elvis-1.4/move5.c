/* move5.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains the word-oriented movement functions */

#include <ctype.h>
#include "config.h"
#include "vi.h"

#ifndef isascii
# define isascii(c)	!((c) & ~0x7f)
#endif


MARK	m_fword(m, cnt, cmd)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
	int	cmd;	/* either 'w' or 'W' */
{
	REG long	l;
	REG char	*text;
	REG int		i;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		i = *text++;

		if (cmd == 'W')
		{
			/* include any non-whitespace */
			while (i && (!isascii(i) || !isspace(i)))
			{
				i = *text++;
			}
		}
		else if (!isascii(i) || isalnum(i) || i == '_')
		{
			/* include an alphanumeric word */
			while (i && (!isascii(i) || isalnum(i) || i == '_'))
			{
				i = *text++;
			}
		}
		else
		{
			/* include contiguous punctuation */
			while (i && isascii(i) && !isalnum(i) && !isspace(i))
			{
				i = *text++;
			}
		}

		/* include trailing whitespace */
		while (!i || isascii(i) && isspace(i))
		{
			/* did we hit the end of this line? */
			if (!i)
			{
				/* move to next line, if there is one */
				l++;
				if (l > nlines)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext;
			}

			i = *text++;
		}
		text--;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}


MARK	m_bword(m, cnt, cmd)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
	int	cmd;	/* either 'b' or 'B' */
{
	REG long	l;
	REG char	*text;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		text--;

		/* include preceding whitespace */
		while (text < ptext || isascii(*text) && isspace(*text))
		{
			/* did we hit the end of this line? */
			if (text < ptext)
			{
				/* move to preceding line, if there is one */
				l--;
				if (l <= 0)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext + plen - 1;
			}
			else
			{
				text--;
			}
		}

		if (cmd == 'B')
		{
			/* include any non-whitespace */
			while (text >= ptext && (!isascii(*text) || !isspace(*text)))
			{
				text--;
			}
		}
		else if (!isascii(*text) || isalnum(*text) || *text == '_')
		{
			/* include an alphanumeric word */
			while (text >= ptext && (!isascii(*text) || isalnum(*text) || *text == '_'))
			{
				text--;
			}
		}
		else
		{
			/* include contiguous punctuation */
			while (text >= ptext && isascii(*text) && !isalnum(*text) && !isspace(*text))
			{
				text--;
			}
		}
		text++;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}

MARK	m_eword(m, cnt, cmd)
	MARK	m;	/* movement is relative to this mark */
	long	cnt;	/* a numeric argument */
	int	cmd;	/* either 'e' or 'E' */
{
	REG long	l;
	REG char	*text;
	REG int		i;

	DEFAULT(1);

	l = markline(m);
	pfetch(l);
	text = ptext + markidx(m);
	while (cnt-- > 0) /* yes, ASSIGNMENT! */
	{
		text++;
		i = *text++;

		/* include preceding whitespace */
		while (!i || isascii(i) && isspace(i))
		{
			/* did we hit the end of this line? */
			if (!i)
			{
				/* move to next line, if there is one */
				l++;
				if (l > nlines)
				{
					return MARK_UNSET;
				}
				pfetch(l);
				text = ptext;
			}

			i = *text++;
		}

		if (cmd == 'E')
		{
			/* include any non-whitespace */
			while (i && (!isascii(i) || !isspace(i)))
			{
				i = *text++;
			}
		}
		else if (!isascii(i) || isalnum(i) || i == '_')
		{
			/* include an alphanumeric word */
			while (i && (!isascii(i) || isalnum(i) || i == '_'))
			{
				i = *text++;
			}
		}
		else
		{
			/* include contiguous punctuation */
			while (i && isascii(i) && !isalnum(i) && !isspace(i))
			{
				i = *text++;
			}
		}
		text -= 2;
	}

	/* construct a MARK for this place */
	m = buildmark(text);
	return m;
}
