/* regsub.c */

/* This file contains the regsub() function, which performs substitutions
 * after a regexp match has been found.
 */

#include <ctype.h>
#include "config.h"
#include "vi.h"
#include "regexp.h"

static char *previous;	/* a copy of the text from the previous substitution */

/* perform substitutions after a regexp match */
void regsub(re, src, dst)
	regexp		*re;
	REG char	*src;
	REG char	*dst;
{
	REG char	*cpy;
	REG char	*end;
	REG char	c;
	char		*start;
#ifndef CRUNCH
	int		mod;

	mod = 0;
#endif

	start = src;
	while ((c = *src++) != '\0')
	{
#ifndef NO_MAGIC
		/* recognize any meta characters */
		if (c == '&' && *o_magic)
		{
			cpy = re->startp[0];
			end = re->endp[0];
		}
		else if (c == '~' && *o_magic)
		{
			cpy = previous;
			if (cpy)
				end = cpy + strlen(cpy);
		}
		else
#endif /* not NO_MAGIC */
		if (c == '\\')
		{
			c = *src++;
			switch (c)
			{
#ifndef NO_MAGIC
			  case '0':
			  case '1':
			  case '2':
			  case '3':
			  case '4':
			  case '5':
			  case '6':
			  case '7':
			  case '8':
			  case '9':
				/* \0 thru \9 mean "copy subexpression" */
				c -= '0';
				cpy = re->startp[c];
				end = re->endp[c];
				break;
# ifndef CRUNCH
			  case 'U':
			  case 'u':
			  case 'L':
			  case 'l':
				/* \U and \L mean "convert to upper/lowercase" */
				mod = c;
				continue;

			  case 'E':
			  case 'e':
				/* \E ends the \U or \L */
				mod = 0;
				continue;
# endif /* not CRUNCH */
			  case '&':
				/* "\&" means "original text" */
				if (*o_magic)
				{
					*dst++ = c;
					continue;
				}
				cpy = re->startp[0];
				end = re->endp[0];
				break;

			  case '~':
				/* "\~" means "previous text, if any" */
				if (*o_magic)
				{
					*dst++ = c;
					continue;
				}
				cpy = previous;
				if (cpy)
					end = cpy + strlen(cpy);
				break;
#else /* NO_MAGIC */
			  case '&':
				/* "\&" means "original text" */
				cpy = re->startp[0];
				end = re->endp[0];
				break;

			  case '~':
				/* "\~" means "previous text, if any" */
				cpy = previous;
				if (cpy)
					end = cpy + strlen(cpy);
				break;
#endif /* NO_MAGIC */
			  default:
				/* ordinary char preceded by backslash */
				*dst++ = c;
				continue;
			}
		}
		else
		{
			/* ordinary character, so just copy it */
			*dst++ = c;
			continue;
		}

		/* Note: to reach this point in the code, we must have evaded
		 * all "continue" statements.  To do that, we must have hit
		 * a metacharacter that involves copying.
		 */

		/* if there is nothing to copy, loop */
		if (!cpy)
			continue;

		/* copy over a portion of the original */
		while (cpy < end)
		{
#ifndef NO_MAGIC
# ifndef CRUNCH
			switch (mod)
			{
			  case 'U':
			  case 'u':
				/* convert to uppercase */
				if (isascii(*cpy) && islower(*cpy))
				{
					*dst++ = toupper(*cpy);
					cpy++;
				}
				else
				{
					*dst++ = *cpy++;
				}
				break;

			  case 'L':
			  case 'l':
				/* convert to lowercase */
				if (isascii(*cpy) && isupper(*cpy))
				{
					*dst++ = tolower(*cpy);
					cpy++;
				}
				else
				{
					*dst++ = *cpy++;
				}
				break;

			  default:
				/* copy without any conversion */
				*dst++ = *cpy++;
			}

			/* \u and \l end automatically after the first char */
			if (mod && (mod == 'u' || mod == 'l'))
			{
				mod = 0;
			}
# else /* CRUNCH */
			*dst++ = *cpy++;
# endif /* CRUNCH */
#else /* NO_MAGIC */
			*dst++ = *cpy++;
#endif /* NO_MAGIC */
		}
	}
	*dst = '\0';

	/* remember what text we inserted this time */
	if (previous)
		free(previous);
	previous = (char *)malloc((unsigned)(strlen(start) + 1));
	if (previous)
		strcpy(previous, start);
}
