# include	<ingres.h>
# include	<aux.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	<sccs.h>

SCCSID(@(#)string.c	7.1	2/5/81)

/*
**	This file contains the string
**	manipulation routines
*/






/*
**	Concat takes the two character strings in
**	s1 and s2 and concatenates them together
**	into a new location.
**
**	trailing blanks are removed from the first symbol.
**	The size of the concatenation equals the sum of
**	the two original strings.
*/

concatsym(s1, s2)
register SYMBOL	*s1, *s2;
{
	register char	*p;
	int		size1, size2, i;
	char		*px;
	extern char	*need();

	size1 = size(s1);	/* compute size w/o trailing blanks */
	if (size1 == 0 && s1->len != 0)
		size1++;	/* don't allow string to be trunc to zero length */
	size2 = s2->len & 0377;	/* size of second string remains the same */
	i = (s1->len & 0377) + size2;	/* i equals sum of original sizes */
	if (i > 255)
		i = 255;	/* a string can't exceed this size */
	if (size2 + size1 > 255)
		size2 = 255 - size1;	/* adjust size2 to not exceed 255 */

	px = p = need(De.ov_ovqpbuf, i);	/* request the needed space */
	bmove(s1->value.sym_data.cptype, p, size1);	/* copy first string */
	p = &p[size1];
	bmove(s2->value.sym_data.cptype, p, size2);
	p = &p[size2];
	s1->value.sym_data.cptype = px;
	s1->len = i;
	/* pad with blanks if necessary */
	i -= size1 - size2;
	while (i--)
		*p++ = ' ';

#	ifdef xOTR1
	if (tTf(82, 1))
	{
		printf("Concat:");
		prstack(s1);
	}
#	endif
}
/*
**	Size determines the size of a character symbol
**	without trailing blanks.
*/

size(s)
register SYMBOL	*s;
{
	register char		*c;
	register int		i;

	c = s->value.sym_data.cptype;
	i = s->len & 0377;

	for (c += i; i; i--)
		if(*--c != ' ')
			break;

	return (i);
}
/*
**	Converts the numeric symbol to
**	ascii. Formats to be used are determined
**	by Out_arg.
*/

ascii(s)
register SYMBOL	*s;
{
	register int		i;
	register char		*p;
	char			temp[MAXFIELD];
	extern struct out_arg	Out_arg;	/* used for float conversions */
	char			*locv();

	p = temp;
	switch(s->type)
	{

	  case INT:
		if (s->len == 4)
		{
			i = Out_arg.i4width;
			p = locv(s->value.sym_data.i4type);
		}
		else
		{
			itoa(s->value.sym_data.i2type, p);
			if (s->len == 2)
				i = Out_arg.i2width;
			else
				i = Out_arg.i1width;
		}
		break;

	  case CHAR:
		return;

	  case FLOAT:
		if (s->len == 4)
		{
			i = Out_arg.f4width;
			ftoa(s->value.sym_data.f8type, p, i, Out_arg.f4prec, Out_arg.f4style);
		}
		else
		{
			i = Out_arg.f8width;
			ftoa(s->value.sym_data.f8type, p, i, Out_arg.f8prec, Out_arg.f8style);
		}
	}
	s->value.sym_data.cptype = need(De.ov_ovqpbuf, i);
	pmove(p, s->value.sym_data.cptype, i, ' ');	/* blank pad to fixed length i */
	s->type = CHAR;
	s->len = i;
}
/*
**	LEXCOMP performs character comparisons between the two
**	strings ss1 and ss2. All blanks and null are ignored in
**	both strings. In addition pattern matching is performed
**	using the "shell syntax". Pattern matching characters
**	are converted to the pattern matching symbols PAT_ANY etc.
**	by the scanner.
**
**	Pattern matching characters can appear in either or
**	both strings. Since they cannot be stored in relations,
**	pattern matching chars in both strings can only happen
**	if the user types in such an expression.
**
**	examples:
**
**	"Smith, Homer" = "Smith,Homer"
**
**	"abcd" < "abcdd"
**
**	"abcd" = "aPAT_ANYd"
**
**	returns	<0 if s1 < s2
**		 0 if s1 = s2
**		>0 if s1 > s2
*/

lexcomp(s1, l1, s2, l2)
register char	*s1, *s2;
register int	l1, l2;
{
	char		c1, c2;

loop:
	while (l1--)
	{
		switch (c1 = *s1++)
		{

		  case ' ':
		  case '\0':
			break;

		  case PAT_ANY:
			return (pmatch(s1, l1, s2, l2));

		  case PAT_LBRAC:
			return (lmatch(s1, l1, s2, l2));

		  default:
			while (l2--)
			{
				switch (c2 = *s2++)
				{

				  case ' ':
				  case '\0':
					continue;

				  case PAT_ANY:
					return (pmatch(s2, l2, --s1, ++l1));

				  case PAT_LBRAC:
					return (lmatch(s2, l2, --s1, ++l1));

				  default:
					if (c1 == c2)
						goto loop;
					if (c1 == PAT_ONE || c2 == PAT_ONE)
						goto loop;
					return (c1 - c2);
				}
			}
			return (1);	/* s1 > s2 */
		}
	}

	/* examine remainder of s2 for any characters */
	while (l2--)
		if ((c1 = *s2++) != ' ' && (c1 != '\0') && (c1 != PAT_ANY))
			return (-1);	/* s1 < s2 */
	return (0);
}


pmatch(pat, plen, str, slength)
char	*pat;		/* the string holding the pattern matching char */
char	*str;		/* the string to be checked */
int	plen, slength;	/* the lengths */
{
	register char	d, *s;
	register int	slen;
	char		c;

	s = str;
	slen = slength;

	if (plen == 0)
		return  (0);	/* a match if no more chars in p */

	/*
	** If the next character in "pat" is not another
	** pattern matching character, then scan until
	** first matching char and continue comparison.
	*/
	if ((c = *pat) != PAT_ANY && c != PAT_LBRAC && c != PAT_ONE)
	{
		while (slen--)
		{
			if ((d = *s) == c || d == PAT_ANY || d == PAT_LBRAC && d != PAT_ONE)
			{
				if (lexcomp(pat, plen, s, slen + 1) == 0)
					return (0);
			}
			s++;
		}
	}
	else
	{
		while (slen)
			if (lexcomp(pat, plen, s++, slen--) == 0)
				return (0);	/* match */
	}
	return (-1);	/* no match */
}

lmatch(pat, plen, str, slen)
char	*pat;	/* the string holding the pattern matching char */
char	*str;	/* the other string */
int	plen, slen;	/* their respective sizes */
{
	register char	*p, *s;
	register int	cc;
	int		oldc, c, found;

	p = pat;
	s = str;

	/* find a non-blank, non-null char in s */
	while (slen--)
	{
		if ((c = *s++) != ' ' && c != '\0')
		{
			/* search for a match on 'c' */
			found = 0;	/* assume failure */
			oldc = 0777;	/* make previous char large */

			while (plen--)
			{

				switch(cc = *p++)
				{

				  case PAT_RBRAC:
					if (found)
						return (lexcomp(p, plen, s, slen));
					return (-1);

				  case '-':
					if (plen-- == 0)
						return (-1);	/* not found */
					if (oldc <= c && c <= (cc = *p++))
						found++;
					break;

				  default:
					if (c == (oldc = cc))
						found++;
				}
			}
			return (-1);	/* no match */
		}
	}
	return (1);
}
