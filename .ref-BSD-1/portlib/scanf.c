# include	"iodec.h"

/*
**	formatted read routine
**
**		based on scanf() in the portable C library
*/


int	(*__getc)(), (*__ungc)();
int	__input;

scanf(paramlist)
char	*paramlist;
{
	extern		cgetc(), ungetc();
	extern		__gets(), __ungs();
	register char	**p;
	extern int	cin;
	register int	nmatch;
	char		*format;
	register char	ch;
	char		match_ch;

	p = &paramlist;
	__getc = cgetc;
	__ungc = ungetc;
	__input = cin;
	if (paramlist == -1)
	{
		p++;
		__getc = __gets;
		__ungc = __ungs;
		__input = p++;
	}
	else
		if (paramlist >= 0 && paramlist < MAXFILES)
		{
			__input = *p++;
		}
	nmatch = 0;
	format = *p++;
	while (1)
	{
		switch (ch = *format++)
		{

		  case '\0':
			return (nmatch);

		  case '%':
			if (*format != '%')
			{
				switch (__scan(&format, *p))
				{
	
				  case 1:	/* good return */
					p++;
					nmatch++;
				  case 0:	/* no return */
					break;
	
				  case -1:	/* no match */
					return (nmatch);
	
				  case -2:	/* end of file */
					return (nmatch ? nmatch : -1);
	
				  default:	/* syntax error */
					return (-1);
	
				}
				break;
			}
			format++;

		  default:
			match_ch = __next(0);
			if (ch != match_ch)
			{
				(*__ungc)(match_ch, __input);
				return (nmatch ? nmatch : -2);
			}
			break;

		}
	}
}


__scan(spec, result)
char	**spec;
char	*result;
{
	register int	longf, length;
	register char	ch;
	extern int	__strend(), __splend();

	longf = length = 0;
	while (1)
	{
		switch (ch = *(*spec)++)
		{
		  case '*':
			result = 0;
			break;
	
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
			length = length * 10 + ch - '0';
			break;
	
		  case 'l':
			if (longf)
				return (-3);
			longf = 1;
			break;
	
		  case 'h':	/* short */
			if (longf)
				return (-3);
			longf = -1;
			break;
	
		  case 'o':
		  case 'O':
			return (__dec(result, length ? length : 100, 8, longf));
	
		  case 'd':
		  case 'D':
			return (__dec(result, length ? length : 100, 10, longf));
	
		  case 'x':
		  case 'X':
			return (__dec(result, length ? length : 100, 16, longf));
	
		  case 'c':
		  case 'C':
			if (longf)
				return (-3);
			return (__char(result, length ? length : 1));
	
		  case 's':
		  case 'S':
			if (longf)
				return (-3);
			return (__str(result, length ? length : 100, __strend));
	
		  case 'e':
		  case 'E':
		  case 'f':
		  case 'F':
			if (longf < 0)
				return (-3);
			return (__float(result, length ? length : 100, longf));
	
		  case '[':
			if (longf)
				return (-3);
			if (__inits(spec))
				return (-3);
			return (__str(result, length ? length : 100, __splend));
	
		  default:
			return (-3);
	
		}
	}
}


__dec(result, length, base, longf)
int	*result;
int	length;
int	base;
int	longf;
{
	register char	ch;
	long		n, *lresult;
	register int	val;
	long		lres;
	int		ires;
	int		minus, ok;
	register int	ndigit;

	ires = 0;
	lres = 0;
	ndigit = minus = 0;
	switch (ch = __next(1))
	{

	  case '\0':
		return (-2);

	  case '-':
		minus = 1;
	  case '+':
		ndigit++;
		ch = __next(-1);
	}
	ok = 0;
	while ((val = __digit(ch, base)) >= 0 && ndigit++ < length)
	{
		ok++;
		if (longf)
			lres = lres * base + val;
		else
			ires = ires * base + val;
		ch = __next(0);
	}
	(*__ungc)(ch, __input);
	if (!ok)
		return (-1);
	if (!result)
		return (0);
	if (minus)
		if (longf)
			lres = -lres;
		else
			ires = -ires;
	if (longf)
	{
		lresult = result;
		*lresult = lres;
	}
	else
		*result = ires;
	return (1);
}


__next(mode)
int	mode;
{
/*
 *	mode -1:  get next non-space or non-tab
 *	mode 0:   get next character
 *	mode 1:   get next non-space, non-tab, or non-newline
 */
	register int	ch;

	ch = (*__getc)(__input);
	if (mode == 0)
		return (ch);
	while (ch == ' ' || ch == '\t' || ch == '\n')
	{
		if (ch == '\n' && mode < 0)
			break;
		ch = (*__getc)(__input);
	}
	return (ch);
}


__digit(ch, base)
char	ch;
int	base;
{
	register int	n;

	if (ch < '0')
		return (-1);
	if (ch <= '7')
		return (ch - '0');
	if (base == 8)
		return (-1);
	if (ch <= '9')
		return (ch - '0');
	if (base == 10 || ch < 'A')
		return (-1);
	if (ch <= 'F')
		return (ch - 'A' + 10);
	if (ch < 'a' || ch > 'f')
		return (-1);
	return (ch - 'a' + 10);
}


__fltend(cha)
char	cha;
{
	register char	ch;
	static char	gote, gotpt, gotsn;

	ch = cha;
	if (ch < 0)
		return (gote = gotpt = gotsn = 0);
	if (ch >= '0' && ch <= '9')
	{
		gotsn = -1;
		return (0);
	}
	if (ch == '+' || ch == '-')
	{
		if (gotsn || gotpt)
			return (1);
		gotsn = 1;
		return (0);
	}
	if (ch == '.')
	{
		if (gote || gotpt)
			return (1);
		gotpt++;
		return (0);
	}
	if (ch != 'e' && ch != 'E')
		return (1);
	if (gote)
		return (1);
	gote++;
	gotsn = 0;
	return (0);
}


__strend(cha)
char	cha;
{
	register char	ch;

	ch = cha;
	if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\0')
		return (1);
	return (0);
}


char	__splset[128];

__splend(ch)
char	ch;
{
	return (__splset[ch]);
}


__inits(spec)
char	**spec;
{
	register char	ch;
	register int	i;
	register int	val;

	ch = *(*spec)++;
	if (ch == '^')
	{
		val = 0;
		ch = *(*spec)++;
	}
	else
		val = 1;
	for (i = 1; i < 128; i++)
		__splset[i] = val;
	val = 1 - val;
	while (ch != ']')
	{
		if (ch == 0)
			return (-1);
		__splset[ch & 0177] = val;
		ch = *(*spec)++;
	}
	__splset[0] = 1;
	return (0);
}


__float(result, length, longf)
double	*result;
int	length;
int	longf;
{
	extern int	__fltend();
	char		temp[101];
	float		*fres;
	register int	r;
	double		x;
	extern double	atof();

	__fltend(-1);
	r = __str(temp, length, __fltend);
	if (r < 0)
		return (r);
	if (!result)
		return (0);
	x = atof(temp);
	if (longf)
	{
		*result = x;
		return (1);
	}
	fres = result;
	*fres = x;
	return (1);
}


__str(result, length, endfn)
char	*result;
int	length;
int	(*endfn)();
{
	register char	ch;
	extern int	__splend();
	register int	imode, notok;

	notok = 1;
	imode = (endfn != __splend);
	while (!(*endfn)(ch = __next(imode)) && length-- > 0)
	{
		if (result)
			*result++ = ch;
		imode = notok = 0;
	}
	(*__ungc)(ch, __input);
	if (notok)
		return (ch ? -1 : -2);
	if (!result)
		return (0);
	*result = 0;
	return (1);
}


__char(result, length)
char	*result;
int	length;
{
	register char	*r, ch;
	register int	l;

	r = result;
	l = length;

	while (l--)
	{
		if ((ch = __next(0)) <= 0)
			if (l + 1 == length)
				return (ch ? -1 : -2);
			else
				return (result != 0);
		if (result)
			*result++ = ch;
	}
	return (result != 0);
}


__gets(s)
char	**s;
{
	register char	c;

	c = **s;
	if (c)
		(*s)++;
	return (c);
}


__ungs(c, s)
char	c;
char	**s;
{
	if (c)
		(*s)--;
}
