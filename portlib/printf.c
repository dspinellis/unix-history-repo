# include	"iodec.h"
# define	BUFSIZ		80

/**
 **	formated print
 **/

printf(parlist)
char	*parlist;
{
	register char	*fmt, c;
	char		buf[BUFSIZ];
	extern char	*__prtshort(), *__prtld(), *__prtld();
	double		*dblptr;
	int		mode;
	char		*fd;
	register char 	**p;
	int		width, prec;
	int		left, longf;
	char		padchar;
	char		*s;
	int		n;
	auto		(*fn)();
	int		len;

	p = &parlist;
	fd = 0;
	mode = 0;		/* mode zero, putchar */
	if (parlist + 1 < MAXFILES + 1)
	{
		mode++;		/* mode one, cputc */
		fd = *p++;
	}
	if (fd == -1)
	{
		mode++;		/* mode two, string */
		fd = *p++;
	}
	fmt = *p++;

	while (c = *fmt++)
	{
		if (c != '%')
		{
			__putch(mode, &fd, c);
			continue;
		}
		left = 0;
		if ((c = *fmt++) == '-')
		{
			c = *fmt++;
			left++;
		}
		padchar = ' ';
		if (c == '0')
		{
			padchar = c;
			c = *fmt++;
		}
		width = -1;
		while (c >= '0' && c <= '9')
		{
			if (width < 0)
				width = 0;
			width = width * 10 + (c - '0');
			c = *fmt++;
		}
		prec = -1;
		if (c == '.')
		{
			prec = 0;
			c = *fmt++;
		}
		while (c >= '0' && c <= '9')
		{
			prec = prec * 10 + (c - '0');
			c = *fmt++;
		}
		longf = 0;
		if (c == 'l')
		{
			longf++;
			c = *fmt++;
		}
		/* we now have all the prelims out of the way;
		   let's see what we want to print */

		s = buf;
		switch (c)
		{

		  case 'd':		/* decimal signed */
		  case 'D':
			if (longf)
				fn = __prtld;
			else
				fn = __prtshort;
			__prtint(p++, buf, 10, 1, fn, 0);
			if (longf)
				p++;
			break;

		  case 'u':		/* decimal unsigned */
		  case 'U':
			__prtint(p++, buf, 10, 0, __prtshort, 0);
			break;

		  case 'o':		/* octal unsigned */
		  case 'O':
			__prtint(p++, buf, 8, 0, __prtshort, 0);
			break;

		  case 'x':		/* hexadecimal unsigned */
		  case 'X':
			__prtint(p++, buf, 16, 0, __prtshort, c == 'X');
			break;

		  case 's':		/* string */
		  case 'S':
			s = *p++;
			break;

		  case 'c':		/* character */
		  case 'C':
			n = *p++;
			buf[0] = n;
			buf[1] = '\0';
			break;

		  case 'e':		/* exponential */
		  case 'E':
		  case 'f':		/* floating */
		  case 'F':
		  case 'g':		/* e or f */
		  case 'G':
		  case 'n':
		  case 'N':
			dblptr = p;
			if (prec < 0)
				prec = 7;
			ftoa(*dblptr, buf, sizeof buf - 1, prec, c);
			while (*s == ' ')
				s++;
			p =+ 4;
			prec = -1;
			break;

		  default:		/* just print the character */
			__putch(mode, &fd, c);
			continue;

		}
		len = __length(s);
		if (prec < len && prec >= 0)
			len = prec;
		n = width - len;
		if (!left)
		{
			if (padchar != ' ' && *s == '-')
			{
				len--;
				__putch(mode, &fd, *s++);
			}
			while (n-- > 0)
				__putch(mode, &fd, padchar);
		}
		while (len--)
			__putch(mode, &fd, *s++);
		while (n-- > 0)
			__putch(mode, &fd, padchar);
	}
	if (mode == 2)
		*fd = '\0';
}


__putch(mode, pfd, c)
int	mode;
char	c;
char	**pfd;
{
	switch (mode)
	{

	  case 0:
		putchar(c);
		break;

	  case 1:
		cputc(c, *pfd);
		break;

	  case 2:
		*(*pfd)++ = c;
		break;

	}
	return (c);
}


char *__prtld(pobj, pbuf, base, signed, digs)
long	*pobj;
char	**pbuf;
int	base;
int	signed;
char	*digs;
{
	long		n;
	register char	*p;

	p = digs;
	n = *pobj;
	if (signed && n < 0)
	{
		*(*pbuf)++ = '-';
		n = -n;
	}
	for (; n != 0; n =/ base)
		*p++ = n % base;
	return (p);
}
