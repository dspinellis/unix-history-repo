# include	"iodec.h"

/**
 **	output an error message and exit
 **/

__error(parlist)
char	*parlist;
{
	register char	*fmt;
	register char	**p;
	char		buf[10], *s;
	register int	c;
	extern char	*__prtshort();
	extern int	cout;

	p = &parlist;
	fmt = *p++;

	while (c = *fmt++)
	{
		if (c != '%')
		{
			cputc(c, cout);
			continue;
		}
		c = *fmt++;
		switch (c)
		{

		  case 'd':
			s = buf;
			__prtint(p++, buf, 10, 1, __prtshort, 0);
			break;

		  case 's':
			s = *p++;
			break;

		}
		while (*s)
			cputc(*s++, cout);
	}
	cputc('\n', cout);

	flush();
	abort();
}
