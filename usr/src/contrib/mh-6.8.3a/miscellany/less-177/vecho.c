/*
 * This dumb little program emulates the System V "echo" command,
 * to accommodate BSD systems which don't understand the \c escape,
 * meaning don't echo a newline.  BSD uses "echo -n".
 */

#include <stdio.h>

int putnl;

main(argc, argv)
	int argc;
	char *argv[];
{
	putnl = 1;
	while (--argc > 0)
	{
		vecho(*++argv);
		if (argc > 1)
			putchar(' ');
	}
	if (putnl)
		putchar('\n');
	exit(0);
}

vecho(str)
	char *str;
{
	register char *s;

	for (s = str;  *s != '\0';  s++)
	{
		if (*s == '\\' && s[1] == 'c')
		{
			putnl = 0;
			return;
		}
		putchar(*s);
	}
}
