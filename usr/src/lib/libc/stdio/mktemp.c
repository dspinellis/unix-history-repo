#ifndef lint
static char sccsid[] = "@(#)mktemp.c	5.1 (Berkeley) %G%";
#endif not lint

char *
mktemp(as)
char *as;
{
	register char *s;
	register unsigned pid;
	register i;

	pid = getpid();
	s = as;
	while (*s++)
		;
	s--;
	while (*--s == 'X') {
		*s = (pid%10) + '0';
		pid /= 10;
	}
	s++;
	i = 'a';
	while (access(as, 0) != -1) {
		if (i=='z')
			return("/");
		*s = i++;
	}
	return(as);
}
