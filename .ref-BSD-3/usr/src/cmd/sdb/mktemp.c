/* make temporary file with given prefix. */
char *
mktemp(as)
char *as;
{
	register char *s;
	register unsigned pid;

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
	*s = 'a';
	return(as);
}
