#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)atol.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

long
atol(p)
register char *p;
{
	long n;
	register int f;

	n = 0;
	f = 0;
	for(;;p++) {
		switch(*p) {
		case ' ':
		case '\t':
			continue;
		case '-':
			f++;
		case '+':
			p++;
		}
		break;
	}
	while(*p >= '0' && *p <= '9')
		n = n*10 + *p++ - '0';
	return(f? -n: n);
}
