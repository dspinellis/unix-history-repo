#ifndef lint
static char sccsid[] = "@(#)atol.c	5.1 (Berkeley) %G%";
#endif not lint

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
