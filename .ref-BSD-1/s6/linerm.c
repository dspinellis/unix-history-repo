/*
	linerm - remove lines from a file
	linerm [+n -n +n ...] <std input >std output

	Jeff Schriebman 8-75
*/
extern fout;
struct numb {
	int line;
	int sign;
} lineb[50];

main(argc, argv)
char **argv;
{
	register struct numb *nptr;
	register char c;
	register int sw;
	int n, lineon, i;
	char *aptr;
	static pbuf[259];

	fout = dup(1);
	nptr = lineb;
	n = 0;
	while (--argc) {
		n++;
		aptr = *++argv;
		if (*aptr == '-') {
			aptr++;
			nptr->sign = 0;
		} else
			nptr->sign = 1;
		if (*aptr == '+')
			aptr++;
		nptr->line = numcv(aptr);
		nptr++;
	}
	nptr->line = 0;
	nptr->sign = -1;
	nptr = lineb;
	for (i=0; i<n-1; i++) {
		lineon = nptr->line;
		if (lineon >= (++nptr)->line) {
			printf("Illegal column sequence\n");
			exit();
		}
	}
	lineon = 1;
	sw = 1;
	nptr = lineb;
	for (;;nptr++) {
		if (nptr->sign == -1 && sw == 0)
			flush(), exit();
		while (nptr->line != lineon) {
			while ((c=getc(pbuf)) != '\n') {
				if (c <= 0)
					flush(), exit();
				if (sw)
					putchar(c);
			}
			if (sw)
				putchar(c);
			lineon++;
		}
		sw = nptr->sign;
	}
	flush();
}

numcv(ptr)
char *ptr;
{
	register char *aptr, c;
	register int num;

	aptr = ptr;
	num = 0;
	while ((c = *aptr++) != '\0') {
		if (c<'0' || c>'9') {
			printf("Illegal number %s\n", ptr);
			exit();
		}
		num = num*10 + c - '0';
	}
	return(num);
}
