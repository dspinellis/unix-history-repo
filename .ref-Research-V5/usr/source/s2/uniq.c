fields 0;
letters 0;
linec;
mode;
uniq;

main(argc, argv)
int argc;
char *argv[];
{
	extern fin, fout;
	int f;
	static char b1[1000], b2[1000];

loop:
	if(argc > 1) {
		if(*argv[1] == '-') {
			if(argv[1][1] <= '9')
				fields = conv(&argv[1][1]);
			else mode = argv[1][1];
			argc--;
			argv++;
			goto loop;
		}
		if(*argv[1] == '+') {
			letters = conv(&argv[1][1]);
			argc--;
			argv++;
			goto loop;
		}
		f = open(argv[1], 0);
		if(f < 0) {
			printf("cannot open %s\n", argv[1]);
			exit();
		}
		fin = f;
	} else
		fin = dup(0);
	if(argc > 2) {
		f = creat(argv[2], 0666);
		if(f < 0) {
			printf("cannot create %s\n", argv[2]);
			exit();
		}
		fout = f;
	} else
		fout = dup(1);
	close(0);
	close(1);
	gline(b1);
l1:
	linec++;
	if(gline(b2)) {
		pline(b1);
		flush();
		exit();
	}
	if(equal(b1, b2)) goto l1;
	pline(b1);
	linec = 0;
l2:
	linec++;
	if(gline(b1)) {
		pline(b2);
		flush();
		exit();
	}
	if(equal(b1, b2)) goto l2;
	pline(b2);
	linec = 0;
	goto l1;
}

gline(buf)
char buf[];
{
	int c;

	while((c = getchar()) != '\n') {
		if(c == '\0')
			return(1);
		*buf++ = c;
	}
	*buf = 0;
	return(0);
}

pline(buf)
char buf[];
{
	int c;

	switch(mode) {

	case 'u':
		if(uniq) {;
			uniq = 0;
			return;
		}
		break;

	case 'd':
		if(uniq) break;
		return;

	case 'c':
		printf("%4d ", linec);
	}
	uniq = 0;
	while((c = *buf++) != 0)
		putchar(c);
	putchar('\n');
}

equal(b1, b2)
char b1[], b2[];
{
	int c;

	b1 = skip(b1);
	b2 = skip(b2);
	while((c = *b1++) != 0)
		if(c != *b2++) return(0);
	if(*b2 != 0)
		return(0);
	uniq++;
	return(1);
}

char *
skip(s)
char *s;
{
	int nf, nl;

	nf = nl = 0;
	while(nf++ < fields) {
		while(*s==' ' || *s=='\t')
			s++;
		while( !(*s==' ' || *s=='\t') ) 
			if(*s == 0) return(s);
			else s++;
	}
	while(nl++ < letters) 
			if(*s == 0) return(s);
			else s++;
	return(s);
}

conv(s)
char *s;
{
	int d, n;

	n = 0;
	for(;;) {
		d = *s++ - '0';
		if(0>d || d>9) break;
		n = n*10 + d;
	}
	return(n);
}
