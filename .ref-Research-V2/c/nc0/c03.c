jumpc(tree, lbl, cond)
int tree[];
{
	extern cctab, block, rcexpr;

	rcexpr(block(1,easystmt()+103,tree,lbl,cond),cctab);
}

rcexpr(tree, table)
int tree[], table;
{
	extern space, ospace, putwrd, putchar, line;
	int c, sp[];

	putchar('#');
	c = space-ospace;
	c =/ 2;		/* # addresses per word */
	sp = ospace;

	putwrd(c);
	putwrd(tree);
	putwrd(table);
	putwrd(line);
	while(c--)
		putwrd(*sp++);
}

jump(lab) {
	extern printf;

	printf("jmp\tl%d\n", lab);
}

label(l) {
	extern printf;

	printf("l%d:", l);
}

retseq() {
	extern printf;

	printf("jmp\tretrn\n");
}

slabel() {
	extern csym[], printf;

	printf(".data; l%d: 1f; .text; 1:\n", csym[2]);
}

setstk(a) {
	extern printf, stack;
	auto ts;

	ts = a-stack;
	stack = a;
	switch(ts) {

	case 0:
		return;

	case 0177776:	/* -2 */
		printf("tst	-(sp)\n");
		return;

	case 0177774:	/* -4 */
		printf("cmp	-(sp),-(sp)\n");
		return;
	}
	printf("add	$%o,sp\n", ts);
}

defvec() {
	extern printf, stack;

	printf("mov\tsp,r0\nmov\tr0,-(sp)\n");
	stack =- 2;
}

defstat(s)
int s[]; {
	extern printf, length;
	int len;

	len = length(s[1]);
	if (s[3])
		printf(".data; l%d:1f; .bss; 1:.=.+%o; .even; .text\n", s[2],
			s[3]*len);
	else
		printf(".bss; l%d:.=.+%o; .even; .text\n", s[2], len);
}

length(t) {

	if (t<0)
		t =+ 020;
	if (t>=020)
		return(2);
	switch(t) {

	case 0:
		return(2);

	case 1:
		return(1);

	case 2:
		return(4);

	case 3:
		return(8);

	case 4:
		return(4);

	}
	return(1024);
}

rlength(c) {
	extern length;
	auto l;

	return((l=length(c))==1? 2: l);
}

printn(n,b) {
	extern putchar;
	auto a;

	if(a=n/b) /* assignment, not test for equality */
		printn(a, b); /* recursive */
	putchar(n%b + '0');
}

printf(fmt,x1,x2,x3,x4,x5,x6,x7,x8,x9)
char fmt[]; {
	extern printn, putchar, namsiz, ncpw;
	char s[];
	auto adx[], x, c, i[];

	adx = &x1; /* argument pointer */
loop:
	while((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		putchar(c);
	}
	x = *adx++;
	switch (c = *fmt++) {

	case 'd': /* decimal */
	case 'o': /* octal */
		if(x < 0) {
			x = -x;
			if(x<0)  {	/* - infinity */
				if(c=='o')
					printf("100000");
				else
					printf("-32767");
				goto loop;
			}
			putchar('-');
		}
		printn(x, c=='o'?8:10);
		goto loop;

	case 's': /* string */
		s = x;
		while(c = *s++)
			putchar(c);
		goto loop;

	case 'p':
		s = x;
		putchar('_');
		c = namsiz;
		while(c--)
			if(*s)
				putchar(*s++);
		goto loop;
	}
	putchar('%');
	fmt--;
	adx--;
	goto loop;
}

