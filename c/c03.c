retseq()
{
	printf("jmp	retrn\n");
}

decref(t)
{

	if ((t & 077770) == 0) {
		error("Illegal indirection");
		return(t);
	}
	return((t>>2) & 077770 | t&07);
}

incref(t)
{
	return((t<<2)&077740 | (t&07) | 010);
}

jumpc(tree, lbl, cond)
int tree[];
{
	extern cctab, block, rcexpr;

	rcexpr(block(1,easystmt()+103,tree,lbl,cond),cctab);
}

rcexpr(tree, table)
int tree[], table;
{
	extern space, putwrd, putchar, line;
	int c, sp[];

	if (tree == 0)
		return;
	putchar('#');
	c = space;
	c =/ 2;		/* # addresses per word */
	sp = 0;
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

	printf("L%d:", l);
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

plength(p)
int p[];
{
	int t, l;

	if (((t=p[1])&077770) == 0)		/* not a reference */
		return(1);
	p[1] = decref(t);
	l = length(p);
	p[1] = t;
	return(l);
}

length(cs)
int cs[];
{
	extern hshtab[];
	int t;

	t = cs[1];
	if ((t&030) == 030)		/* array */
		t = decref(t);
	if (t>=010)
		return(2);
	switch(t&07) {

	case 0:		/* int */
		return(2);

	case 1:		/* char */
		return(1);

	case 2:		/* float */
		return(4);

	case 3:		/* double */
		return(8);

	case 4:		/* structure */
		if (cs>=hshtab)			/* in namelist */
			return(cs[3]);
		return(getlen(cs));

	case 5:
		error("Bad structure");
		return(0);
	}
	error("Compiler error (length)");
}

getlen(p)
int p[];
{
	int p1[];

	switch(*p) {

	case 20:		/* name */
		return(p[2]);

	case 35:
	case 29:		/* & */
	case 36:		/* * */
	case 100:		/* call */
	case 41:		/* - */
		return(getlen(p[3]));

	case 40:		/* + */
		p1 = p[4];
		if ((p1[1]&07) == 04)
			return(getlen(p1));
		return(getlen(p[3]));
	}
	error("Unimplemented pointer conversion");
	return(0);
}

rlength(cs)
int cs[];
{
	auto l;

	if (((l=length(cs))&01) != 0)
		l++;
	return(l);
}

tlength(cs)
int cs[];
{
	int nel;

	if ((nel = cs[8]) == 0)
		nel = 1;
	return(length(cs)*nel);
}

trlength(cs)
int cs[];
{
	int l;

	if (((l=tlength(cs))&01) != 0)
		l++;
	return(l);
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
			if (*s)
				putchar((*s++)&0177);
		goto loop;
	}
	putchar('%');
	fmt--;
	adx--;
	goto loop;
}

