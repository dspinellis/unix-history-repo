jumpc(tree, lbl, cond)
int tree[]; {
	extern jump, cctab[], rcexpr, isn, label, branch, cbranch;
	int l1, l2;

	if (tree==0)
		return;
	switch(*tree) {

	/* & */
	case 47:
		if (cond) {
			cbranch(tree[3], l1=isn++, 0, 0);
			cbranch(tree[4], l1, 0, 0);
			jump(lbl);
			label(l1);
		} else {
			cbranch(tree[3], l1=isn++, 0, 0);
			cbranch(tree[4], l2=isn++, 1, 0);
			label(l1);
			jump(lbl);
			label(l2);
		}
		return;

	/* | */
	case 48:
		if (cond) {
			cbranch(tree[3], l1=isn++, 1, 0);
			cbranch(tree[4], l2=isn++, 0, 0);
			label(l1);
			jump(lbl);
			label(l2);
		} else {
			cbranch(tree[3], l1=isn++, 1, 0);
			cbranch(tree[4], l1, 1, 0);
			jump(lbl);
			label(l1);
		}
		return;

	/* ! */
	case 34:
		jumpc(tree[3], lbl, !cond);
		return;
	}
	rcexpr(tree, cctab, 0);
	branch(l1=isn++, *tree, cond);
	jump(lbl);
	label(l1);
	return;
}

cbranch(tree, lbl, cond, reg)
int tree[]; {
	extern branch, cctab[], rcexpr, isn, label;
	int l1;

	if (tree==0)
		return;
	switch(*tree) {

	/* & */
	case 47:
		if (cond) {
			cbranch(tree[3], l1=isn++, 0, reg);
			cbranch(tree[4], lbl, 1, reg);
			label(l1);
		} else {
			cbranch(tree[3], lbl, 0, reg);
			cbranch(tree[4], lbl, 0, reg);
		}
		return;

	/* | */
	case 48:
		if (cond) {
			cbranch(tree[3], lbl, 1, reg);
			cbranch(tree[4], lbl, 1, reg);
		} else {
			cbranch(tree[3], l1=isn++, 1, reg);
			cbranch(tree[4], lbl, 0, reg);
			label(l1);
		}
		return;

	/* ! */
	case 34:
		cbranch(tree[3], lbl, !cond, reg);
		return;
	}
	rcexpr(tree, cctab, reg);
	branch(lbl, *tree, !cond);
	return;
}


branch(lbl, op, c) {
	extern printf, prins, opdope[];

	if(op) {
		if((opdope[op]&04)==0)
			op = 61;
		prins(op,c);
	} else
		printf("br");
	printf("\tl%d\n", lbl);
}

jump(lab) {
	extern printf;

	printf("jmp\tl%d\n", lab);
}

label(l) {
	extern printf;

	printf("l%d:", l);
}


popstk(a) {
	extern printf;

	switch(a) {

	case 0:
		return;

	case 2:
		printf("tst	(sp)+\n");
		return;

	case 4:
		printf("cmp	(sp)+,(sp)+\n");
		return;
	}
	printf("add	$%o,sp\n", a);
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

error(s, p1, p2) {
	extern printf, line, fout, flush, putchar, nerror;
	int f;

	nerror++;
	flush();
	f = fout;
	fout = 1;
	printf("%d: ", line);
	printf(s, p1, p2);
	putchar('\n');
	fout = f;
}

