printn(n,b) {
	extern putchar;
	auto a;

	if(a=n/b) /* assignment, not test for equality */
		printn(a, b); /* recursive */
	putchar(n%b + '0');
}

printf(fmt,x1,x2,x3,x4,x5,x6,x7,x8,x9)
	char fmt[];
	{
	extern printn, putchar;
	char s[];
	auto adx[], x, c;

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
			if(x<0) {  	/* is - infinity */
				if(c=='o')
					printf("100000");
				else
					printf("-32768");
				goto loop;
			}
			putchar('-');
		}
		printn(x, c=='o'?8:10);
		goto loop;

	case 'c': /* char */
		putchar(x);
		goto loop;

	case 's': /* string */
		s = x;
		while(c = *s++)
			putchar(c);
		goto loop;
	}
	putchar('%');
	fmt--;
	adx--;
	goto loop;
}

