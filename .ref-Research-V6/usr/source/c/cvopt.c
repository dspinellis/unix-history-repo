int	nofloat;
int	peekc;
int	obuf[259];
int	tabflg;
int	labno	1;

main(argc, argv)
char **argv;
{
/*
	A1 -> A
	A2    B
	A     O
	B1    C
	B2    D
	BE    L
	BF    P
	C1    E
	C2    F
	F     G
	H     H
	R     I
	R1    J
	S     K
	I     M
	M     N

		*	+1
		S	+2
		C	+4
		1	+8

	z  -> 4
	c     10
	a     14
	e     20
	n     63
	*	+0100
*/

	auto c,snlflg,nlflg,t,smode,m,ssmode;
	extern fin;

	smode = nlflg = snlflg = ssmode = 0;
	if (argc>1)
		if ((fin = open(argv[1], 0)) < 0) {
			putchar('?\n');
			return;
		}
	obuf[0] = 1;
	if (argc>2) 
		if ((obuf[0] = creat(argv[2], 0666)) < 0) {
			putchar('?\n');
			return;
		}
loop:
	c = getc();
	if (c!='\n' && c!='\t') nlflg = 0;
	if (ssmode!=0 && c!='%') {
		ssmode = 0;
		printf(".data\nL%d:<", labno++);
	}
	switch(c) {

	case '\0':
		printf(".text; 0\n");
		fflush(obuf);
		return;

	case ':':
		if (!smode)
			printf("=.+2; 0"); else
			putchar(':');
		goto loop;

	case 'A':
		if ((c=getc())=='1' || c=='2') {
			putchar(c+'A'-'1');
			goto loop;
		}
		putchar('O');
		peekc = c;
		goto loop;

	case 'B':
		switch (getc()) {

		case '1':
			putchar('C');
			goto loop;

		case '2':
			putchar('D');
			goto loop;

		case 'E':
			putchar('L');
			goto loop;

		case 'F':
			putchar('P');
			goto loop;
		}
		putchar('?');
		goto loop;

	case 'C':
		putchar(getc()+'E'-'1');
		goto loop;

	case 'F':
		putchar('G');
		goto subtre;

	case 'R':
		if ((c=getc()) == '1')
		putchar('J'); else {
			putchar('I');
			peekc = c;
		}
		goto loop;

	case 'H':
		putchar('H');
		goto subtre;

	case 'I':
		putchar('M');
		goto loop;

	case 'S':
		putchar('K');
subtre:
		snlflg = 1;
		t = 'A';
l1:
		switch (c=getc()) {

		case '*':
			t++;
			goto l1;

		case 'S':
			t =+ 2;
			goto l1;

		case 'C':
			t =+ 4;
			goto l1;

		case '1':
			t =+ 8;
			goto l1;

		case '2':
			t =+ 16;
			goto l1;
		}
		peekc = c;
		putchar(t);
		goto loop;

	case '#':
		if(getc()=='1')
			putchar('#'); else
			putchar('"');
		goto loop;

	case '%':
		if (smode)
			printf(".text;");
		if (ssmode==0) {
			if ((peekc=getc())=='[') {
				peekc = 0;
				printf(".data;");
				while((c=getc())!=']')
					putchar(c);
				getc();
				printf(";.text;");
				goto loop;
			}
		}
loop1:
		switch (c=getc()) {

		case ' ':
		case '\t':
			goto loop1;
		case 'a':
			m = 16;
			t = flag();
			goto pf;

		case ',':
			putchar(';');
			goto loop1;

		case 'i':
			m = 12;
			t = flag();
			goto pf;
		case 'z':
			m = 4;
			t = flag();
			goto pf;

		case 'r':
			m = 9;
			t = flag();
			goto pf;

		case '1':
			m = 5;
			t = flag();
			goto pf;

		case 'c':
			t = 0;
			m = 8;
			goto pf;

		case 'e':
			t = flag();
			m = 20;
			goto pf;

		case 'n':
			t = flag();
			m = 63;
pf:
			if ((c=getc())=='*')
				m =+ 0100; else
				peekc = c;
			printf(".byte %o,%o", m, t);
			goto loop1;
		case '[':
			printf("L%d=", labno++);
			while ((c=getc())!=']')
				putchar(c);
			ssmode = 0;
			smode = 0;
			goto loop;

		case '\n':
			printf("\nL%d\n", labno);
			ssmode = 1;
			nlflg = 1;
			smode = 1;
			goto loop;
		}
		putchar(c);
		goto loop1;

	case '\t':
		if (nlflg) {
			nlflg = 0;
			goto loop;
		}
		if (smode) {
			tabflg++;
			goto loop;
		}
		putchar('\t');
		goto loop;

	case '\n':
		if (!smode)  {
			putchar('\n');
			goto loop;
		}
		if (nlflg) {
			nlflg = 0;
			printf("\\0>\n.text\n");
			smode = 0;
			goto loop;
		}
		if (!snlflg)
			printf("\\n");
		snlflg = 0;
		printf(">\n<");
		nlflg = 1;
		goto loop;

	case 'X':
	case 'Y':
	case 'T':
		snlflg++;
	}
	putchar(c);
	goto loop;
}

getc() {
	auto t, ifcnt;

	ifcnt = 0;
gc:
	if (peekc) {
		t = peekc;
		peekc = 0;
	} else
		t = getchar();
	if (t==0)
		return(0);
	if (t=='{') {
		ifcnt++;
		t = getchar();
	}
	if (t=='}') {
		t = getc();
		if (--ifcnt==0)
			if (t=='\n')
				t = getc();
	}
	if (ifcnt && nofloat)
		goto gc;
	return(t);
}

flag() {
	register c, f;

	f = 0;
l1:
	switch(c=getc()) {

	case 'w':
		f = 1;
		goto l1;

	case 'i':
		f = 2;
		goto l1;

	case 'b':
		f = 3;
		goto l1;

	case 'f':
		f = 4;
		goto l1;

	case 'd':
		f = 5;
		goto l1;

	case 's':
		f = 6;
		goto l1;

	case 'l':
		f = 8;
		goto l1;

	case 'p':
		f =+ 16;
		goto l1;
	}
	peekc = c;
	return(f);
}

putchar(c)
{
	if (tabflg) {
		tabflg = 0;
		printf(">;.byte %o;<", c+0200);
	} else
		putc(c, obuf);
}
