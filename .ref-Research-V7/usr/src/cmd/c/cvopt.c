#include <stdio.h>

int	tabflg;
int	labno	= 1;
FILE	*curbuf;
FILE	*obuf;

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

	int c, snlflg, nlflg, t, smode, m, ssmode, peekc;

	smode = nlflg = snlflg = ssmode = 0;
	if (argc>1)
		if (freopen(argv[1], "r", stdin) == NULL) {
			fprintf(stderr, "%s?\n", argv[1]);
			return(1);
		}
	if (argc>2) 
		if (freopen(argv[2], "w", stdout) == NULL) {
			fprintf(stderr, "%s?\n", argv[2]);
			return(1);
		}
	if ((obuf = fopen("cvopt.tmp", "w")) == NULL) {
		fprintf(stderr, "cvopt.tmp?\n");
		exit(1);
	}
	curbuf = obuf;
loop:
	c = getchar();
	if (c!='\n' && c!='\t')
		nlflg = 0;
	if (ssmode!=0 && c!='%') {
		ssmode = 0;
		curbuf = stdout;
		fprintf(curbuf, "L%d:<", labno++);
	}
	switch(c) {

	case EOF:
		fprintf(obuf, "0\n");
		fclose(obuf);
		fprintf(stdout, ".even\n");
		if (freopen("cvopt.tmp", "r", stdin) == NULL) {
			fprintf(stderr, "tmp?\n");
			exit(1);
		}
		while ((c = getchar()) != EOF)
			putchar(c);
		unlink("cvopt.tmp");
		return(0);

	case ':':
		if (!smode)
			fprintf(curbuf, "=.+2; 0"); else
			put(':');
		goto loop;

	case 'A':
		if ((c=getchar())=='1' || c=='2') {
			put(c+'A'-'1');
			goto loop;
		}
		put('O');
		ungetc(c, stdin);
		goto loop;

	case 'B':
		switch (getchar()) {

		case '1':
			put('C');
			goto loop;

		case '2':
			put('D');
			goto loop;

		case 'E':
			put('L');
			goto loop;

		case 'F':
			put('P');
			goto loop;
		}
		put('?');
		goto loop;

	case 'C':
		put(getchar()+'E'-'1');
		goto loop;

	case 'F':
		put('G');
		goto subtre;

	case 'R':
		if ((c=getchar()) == '1')
		put('J'); else {
			put('I');
			ungetc(c, stdin);
		}
		goto loop;

	case 'H':
		put('H');
		goto subtre;

	case 'I':
		put('M');
		goto loop;

	case 'S':
		put('K');
subtre:
		snlflg = 1;
		t = 'A';
l1:
		switch (c=getchar()) {

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
		ungetc(c, stdin);
		put(t);
		goto loop;

	case '#':
		if(getchar()=='1')
			put('#'); else
			put('"');
		goto loop;

	case '%':
		if (smode)
			curbuf = obuf;
		if (ssmode==0) {
			if ((peekc=getchar())=='[') {
				curbuf = stdout;
				while((c=getchar())!=']')
					put(c);
				getchar();
				fprintf(curbuf, ";");
				curbuf = obuf;
				goto loop;
			}
			ungetc(peekc, stdin);
		}
loop1:
		switch (c=getchar()) {

		case ' ':
		case '\t':
			goto loop1;
		case 'a':
			m = 16;
			t = flag();
			goto pf;

		case ',':
			put(';');
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
			if ((c=getchar())=='*')
				m =+ 0100; else
				ungetc(c, stdin);
			fprintf(curbuf, ".byte %o,%o", m, t);
			goto loop1;
		case '[':
			fprintf(curbuf, "L%d=", labno++);
			while ((c=getchar())!=']')
				put(c);
			ssmode = 0;
			smode = 0;
			goto loop;

		case '\n':
			fprintf(curbuf, "\nL%d\n", labno);
			ssmode = 1;
			nlflg = 1;
			smode = 1;
			goto loop;
		}
		put(c);
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
		put('\t');
		goto loop;

	case '\n':
		if (!smode)  {
			put('\n');
			goto loop;
		}
		if (nlflg) {
			nlflg = 0;
			fprintf(curbuf, "\\0>\n");
			curbuf = obuf;
			smode = 0;
			goto loop;
		}
		if (!snlflg)
			fprintf(curbuf, "\\n");
		snlflg = 0;
		fprintf(curbuf, ">\n<");
		nlflg = 1;
		goto loop;

	case 'X':
	case 'Y':
	case 'T':
		snlflg++;
	}
	put(c);
	goto loop;
}

flag() {
	register c, f;

	f = 0;
l1:
	switch(c=getchar()) {

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

	case 'u':
		f = 9;
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
	ungetc(c, stdin);
	return(f);
}

put(c)
{
	if (tabflg) {
		tabflg = 0;
		fprintf(curbuf, ">;.byte %o;<", c+0200);
	} else
		putc(c, curbuf);
}
