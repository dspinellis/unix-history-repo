main() {
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

	extern putchar,getc,peekc,printf,flag,flush;

	auto c,snlflg,nlflg,t,smode,m,ssmode;

	smode = nlflg = snlflg = ssmode = 0;
loop:
	c = getc();
	if (c!='\n' & c!='\t') nlflg = 0;
	if (ssmode!=0 & c!='%') {
		ssmode = 0;
		printf(".data\n1:<");
	}
	switch(c) {

	case '\0':
		printf(".text; 0\n");
		flush();
		return;

	case ':':
		if (!smode)
			printf("=.+2; 0"); else
			putchar(':');
		goto loop;

	case 'A':
		if ((c=getc())=='1' | c=='2') {
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

	case 'M':
		putchar('N');
		snlflg++;
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
loop1:
		switch (c=getc()) {

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
			t = 0;
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

		case '\n':
			printf(";1f\n");
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
	}
	putchar(c);
	goto loop;
}

getc() {
	extern getchar, peekc, nofloat;
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
	if (ifcnt & nofloat)
		goto gc;
	return(t);
}

flag() {
	extern getc, peekc;
	auto c, f;

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

	case 'p':
		f =+ 16;
		goto l1;
	}
	peekc = c;
	return(f);
}

peekc 0;

putchar(c) {
	extern flush, oubuf, ouptr;
	char ouptr[], oubuf[];
	auto c1;

	goto init;
init:
	ouptr = oubuf;
	init = init1;
init1:
	if(c1 = c>>8) {
		*ouptr++ = c1;
		if(ouptr >= oubuf+512)
			flush();
	}
	if(c =& 0377) {
			*ouptr++ = c;
		if(ouptr >= oubuf+512)
			flush();
	}
}

flush() {
	extern ouptr, oubuf, fout, write;
	char ouptr[], oubuf[];

	write(fout, oubuf, ouptr-oubuf);
	ouptr = oubuf;
}

getcha() {
	extern read, incnt, fin, inbuf, inptr;
	char inbuf[], inptr[];

	goto init;
init:
	inptr = inbuf;
	init = init1;
init1:
	if(inptr >= inbuf+incnt) {
		inptr = inbuf;
		incnt = read(fin, inbuf, 512);
		if(!incnt)
			return('\0');
	}
	return(*inptr++);
}

inbuf[256];
oubuf[256];
inptr 0;
incnt 0;
ouptr 0;
fin 0;
fout 1;
nofloat 0;
