int	word[16];
char	gbuf[512];
int	fi;
int	conv;
int	base	010;
int	basem	01000;
int	max;
int	gidx;
int	gcnt;
int	eof;
int	addr[2];
int	from[2];
int	key;
int	flag;
int	nword	8;

main(argc, argv)
char **argv;
{
	extern fout;
	int f, k, w, i, a[2];
	char *p;

	fout = dup(1);
	argv++;
	if(argc > 1) {
		p = *argv;
		if(*p == '-') {
			while((i = *p++) != '\0') {
				switch(i) {
				case 'o':
					conv =| 001;
					f = 6;
					break;
				case 'd':
					conv =| 002;
					f = 5;
					break;
				case 'a':
					conv =| 004;
					f = 4;
					break;
				case 'h':
					conv =| 010;
					f = 4;
					break;
				case 'c':
					conv =| 020;
					f = 5;
					break;
				case 'b':
					conv =| 040;
					f = 7;
					break;
				}
				if(f > max)
					max = f;
			}
			argc--;
			argv++;
		}
	}
	if(!conv) {
		max = 6;
		conv = 1;
	}
	if(argc > 1)
	if(**argv != '+') {
		fi = open(*argv, 0);
		if(fi < 0) {
			printf("cannot open %s\n", *argv);
			goto done;
		}
		argv++;
		argc--;
	}
	if(argc > 1)
		offset(*argv);

loop:
	f = 1;
	a[0] = addr[0];
	a[1] = addr[1];
	for(i=0; i<nword; i++) {
		w = getw();
		if(eof)
			break;
		word[i] = w;
		if(i)
			f =& w==k; else
			k = w;
	}
	if(i)
	if(f && !eof) {
		if(!(flag && k==key)) {
			dupl();
			key = k;
			from[0] = a[0];
			from[1] = a[1];
		}
		flag =+ i;
	} else {
		dupl();
		line(a, word, i);
	}
	if(!eof)
		goto loop;
	puta(addr);
	putchar('\n');

done:
	flush();
}

dupl()
{

	if(flag) {
		flag = 0;
		line(from, &key, 1);
	}
}

puta(a)
int a[2];
{

	putn(a[0], base, 4);
	putn(a[1], base, 3);
	putchar(' ');
}

line(a, w, n)
int w[];
{
	int i, f, c;

	f = 1;
	for(c=1; c; c=+c) {
		if((c&conv) == 0)
			continue;
		if(f) {
			puta(a);
			f = 0;
		} else
			for(i=0; i<8; i++)
				putchar(' ');
		for(i=0; i<n; i++) {
			putx(w[i], c);
			putchar(i==n-1? '\n': ' ');
		}
	}
}

putx(n, c)
{

	switch(c) {
	case 001:
		pre(6);
		putn(n, 8, 6);
		break;
	case 002:
		pre(5);
		putn(n, 10, 5);
		break;
	case 004:
		pre(4);
		putop(n);
		break;
	case 010:
		pre(4);
		putn(n, 16, 4);
		break;
	case 020:
		pre(5);
		putc(n);
		putchar(' ');
		putc(n>>8);
		break;
	case 040:
		pre(7);
		putn(n&0377, 8, 3);
		putchar(' ');
		putn((n>>8)&0377, 8, 3);
		break;
	}
}

getw()
{
	int b1, b2;

	b1 = getc();
	if(b1 == -1) {
		eof = 1;
		return(0);
	}
	b2 = getc();
	if(b2 == -1)
		b2 = 0;
	return(b1|(b2<<8));
}

getc()
{

	if(gidx >= gcnt) {
		gcnt = read(fi, gbuf, 512);
		if(gcnt <= 0)
			return(-1);
		gidx = 0;
	}
	if(++addr[1] >= basem) {
		addr[0]++;
		addr[1] = 0;
	}
	return(gbuf[gidx++]&0377);
}

putc(c)
{
	c =& 0377;
	if(c>037 && c<0177 && c!='\\') {
		putchar(' ');
		putchar(c);
		return;
	}
	putchar('\\');
	switch(c) {
	case '\0':
		putchar('0');
		break;
	case '\n':
		putchar('n');
		break;
	case '\\':
		putchar('\\');
		break;
	case '\t':
		putchar('t');
		break;
	default:
		putchar('?');
	}
}

putn(n, b, c)
{

	if(!c)
		return;
	putn(ldiv(0,n,b),b,c-1);
	if((n=lrem(0,n,b)) > 9)
		putchar(n-10+'a'); else
		putchar(n+'0');
}

pre(n)
{
	int i;

	for(i=n; i<max; i++)
		putchar(' ');
}

offset(s)
char s[];
{
	char *p;
	int a[2], d, i, j, b;

	p = s;
	while(*p++);
	i = p-s-1;
	b = 0;
	if(i > 0)
		if(p[-2] == 'b') {
			i--;
			b++;
			p--;
		}
	if(i > 0)
		if(p[-2] == '.') {
			i--;
			base = 10;
			basem = 1000;
		}
	a[0] = 0;
	for(j=0; i-j>3; j++) {
		d = s[j];
		if(d>='0' && d<='9')
			a[0] = a[0]*base + d-'0';
	}
	a[1] = 0;
	for(; i-j>0; j++) {
		d = s[j];
		if(d>='0' && d<='9')
			a[1] = a[1]*base + d-'0';
	}
	if(b) {
		i = a[0]*basem+a[1];
		a[0] = 0;
		a[1] = 0;
		while(i--) {
			a[1] =+ 512;
			while(a[1] >= basem) {
				a[1] =- basem;
				a[0]++;
			}
		}
	}
	i = 0;
	while(a[0] > addr[0]+1) {
		addr[1] =+ 512;
		while(addr[1] >= basem) {
			addr[1] =- basem;
			addr[0]++;
		}
		i++;
	}
	seek(fi, i, 3);
	while(a[0]!=addr[0] || a[1]!=addr[1])
		if(getc() == -1)
			break;
}

putop(n)
{
	char *p;
	int i, c;

	p = getop(n);
	for(i=0; (c = *p++) != '\0'; i++)
		putchar(c);
	for(; i<4; i++)
		putchar(' ');
}

getop(n)
{

	switch(n&0170000)
	{
	case 0000000:
		switch(n&0177000)
		{
		case 0004000:
			return("jsr");

		case 0077000:
			return("sob");
		}
		switch(n&0177400)
		{
		case 0000400:
			return("br");

		case 0001000:
			return("bne");

		case 0001400:
			return("beq");

		case 0002000:
			return("bge");

		case 0002400:
			return("blt");

		case 0003000:
			return("bgt");

		case 0003400:
			return("ble");
		}
		switch(n&0177700)
		{
		case 0000100:
			return("jmp");

		case 0000300:
			return("swab");

		case 0005000:
			return("clr");

		case 0005100:
			return("com");

		case 0005200:
			return("inc");

		case 0005300:
			return("dec");

		case 0005400:
			return("neg");

		case 0005500:
			return("adc");

		case 0005600:
			return("sbc");

		case 0005700:
			return("tst");

		case 0006000:
			return("ror");

		case 0006100:
			return("rol");

		case 0006200:
			return("asr");

		case 0006300:
			return("asl");

		case 0006400:
			return("mark");

		case 0006500:
			return("mfpi");

		case 0006600:
			return("mtpi");

		case 0006700:
			return("sxt");
		}
		switch(n&0177740)
		{
		case 0000240:
			return("flag");
		}
		switch(n&0177770)
		{
		case 0000200:
			return("rts");

		case 0000230:
			return("spl");
		}
		switch(n&0177777)
		{
		case 0000000:
			return("halt");

		case 0000001:
			return("wait");

		case 0000002:
			return("rti");

		case 0000003:
			return("bpt");

		case 0000004:
			return("iot");

		case 0000005:
			return("rset");

		case 0000006:
			return("rtt");
		}
		break;

	case 0010000:
		return("mov ");

	case 0020000:
		return("cmp");

	case 0030000:
		return("bit");

	case 0040000:
		return("bic");

	case 0050000:
		return("bis");

	case 0060000:
		return("add");

	case 0070000:
		switch(n&0177000)
		{
		case 0070000:
			return("mul");

		case 0071000:
			return("div");

		case 0072000:
			return("ash");

		case 0073000:
			return("ashc");

		case 0074000:
			return("xor");
		}
		break;

	case 0100000:
		switch(n&0177400)
		{
		case 0100000:
			return("bpl");

		case 0100400:
			return("bmi");

		case 0101000:
			return("bhi");

		case 0101400:
			return("blos");

		case 0102000:
			return("bvc");

		case 0102400:
			return("bvs");

		case 0103000:
			return("bhis");

		case 0103400:
			return("blo");

		case 0104000:
			return("emt");

		case 0104400:
			return("sys");
		}
		switch(n&0177700)
		{
		case 0105000:
			return("clrb");

		case 0105100:
			return("comb");

		case 0105200:
			return("incb");

		case 0105300:
			return("decb");

		case 0105400:
			return("negb");

		case 0105500:
			return("adcb");

		case 0105600:
			return("sbcb");

		case 0105700:
			return("tstb");

		case 0106000:
			return("rorb");

		case 0106100:
			return("rolb");

		case 0106200:
			return("asrb");

		case 0106300:
			return("aslb");

		case 0106500:
			return("mfpd");

		case 0106600:
			return("mfpd");
		}
		break;

	case 0110000:
		return("movb");

	case 0120000:
		return("cmpb");

	case 0130000:
		return("bitb");

	case 0140000:
		return("bicb");

	case 0150000:
		return("bisb");

	case 0160000:
		return("sub");

	case 0170000:
		switch(n&01777000)
		{
		case 0:0;
		}
		break;
	}
	return("???");
}
