/*
 * od -- octal (also hex, decimal, and character) dump
 */

#include <stdio.h>

unsigned short	word[8];
unsigned short	lastword[8];
int	conv;
int	base =	010;
int	max;
long	addr;

main(argc, argv)
char **argv;
{
	register char *p;
	register n, f, same;


	argv++;
	f = 0;
	if(argc > 1) {
		p = *argv;
		if(*p == '-') {
			while(*p != '\0') {
				switch(*p++) {
				case 'o':
					conv |= 001;
					f = 6;
					break;
				case 'd':
					conv |= 002;
					f = 5;
					break;
				case 'x':
				case 'h':
					conv |= 010;
					f = 4;
					break;
				case 'c':
					conv |= 020;
					f = 7;
					break;
				case 'b':
					conv |= 040;
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
		if (freopen(*argv, "r", stdin) == NULL) {
			printf("cannot open %s\n", *argv);
			exit(1);
		}
		argv++;
		argc--;
	}
	if(argc > 1)
		offset(*argv);

	same = -1;
	for ( ; (n = fread((char *)word, 1, sizeof(word), stdin)) > 0; addr += n) {
		if (same>=0) {
			for (f=0; f<8; f++)
				if (lastword[f] != word[f])
					goto notsame;
			if (same==0) {
				printf("*\n");
				same = 1;
			}
			continue;
		}
	notsame:
		line(addr, word, (n+sizeof(word[0])-1)/sizeof(word[0]));
		same = 0;
		for (f=0; f<8; f++)
			lastword[f] = word[f];
		for (f=0; f<8; f++)
			word[f] = 0;
	}
	putn(addr, base, 7);
	putchar('\n');
}

line(a, w, n)
long a;
unsigned short *w;
{
	register i, f, c;

	f = 1;
	for(c=1; c; c<<=1) {
		if((c&conv) == 0)
			continue;
		if(f) {
			putn(a, base, 7);
			putchar(' ');
			f = 0;
		} else
			putchar('\t');
		for (i=0; i<n; i++) {
			putx(w[i], c);
			putchar(i==n-1? '\n': ' ');
		}
	}
}

putx(n, c)
unsigned n;
{

	switch(c) {
	case 001:
		pre(6);
		putn((long)n, 8, 6);
		break;
	case 002:
		pre(5);
		putn((long)n, 10, 5);
		break;
	case 010:
		pre(4);
		putn((long)n, 16, 4);
		break;
	case 020:
		pre(7);
		{
			unsigned short sn = n;
			cput(*(char *)&sn);
			putchar(' ');
			cput(*((char *)&sn + 1));
			break;
		}
	case 040:
		pre(7);
		{
			unsigned short sn = n;
			putn((long)(*(char *)&sn)&0377, 8, 3);
			putchar(' ');
			putn((long)(*((char *)&sn + 1))&0377, 8, 3);
			break;
		}
	}
}

cput(c)
{
	c &= 0377;
	if(c>037 && c<0177) {
		printf("  ");
		putchar(c);
		return;
	}
	switch(c) {
	case '\0':
		printf(" \\0");
		break;
	case '\b':
		printf(" \\b");
		break;
	case '\f':
		printf(" \\f");
		break;
	case '\n':
		printf(" \\n");
		break;
	case '\r':
		printf(" \\r");
		break;
	case '\t':
		printf(" \\t");
		break;
	default:
		putn((long)c, 8, 3);
	}
}

putn(n, b, c)
long n;
{
	register d;

	if(!c)
		return;
	putn(n/b, b, c-1);
	d = n%b;
	if (d > 9)
		putchar(d-10+'a');
	else
		putchar(d+'0');
}

pre(n)
{
	int i;

	for(i=n; i<max; i++)
		putchar(' ');
}

offset(s)
register char *s;
{
	register char *p;
	long a;
	register int d;

	if (*s=='+')
		s++;
	if (*s=='x') {
		s++;
		base = 16;
	} else if (*s=='0' && s[1]=='x') {
		s += 2;
		base = 16;
	} else if (*s == '0')
		base = 8;
	p = s;
	while(*p) {
		if (*p++=='.')
			base = 10;
	}
	for (a=0; *s; s++) {
		d = *s;
		if(d>='0' && d<='9')
			a = a*base + d - '0';
		else if (d>='a' && d<='f' && base==16)
			a = a*base + d + 10 - 'a';
		else
			break;
	}
	if (*s == '.')
		s++;
	if(*s=='b' || *s=='B')
		a *= 512;
	fseek(stdin, a, 0);
	addr = a;
}
