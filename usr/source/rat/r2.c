#include "r.h"

char	outbuf[80];
int	outp	0;
int	cont	0;

outcode(p) char *p; {
	int i,j,c,c1;
	char *q;
	if( p == 0 ){
		outbuf[outp] = '\0';
		printf("%s\n", outbuf);
		outp = cont = 0;
		return;
	}
	while( (c = *p++) ){
		c1 = *p;
		switch(c){

		case '"':
		case '\'':
			for( q=p; *q != c; q++ );
			outnum(q-p);
			ptc('h');
			while( p != q )
				ptc(*p++);
			p++;
			break;
		case '>':
			if( c1=='=' ){
				pts(".ge."); p++;
			} else
				pts(".gt.");
			break;
		case '<':
			if( c1=='=' ){
				pts(".le."); p++;
			} else if( c1=='>' ){
				pts(".ne."); p++;
			} else
				pts(".lt.");
			break;
		case '=':
			if( c1=='=' ){
				pts(".eq."); p++;
			} else
				ptc('=');
			break;
		case '!':
			if( c1=='=' ){
				pts(".ne."); p++;
			} else
				pts(".not.");
			break;
		case '&':
			if( c1=='&' )
				p++;
			pts(".and.");
			break;
		case '|':
			if( c1=='|' )
				p++;
			pts(".or.");
			break;
		case '\t':
			tabs();
			break;
		case '\n':
			ptc(' ');
			break;
		default:
			ptc(c);
			break;
		}
	}
}

ptc(c) char c; {
	if( outp > 71 )
		contcard();
	outbuf[outp++] = c;
}

pts(s) char *s; {
	while(*s)
		ptc(*s++);
}

int	contfld	0;

contcard(){
	outbuf[outp] = '\0';
	printf("%s\n", outbuf);
	for( outp=0; outp<contfld-1; outbuf[outp++] = ' ' );
	outbuf[outp++] = '&';
}

tabs(){
	ptc(' ');
	while( outp<7 )
		ptc(' ');
	while( outp%3 != 1)
		ptc(' ');
}

outnum(n) int n; {
	int a;
	if( a = n/10 )
		outnum(a);
	ptc(n%10 + '0');
}

outcont(n) int n; {
	if( n > 0 )
		outnum(n);
	outcode("\tcontinue");
	outcode(0);
}

outgoto(n) int n; {
	outcode("\tgoto ");
	outnum(n);
	outcode(0);
}
