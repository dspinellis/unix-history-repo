#include "r.h"

extern int hollerith;

char	outbuf[80];
int	outp	= 0;
int	cont	= 0;
int	contchar	= '&';

char	comment[320];
int	comptr	= 0;
int	indent	= 0;

outdon() {
	outbuf[outp] = '\0';
	if (outp > 0)
		fprintf(outfil, "%s\n", outbuf);
	outp = cont = 0;
}

putcom(s) char *s; {
	if (printcom) {
		ptc('c');
		outtab();
		pts(s);
		outdon();
	}
}

outcode(xp) char *xp; {
	register c, c1, j;
	char *q, *p;

	p = (char *) xp;	/* shut lint up */
	if (cont == 0 && comptr > 0)	/* flush comment if not on continuation */
		flushcom();
	while( (c = *p++) ){
		c1 = *p;
		if (type[c] == LET || type[c] == DIG) {
			pts(p-1);
			break;
		}
		switch(c){

		case '"': case '\'':
			j = 0;
			for (q=p; *q; q++) {
				if (*q == '\\')
					q++;
				j++;
			}
			if (outp+j+2 > 71)
				contcard();
			if (hollerith) {
				outnum(--j);
				ptc('h');
			} else
				ptc(c);
			while (*p != c) {
				if (*p == '\\')
					p++;
				ptc(*p++);
			}
			if (!hollerith)
				ptc(c);
			p++;
			break;
		case '$': case '\\':
			if (strlen(p-1)+outp > 71)
				contcard();
			if (c1 == '"' || c1 == '\'') {
				ptc(c1);
				p++;
			} else
				for (p--; *p; p++)
					ptc(*p);
			break;
		case '%':
			outp = 0;
			while (*p)
				ptc(*p++);
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
		case '!': case '^':
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
			outtab();
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
	if (strlen(s)+outp > 71)
		contcard();
	while(*s)
		ptc(*s++);
}

contcard(){
	int n;
	outbuf[outp] = '\0';
	fprintf(outfil, "%s\n", outbuf);
	n = 6;
	if (printcom) {
		n += INDENT * indent + 1;
		if (n > 35) n = 35;
	}
	for( outp=0; outp<n; outbuf[outp++] = ' ' );
	outbuf[contfld-1] = contchar;
	cont++;
	if (cont > 19)
		error("more than 19 continuation cards");
}

outtab(){
	int n;
	n = 6;
	if (printcom) {
		n += INDENT * indent;
		if (n > 35) n = 35;
	}
	while (outp < n)
		ptc(' ');
}

outnum(n) int n; {
	int a;
	if( a = n/10 )
		outnum(a);
	ptc(n%10 + '0');
}

outcont(n) int n; {
	transfer = 0;
	if (n == 0 && outp == 0)
		return;
	if( n > 0 )
		outnum(n);
	outcode("\tcontinue");
	outdon();
}

outgoto(n) int n; {
	if (transfer != 0)
		return;
	outcode("\tgoto ");
	outnum(n);
	outdon();
}

flushcom() {
	int i, j;
	if (printcom == 0)
		comptr = 0;
	else if (cont == 0 && comptr > 0) {
		for (i=j=0; i < comptr; i++)
			if (comment[i] == '\n') {
				comment[i] = '\0';
				fprintf(outfil, "%s\n", &comment[j]);
				j = i + 1;
			}
		comptr = 0;
	}
}
