#ifndef lint
static char sccsid[] = "@(#)text.c	4.3 %G%";
#endif

# include "e.h"
# include "e.def"

int	csp;
int	psp;
#define	CSSIZE	400
char	cs[420];

int	lf, rf;	/* temporary spots for left and right fonts */

text(t,p1) int t; char *p1; {
	int c;
	char *p;
	tbl *tp, *lookup();
	extern tbl *restbl;

	yyval = oalloc();
	ebase[yyval] = 0;
#ifndef NEQN
	eht[yyval] = VERT(6 * ((ps>6)?ps:6));	/* ht in machine units */
#else NEQN
	eht[yyval] = VERT(2);	/* 2 half-spaces */
#endif NEQN
	lfont[yyval] = rfont[yyval] = ROM;
	if (t == QTEXT)
		p = p1;
	else if ( t == SPACE )
		p = "\\ ";
	else if ( t == THIN )
		p = "\\|";
	else if ( t == TAB )
		p = "\\t";
	else if ((tp = lookup(&restbl, p1, NULL)) != NULL)
		p = tp->defn;
	else {
		lf = rf = 0;
		for (csp=psp=0; (c=p1[psp++])!='\0';) {
			rf = trans(c, p1);
			if (lf == 0)
				lf = rf;	/* save first */
			if (csp>CSSIZE)
				error(FATAL,"converted token %.25s... too long",p1);
		}
		cs[csp] = '\0';
		p = cs;
		lfont[yyval] = lf;
		rfont[yyval] = rf;
	}
	if(dbg)printf(".\t%dtext: S%d <- %s; b=%d,h=%d,lf=%c,rf=%c\n",
		t, yyval, p, ebase[yyval], eht[yyval], lfont[yyval], rfont[yyval]);
	printf(".ds %d \"%s\n", yyval, p);
}

trans(c,p1) int c; char *p1; {
	int f;
	f = ROM;
	switch( c) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	case ':': case ';': case '!': case '%':
	case '(': case '[': case ')': case ']':
	case ',':
		if (rf == ITAL)
			shim();
		roman(c); break;
	case '.':
		if (rf == ROM)
			roman(c);
		else
			cs[csp++] = c;
		f = rf;
		break;
	case '|':
		if (rf == ITAL)
			shim();
		shim(); roman(c); shim(); break;
	case '=':
		if (rf == ITAL)
			shim();
		name4('e','q');
		break;
	case '+':
		if (rf == ITAL)
			shim();
		name4('p', 'l');
		break;
	case '>': case '<':
		if (rf == ITAL)
			shim();
		if (p1[psp]=='=') {	/* look ahead for == <= >= */
			name4(c,'=');
			psp++;
		} else {
			cs[csp++] = c;  
		}
		break;
	case '-':
		if (rf == ITAL)
			shim();
		if (p1[psp]=='>') {
			name4('-','>'); psp++;
		} else {
			name4('m','i');
		}
		break;
	case '/':
		if (rf == ITAL)
			shim();
		name4('s','l');
		break;
	case '~': case ' ':
		shim(); shim(); break;
	case '^':
		shim(); break;
	case '\\':	/* troff - pass 2 or 3 more chars */
		if (rf == ITAL)
			shim();
		cs[csp++] = c; cs[csp++] = c = p1[psp++]; cs[csp++] = p1[psp++];
		if (c=='(') cs[csp++] = p1[psp++];
		if (c=='*' && cs[csp-1] == '(') {
			cs[csp++] = p1[psp++];
			cs[csp++] = p1[psp++];
		}
		break;
	case '\'':
		cs[csp++] = '\\'; cs[csp++] = 'f'; cs[csp++] = rf==ITAL ? ITAL : ROM;
		name4('f','m');
		cs[csp++] = '\\'; cs[csp++] = 'f'; cs[csp++] = 'P';
		f = rf==ITAL ? ITAL : ROM;
		break;

	case 'f':
		if (ft == ITAL) {
			cs[csp++] = '\\'; cs[csp++] = '^';
			cs[csp++] = 'f';
			cs[csp++] = '\\'; cs[csp++] = '|';	/* trying | instead of ^ */
			f = ITAL;
		}
		else
			cs[csp++] = 'f';
		break;
	case 'j':
		if (ft == ITAL) {
			cs[csp++] = '\\'; cs[csp++] = '^';
			cs[csp++] = 'j';
			f = ITAL;
		}
		else
			cs[csp++] = 'j';
		break;
	default:
		cs[csp++] = c;
		f = ft==ITAL ? ITAL : ROM;
		break;
	}
	return(f);
}

shim() {
	cs[csp++] = '\\'; cs[csp++] = '|';
}

roman(c) int c; {
	cs[csp++] = '\\'; cs[csp++] = 'f'; cs[csp++] = ROM;
	cs[csp++] = c;
	cs[csp++] = '\\'; cs[csp++] = 'f'; cs[csp++] = 'P';
}

name4(c1,c2) int c1,c2; {
	cs[csp++] = '\\';
	cs[csp++] = '(';
	cs[csp++] = c1;
	cs[csp++] = c2;
}
