#ifndef lint
static char sccsid[] = "@(#)yylex.c	1.4 10/15/86";
#endif lint

#define isid(a)  ((fastab+COFF)[a]&IB)
#define IB 1
/*	#if '\377' < 0		it would be nice if this worked properly!!!!! */
#if pdp11 | vax | mc68000 | tahoe
#define COFF 128
#else
#define COFF 0
#endif

yylex() {
	static int ifdef=0;
	static char *op2[]={"||",  "&&" , ">>", "<<", ">=", "<=", "!=", "=="};
	static int  val2[]={OROR, ANDAND,  RS,   LS,   GE,   LE,   NE,   EQ};
	static char *opc="b\bt\tn\nf\fr\r\\\\";
	extern char fastab[];
	extern char *outp,*inp,*newp; extern int flslvl;
	register char savc, *s; char *skipbl(); int val;
	register char **p2;
	struct symtab {
		char *name;
		char *value;
	} *sp, *lookup();

for (;;) {
	extern int passcom;		/* this crap makes #if's work */
	int opt_passcom = passcom;	/* even with -C option */
	passcom = 0;			/* (else comments make syntax errs) */
	newp=skipbl(newp);
	passcom = opt_passcom;		/* nb: lint uses -C so its useful! */
	if (*inp=='\n') return(stop);	/* end of #if */
	savc= *newp; *newp='\0';
	for (p2=op2+8; --p2>=op2; )	/* check 2-char ops */
		if (0==strcmp(*p2,inp)) {val=val2[p2-op2]; goto ret;}
	s="+-*/%<>&^|?:!~(),";	/* check 1-char ops */
	while (*s) if (*s++== *inp) {val= *--s; goto ret;}
	if (*inp<='9' && *inp>='0') {/* a number */
		if (*inp=='0') yylval= (inp[1]=='x' || inp[1]=='X') ?
			tobinary(inp+2,16) : tobinary(inp+1,8);
		else yylval=tobinary(inp,10);
		val=number;
	} else if (isid(*inp)) {
		if (0==strcmp(inp,"defined")) {ifdef=1; ++flslvl; val=DEFINED;}
		else {
			sp=lookup(inp,-1); if (ifdef!=0) {ifdef=0; --flslvl;}
			yylval= (sp->value==0) ? 0 : 1;
			val=number;
		}
	} else 	if (*inp=='\'') {/* character constant */
		val=number;
		if (inp[1]=='\\') {/* escaped */
			char c; if (newp[-1]=='\'') newp[-1]='\0';
			s=opc;
			while (*s) if (*s++!=inp[2]) ++s; else {yylval= *s; goto ret;}
			if (inp[2]<='9' && inp[2]>='0') yylval=c=tobinary(inp+2,8);
			else yylval=inp[2];
		} else yylval=inp[1];
	} else if (0==strcmp("\\\n",inp)) {*newp=savc; continue;}
	else {
		*newp=savc; pperror("Illegal character %c in preprocessor if", *inp);
		continue;
	}
ret:
	*newp=savc; outp=inp=newp; return(val);
}
}

tobinary(st, b) char *st; {
	int n, c, t;
	char *s;
	n=0;
	s=st;
	while (c = *s++) {
	switch(c) {
		case '0': case '1': case '2': case '3': case '4': 
		case '5': case '6': case '7': case '8': case '9': 
			t = c-'0'; break;
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': 
			t = c-'a'+10; if (b>10) break;
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': 
			t = c - 'A'+10; if (b>10) break;
		default:
			t = -1;
			if ( c=='l' || c=='L') if (*s=='\0') break;
			pperror("Illegal number %s", st);
	}
	if (t<0) break;
	n = n*b+t;
	}
return(n);
}
