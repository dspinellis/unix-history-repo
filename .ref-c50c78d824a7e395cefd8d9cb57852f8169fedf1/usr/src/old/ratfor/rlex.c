Original BTL Ratfor System for 4.2
#ifndef lint
static char sccsid[] = "@(#)rlex.c	1.2 (Berkeley) %G%";
#endif

# include "r.h"

char *keyword [] = {
	"do",
	"if",
	"else",
	"for",
	"repeat",
	"until",
	"while",
	"break",
	"next",
	"define",
	"include",
	"return",
	"switch",
	"case",
	"default",
	0};

int keytran[] = {
	DO,
	IF,
	ELSE,
	FOR,
	REPEAT,
	UNTIL,
	WHILE,
	BREAK,
	NEXT,
	DEFINE,
	INCLUDE,
	RETURN,
	SWITCH,
	CASE,
	DEFAULT,
	0};

char	*fcnloc;	/* spot for "function" */

int	svargc;
char	**svargv;
char	*curfile[10]	= { "" };
int	infptr	= 0;
FILE	*outfil	= { stdout };
FILE	*infile[10]	= { stdin };
int	linect[10];

int	contfld	= CONTFLD;	/* place to put continuation char */
int	printcom	= 0;	/* print comments if on */
int	hollerith	= 0;	/* convert "..." to 27H... if on */

#ifdef	gcos
char	*ratfor	"tssrat";
int	bcdrat[2];
char	*bwkmeter	".           bwkmeter    ";
int	bcdbwk[5];
#endif

main(argc,argv) int argc; char **argv; {
	int i;
	while(argc>1 && argv[1][0]=='-') {
		if(argv[1][1]=='6') {
			contfld=6;
			if (argv[1][2]!='\0')
				contchar = argv[1][2];
		} else if (argv[1][1] == 'C')
			printcom++;
		else if (argv[1][1] == 'h')
			hollerith++;
		argc--;
		argv++;
	}

#ifdef	gcos
	if (!intss()) {
		_fixup();
		ratfor = "batrat";
	}
	ascbcd(ratfor,bcdrat,6);
	ascbcd(bwkmeter,bcdbwk,24);
	acdata(bcdrat[0],1);
	acupdt(bcdbwk[0]);
	if (!intss()) {
		if ((infile[infptr]=fopen("s*", "r")) == NULL)
			cant("s*");
		if ((outfil=fopen("*s", "w")) == NULL)
			cant("*s");
	}
#endif

	svargc = argc;
	svargv = argv;
	if (svargc > 1)
		putbak('\0');
	for (i=0; keyword[i]; i++)
		install(keyword[i], "", keytran[i]);
	fcnloc = install("function", "", 0);
	yyparse();
#ifdef	gcos
	if (!intss())
		bexit(errorflag);
#endif
	exit(errorflag);
}

#ifdef gcos
bexit(status) {
	/* this is the batch version of exit for gcos tss */
	FILE *inf, *outf;
	char c;

	fclose(stderr);	/* make sure diagnostics get flushed */
	if (status) /* abort */
		_nogud();

	/* good: copy output back to s*, call forty */

	fclose(outfil,"r");
	fclose(infile[0],"r");
	inf = fopen("*s", "r");
	outf = fopen("s*", "w");
	while ((c=getc(inf)) != EOF)
		putc(c, outf);
	fclose(inf,"r");
	fclose(outf,"r");
	__imok();
}
#endif

cant(s) char *s; {
	linect[infptr] = 0;
	curfile[infptr] = s;
	error("can't open");
	exit(1);
}

inclstat() {
	int c;
	char *ps;
	char fname[100];
	while ((c = getchr()) == ' ' || c == '\t');
	if (c == '(') {
		for (ps=fname; (*ps=getchr()) != ')'; ps++);
		*ps = '\0';
	} else if (c == '"' || c == '\'') {
		for (ps=fname; (*ps=getchr()) != c; ps++);
		*ps = '\0';
	} else {
		putbak(c);
		for (ps=fname; (*ps=getchr()) != ' ' &&*ps!='\t' && *ps!='\n' && *ps!=';'; ps++);
		*ps = '\0';
	}
	if ((infile[++infptr] = fopen(fname,"r")) == NULL) {
		cant(fname);
		exit(1);
	}
	linect[infptr] = 0;
	curfile[infptr] = fname;
}

char	str[500];
int	nstr;

yylex() {
	int c, t;
	for (;;) {
		while ((c=gtok(str))==' ' || c=='\n' || c=='\t')
			;
		yylval = c;
		if (c==';' || c=='{' || c=='}')
			return(c);
		if (c==EOF)
			return(0);
		yylval = (int) str;
		if (c == DIG)
			return(DIGITS);
		t = lookup(str)->ydef;
		if (t==DEFINE)
			defstat();
		else if (t==INCLUDE)
			inclstat();
		else if (t > 0)
			return(t);
		else
			return(GOK);
	}
}

int	dbg	= 0;

yyerror(p) char *p; {;}


defstat() {
	int c,i,val,t,nlp;
	extern int nstr;
	extern char str[];
	while ((c=getchr())==' ' || c=='\t');
	if (c == '(') {
		t = '(';
		while ((c=getchr())==' ' || c=='\t');
		putbak(c);
	}
	else {
		t = ' ';
		putbak(c);
	}
	for (nstr=0; c=getchr(); nstr++) {
		if (type[c] != LET && type[c] != DIG)
			break;
		str[nstr] = c;
	}
	putbak(c);
	str[nstr] = '\0';
	if (c != ' ' && c != '\t' && c != '\n' && c != ',') {
		error("illegal define statement");
		return;
	}
	val = nstr+1;
	if (t == ' ') {
		while ((c=getchr())==' ' || c=='\t');
		putbak(c);
		for (i=val; (c=getchr())!='\n' && c!='#' && c!='\0'; i++)
			str[i] = c;
		putbak(c);
	} else {
		while ((c=getchr())==' ' || c=='\t' || c==',' || c=='\n');
		putbak(c);
		nlp  = 0;
		for (i=val; nlp>=0 && (c=str[i]=getchr()); i++)
			if (c == '(')
				nlp++;
			else if (c == ')')
				nlp--;
		i--;
	}
	for ( ; i>0; i--)
		if (str[i-1] != ' ' && str[i-1] != '\t')
			break;
	str[i] = '\0';
	install(str, &str[val], 0);
}

