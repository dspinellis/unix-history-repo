#include <stdio.h>
#include "as.yh"
#define NCPS 24
#define LIBNAME "	.LIBRARY	/DB0:[C]JEQL.MLB/\n"
#define TRUE 1
#define FALSE 0
#define clearl()	(*strpnt=0,fputs(strbuf,stdout),strpnt=strbuf)
FILE	*tmpfil;
FILE	*relfil;
FILE	*txtfil;
char	yytext[NCPS+2];
char	sname[8];
char	strbuf[512];
char	*strpnt = strbuf;
int	pending;
int	index;
int	lineno;
int	curval;
int	skipspace;
int	coffset;
int	yylval;
long	intval;
int	anyerrs;
int	sawname;
int	saveit;
/*define yylex() (fprintf(stderr,"yylex returns %d\n",saveit=oyylex()),saveit)*/
#define yylex() oyylex()

main(argc,argv)
register char **argv;
{
	int nameflag = 0;char namebuf[16]; register int i;
	if(--argc>0) {
		freopen(*++argv,"r",stdin);
		nameflag = 1;
	}
	if(--argc>0)
		freopen(argv[1],"w",stdout);
	if(nameflag) {
		register char *base = *argv;

		while(*base++);				/* Find end */
		while(--base>=*argv && *base!='/');	/* find last slash */
		base++; 				/* set base */
		for(i = 0; i < 16 && base[i] && base[i]!='.';)
			namebuf[i++]=((base[i] >= 'a' && base[i] <= 'z')
					? (base[i] + 'A' -'a') : base[i]);
			namebuf[i] = 0;
		printf("\t.TITLE\t%s\n",namebuf);
		printf(LIBNAME);
	}


loop:
	for(;;)  {
		i = yylex();
	again:
		switch(i) {

		case NL:
			clearl();
			break;

		case NAME:
			sawname = 1;
			break;

		case CM: case SP:
			sawname = 0;
			
		case LP:
			fixlength();
			break;

		case 0:
			goto done;
		
		}
	}
done:	printf("	.END\n");
}

fixlength(){
	register char *cp = strpnt;
	if(sawname==0) return;
	while(*--cp != ' ' && *cp != '\t' && *cp != ',' && *cp!='@') {
		cp[2] = *cp;
		if(cp <= strbuf) yyerror("Bad ( construction");
	}
	cp++;
	*cp++ = 'L';
	*cp++ = '^';
	strpnt += 2;
}

static long mask[] = {
0xffffffff, 0xfffffffe, 0xfffffffc, 0xfffffff8, 
0xfffffff0, 0xffffffe0, 0xffffffc0, 0xffffff80, 
0xffffff00, 0xfffffe00, 0xfffffc00, 0xfffff800, 
0xfffff000, 0xffffe000, 0xffffc000, 0xffff8000, 
0xffff0000, 0xfffe0000, 0xfffc0000, 0xfff80000, 
0xfff00000, 0xffe00000, 0xffc00000, 0xff800000, 
0xff000000, 0xfe000000, 0xfc000000, 0xf8000000, 
0xf0000000, 0xe0000000, 0xc0000000, 0x80000000,
};

short type[] = {
	EOF,
	SP,	0,	0,	0,	0,	0,	0,	0,
	0,	SP,	NL,	0,	0,	SP,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	SP,	0,	0,	SH,	LITOP,	REG,	AND,	SQ,
	LP,	RP,	MUL,	PLUS,	CM,	MINUS,	ALPH,	DIV,
	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,
	DIG,	DIG,	COLON,	SEMI,	LSH,	0,	RSH,	0,
	0,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	LB,	0,	RB,	XOR,	ALPH,
	0,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	0,	IOR,	0,	TILDE,	0,
};

oyylex()
{
	register val;
	register base;
	register char *cp;
	struct symtab *op;
	static double fltval;
	char fltchr[64];
	int minflag = FALSE;

loop:
	switch(yylval = (type+1)[val = getchar()]) {

	case SH:
		copy(';');
		while ((val = getchar()) != '\n' && val>0)
			copy(val);
		copy(val);
		val = NL;
		goto ret;


	case EOF:
		val = 0;
		goto ret;

	case ALPH:
		cp = yytext;
		if(val=='_') val = getchar();
		do {
			if (cp <= &yytext[NCPS])
				*cp++ = val;
				copy(val);
		} while ((type+1)[val=getchar()]==ALPH || (type+1)[val]==DIG 
				|| (type+1)[val]==LITOP);
		*cp = '\0';
		if(skipspace)
			for(;(type+1)[val]==SP; val = getchar())
				copy(val);
		ungetc(val, stdin);
		if(*yytext!='.')
			return(val=NAME);
		else if(strcmp(yytext,".byte")==0)
			return(IBYTE);
		else if(strcmp(yytext,".word")==0)
			return(IWORD);
		else if(strcmp(yytext,".long")==0)
			return(ILONG);
		else
			return(NAME);

	case MINUS:
		copy(val);
		switch ((type+1)[val = getchar()]) {
		case LP:
			copy(val);
			return(val = MP);
		case DIG:
			break;
		default:
			ungetc(val,stdin);
			return(val = MINUS);
		}
		minflag = TRUE;
	case DIG:
		intval = val-'0';
		if (val=='0') {
			val = getchar();
			if (val=='x' || val=='X') {
				base = 16;
				copy('^');
				copy('X');
			} else if (val=='d' || val=='D' || val=='f' || val=='F') {
				char *p = fltchr;
				double atof();
				while (p < &fltchr[63] && ((val=getchar())=='.'
				 || val=='e' || val=='d' || val=='E' || val=='D'
				 || val=='-' || val=='+' || (type+1)[val]==DIG))
					copy(val),*p++ = val;
				ungetc(val, stdin);
				*p++ = '\0';
				fltval = atof(fltchr);
				val = FLTNUM;
				goto ret;
			} else {
				ungetc(val, stdin);
				base = 8;
				if((type+1)[val]!=DIG) {
					copy('0');
					val = INT;
					yylval = 0;
					goto ret;
				}
				copy('^');
				copy('O');
			}
		} else
			copy(val),base = 10;
		while ((type+1)[val=getchar()]==DIG
		    || (base==16 && (val>='a' && val<='f'||val>='A' && val<='F'))) {
			copy(val);
			if (base==8)
				intval <<= 3;
			else if (base==10)
				intval *= 10;
			else
				intval <<= 4;
			if (val>='a' && val<='f')
				val -= 'a' - 10 - '0';
			else if (val>='A' && val<='F')
				val -= 'A' - 10 - '0';
			intval += val-'0';
		}
		ungetc(val, stdin);
		val = INT;
		if(minflag) intval = -intval;
		goto ret;


	case MUL:
		copy('@');
		goto ret;
	
	case LITOP:
		copy('#');
		goto ret;
	
	case SQ:
		if ((yylval = getchar()) == '\n')
			lineno++;
		intval = yylval;
		sprintf(strpnt,"%d",intval);
		for(;(strpnt<&strbuf[512])&& *strpnt; strpnt++);
		val = INT;
		goto ret;

	case 0:
		yyerror("Illegal character");
		val = NOCHAR;
		goto ret;

	case SP:
		if(skipspace) {
			copy(val);
			goto loop;
		}

	default:
		copy(val);
		val = yylval;
		goto ret;
	}
ret:
	return(val);
}

yyerror(s, a)
char *s;
{
	if (anyerrs==0)
		fprintf(stderr, "Assembler:\n");
	anyerrs++;
	fprintf(stderr, "line %d: ", lineno);
	fprintf(stderr, s, a);
	fprintf(stderr, "\n");
}

copy(p)
register p;
{	
	if(p>='a'&&p<='z')
		p += 'A' - 'a';
	if(strpnt<strbuf+512)
		*strpnt++=(p);
	return(p);
}
