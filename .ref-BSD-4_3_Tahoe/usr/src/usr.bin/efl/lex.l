%Start DOTSON
%{
#include <ctype.h>
#include "defs"
#include "tokdefs"

typedef union { int ival; ptr pval; } YYSTYPE;
extern YYSTYPE yylval;
YYSTYPE prevl;
int prevv;
char *copys();
static ptr p;
static ptr q;
static FILE *fd;
static int quoted, k;
static int rket	= 0;
FILE *opincl();
ptr mkdef(), mkcomm(), mkname(), mkimcon();

#define RET(x)	{ RETI(x,x);  }

#define RETL(yv,yl) {yylval=prevl=yl;igeol=comneed=0;return(prevv=yv); }
#define RETP(yv,yl) {yylval.pval=prevl.pval=yl;igeol=comneed=0;return(prevv=yv); }
#define RETI(yv,yl) {yylval.ival=prevl.ival=yl;igeol=comneed=0;return(prevv=yv); }
#define REL(n)  {  RETI(RELOP, OPREL+n);}
#define AS(n)  {  RETI(ASGNOP, OPASGN+n); }
#define RETC(x) { RETP(CONST, mkconst(x,yytext) );  }
#define RETZ(x) { yytext[yyleng-1] = '\0'; RETP(CONST, mkimcon(x,yytext) ); }

%}

D	[0-9]
d	[dD][+-]?[0-9]+
e	[eE][+-]?[0-9]+
i	[iI]

%%

[a-zA-Z][a-zA-Z0-9_]*	{
		   lower(yytext);
		   if(lettneed && yyleng==1)
			{ RETI(LETTER, yytext[0]); }
		   else if(defneed)
			{
			register char *q1, *q2;
			for(q2=q1=yytext+yyleng+1 ; (*q1 = efgetc)!='\n' ; ++q1)
				;
			*q1 = '\0';
			p = mkdef(yytext, q2);
			defneed = 0;
			++yylineno;
			unput('\n');
			}
		   else if(optneed)
			{ RETP(OPTNAME, copys(yytext)); }
		   else if(comneed && ( (q=name(yytext,1))==NULL || q->tag!=TDEFINE) )
			{ RETP(COMNAME, mkcomm(yytext) ); }
		   else if(q = name(yytext,1)) switch(q->tag)
			{
			case TDEFINE:
				filelines[filedepth] = yylineno;
				filemacs[filedepth] = efmacp;
				pushchars[filedepth] = (yysptr>yysbuf?
						*--yysptr : -1);
				if(++filedepth >= MAXINCLUDEDEPTH)
					fatal("macro or include too deep");
				filelines[filedepth] = yylineno = 1;
				efmacp = q->varp->valp;
				filenames[filedepth] = NULL;
				break;	/*now process new input */

			case TSTRUCT:
				RETP(STRUCTNAME, q);

			case TNAME:
				RETP(NAME, q);

			case TKEYWORD:
				if(q->subtype == END)
					{
					register int c;
					eofneed = YES;
					while((c=input())!=';'&&c!='\n'&&c!=EOF)
						;
					NLSTATE;
					}
				RET(q->subtype);

			default:
				fatal1("lex: impossible type code %d", q->tag);
			}
		   else  RETP(NAME, mkname(yytext) );
		}

","	RET(COMMA);
";"	RET(EOS);

"("	RET(LPAR);
")"	RET(RPAR);


"["	|
"{"	RET(LBRACK);

"]"	|
"}"	{ if(iobrlevel>0) RET(RBRACK); rket = 1;  RET(EOS); }

","	RET(COMMA);
":"	RET(COLON);

"$"	RET(REPOP);

<DOTSON>"."[oO][rR]"."	|
"|"	RETI(OR,OPOR);
<DOTSON>"."[cC][oO][rR]"."	|
"||"	RETI(OR,OP2OR);
<DOTSON>"."[aA][nN][dD]"."	|
"&"	RETI(AND,OPAND);
<DOTSON>"."[cC][aA][nN][dD]"."	|
"&&"	RETI(AND,OP2AND);
<DOTSON>"."[nN][oO][tT]"."	|
"~"	RETI(NOT,OPNOT);
"!"	RETI(NOT,OPNOT);

<DOTSON>"."[lL][tT]"."	|
"<"	REL(OPLT);
<DOTSON>"."[lL][eE]"."	|
"<="	REL(OPLE);
<DOTSON>"."[gG][tT]"."	|
">"	REL(OPGT);
<DOTSON>"."[gG][eE]"."	|
">="	REL(OPGE);
<DOTSON>"."[eE][qQ]"."	|
"=="	REL(OPEQ);
<DOTSON>"."[nN][eE]"."	|
"~="	|
"!="	REL(OPNE);

"->"	RET(ARROW);
"."	RET(QUALOP);

"+"	RETI(ADDOP, OPPLUS);
"-"	RETI(ADDOP, OPMINUS);
"*"	RETI(MULTOP, OPSTAR);
"/"	RETI(MULTOP, OPSLASH);

"**"	|
"^"	RETI(POWER, OPPOWER);

"++"	RETI(DOUBLEADDOP, OPPLUS);
"--"	RETI(DOUBLEADDOP, OPMINUS);

"="	AS(OPASGN);
"+="	AS(OPPLUS);
"-="	AS(OPMINUS);
"*="	AS(OPSTAR);
"/="	AS(OPSLASH);
"**="	|
"^="	AS(OPPOWER);

"&="	AS(OPAND);
"&&="	AS(OP2AND);
"|="	AS(OPOR);
"||="	AS(OP2OR);

\'[^\n']*\'	|
\"[^\n"]*\"	{ yytext[yyleng-1] = '\0'; p = mkconst(TYCHAR,yytext+1);
		  RETP(CONST,p); }

{D}+[hH]	{ /* nh construct */
		int i, n;  char c;
		yytext[yyleng-1] = '\0';  n = convci(yytext);
		for(i = 0; i<n ; ++i)
			if( (c=yytext[i]=input()) == '\n' || c=='\0') break;
		yytext[i] = '\0';
		p = mkconst(TYCHAR,yytext);
		p->vtypep = mkint(i);
		RETP(CONST, p);
		}

{D}+		RETC(TYINT);

{D}+"."{D}*	|
{D}*"."{D}+	RETC(TYREAL);

{D}+"."?{D}*{e}	|
{D}*"."{D}+{e}	RETC(TYREAL);

{D}+"."?{D}*{d}	|
{D}*"."{D}+{d}	RETC(TYLREAL);

{D}+{i}		{ yytext[yyleng-1] = '.';
		  RETP(CONST,mkimcon(TYCOMPLEX,yytext)); }

{D}+"."{D}*{i}	|
{D}*"."{D}+{i}	RETZ(TYCOMPLEX);

{D}+"."?{D}*{e}{i}	|
{D}*"."{D}+{e}{i}	RETZ(TYCOMPLEX);

{D}+"."?{D}*{d}{i}	|
{D}*"."{D}+{d}{i}	RETZ(TYLCOMPLEX);

"#".*	{ if(! nocommentflag) goto litline; }

^"%".*	{ if(thisexec) thisexec->nftnst += 2;
	  if(inproc)
		{
		unput('\n');
		RETP(ESCAPE, copys(yytext));
		}

	litline:	p = mkchain( copys(yytext), CHNULL);
			if(inproc==0 && yytext[0]=='%')
				prevcomments = hookup(prevcomments, p);
			else
				comments =  hookup(comments,p);
	}

" "	;
\t	;
\f	;

"_"[ \t]*\n	;

\n	{ if(igeol) { igeol=0; prevv = NEWLINE; }
	  else if(prevv>=NAME || prevv==RPAR || prevv==RBRACK
			|| prevv== -1 || prevv==QUALOP)
		RET(EOS); }

.	{ char * linerr();
	  fprintf(diagfile, "Bad input character %c %s\n", yytext[0], linerr());
	  ++nerrs;
	}

^[ \t]*[iI][nN][cC][lL][uU][dD][eE].*\n	{ /* Include statement */
	char *q1;
	register char *q2;
	for(q1=yytext ; *q1==' ' || *q1=='\t' ; ++q1) ;
	quoted = NO;
	for(q1 += 7 ; *q1==' ' || *q1=='\t' ||
		*q1=='\'' || *q1=='"' || *q1=='(' ; ++q1 )
			if(*q1=='"' || *q1=='\'')
				quoted = YES;
	for(q2=q1 ; *q2!='\0' &&  *q2!=' ' && *q2!='\n' &&
		*q2!='\'' && *q2!='"' && *q2!=')' ; ++q2 )
			;
	*q2 = '\0';
	if( ! quoted)
		for(k=0; (q = name(q1,1)) && q->tag==TDEFINE ; ++k)
			{
			if(k > MAXINCLUDEDEPTH)
				fatal1("Macros too deep for %s", yytext);
			q1 = q->varp->valp;
			}
	if( (fd = opincl(&q1)) == NULL)
		{
		fprintf(diagfile, "Cannot open file %s.  Stop.\n", q1);
		exit(2);
		}
	filelines[filedepth] = yylineno;
	pushchars[filedepth] = '\n';
	if(++filedepth >= MAXINCLUDEDEPTH)
		fatal("macro or include too deep");
	fileptrs[filedepth] = yyin = fd;
	filenames[filedepth] = copys(q1);
	filelines[filedepth] = yylineno = 1;
	filemacs[filedepth] = NULL;
	}

%%

yywrap()
{
if(filedepth == 0)
	{
	ateof = 1;
	return(1);
	}

if(efmacp == 0)
	{
	fclose(yyin);
	cfree(filenames[filedepth]);
	}

--filedepth;
if( filemacs[filedepth] )
	efmacp = filemacs[filedepth];
else	{
	yyin = fileptrs[filedepth];
	efmacp = 0;
	}
yylineno = filelines[filedepth];
if(pushchars[filedepth] != -1)
	unput( pushchars[filedepth] );
return(0);
}



lower(s)	/* replace upper with lower case letters */
register char *s;
{
register char *t;
for(t=s ; *t ; ++t)
	if( isupper(*t) )
		*s++ = tolower(*t);
	else if(*t != '_')
		*s++ = *t;
}




setdot(k)
int k;
{
if(k)
	BEGIN DOTSON;
else	BEGIN 0;
}




FILE *opincl(namep)
char **namep;
{
#ifndef unix
	return( fopen(*namep, "r") );
#else

	/* On Unix, follow the C include conventions */
	
	register char *s, *lastslash;
	char *dir, *name, temp[100];
	int i;
	FILE *fp;
	
	name = *namep;
	if(name[0] == '/')
		return( fopen(name, "r") );
	
	dir = basefile;
	for(i = filedepth ; i>=0 ; --i)
		if( filemacs[i] == NULL)
			{
			dir = filenames[i];
			break;
			}

	lastslash = NULL;
	for(s = dir ; *s ; ++s)
		if(*s == '/')
			lastslash = s;
	if(lastslash)
		{
		*lastslash = '\0';
		sprintf(temp, "%s/%s", dir, name);
		*lastslash = '/';
		if( fp = fopen(temp, "r") )
			*namep = temp;
		}
	else
		fp = fopen(name, "r");
	
	if(fp == NULL)
		{
		sprintf(temp, "/usr/include/%s", name);
		fp = fopen(temp, "r");
		*namep = temp;
		}
	return(fp);

#endif
}

