%{
/* break out words, output cap + word(inverted) */
#include <stdio.h>
#define OUT()	for(i=yyleng-1;i>=0; i--)putchar(yytext[i]); putchar('\n')
#define OUT1(nam)	printf("%c:%s\n",nam,yytext)
#define OUTN(string)	printf("%s\n",string)
#include "names.h"
#include "nhash.c"
#include "dict.c"
#include "ydict.c"
char nt[]  = "D:n't";
char qs[]  = "c:'s";
char fin[]  = "E:.";
int i,j;
int dot = 0;
int first  = 1;
int qflg,nflg;
int cap  = 0;
%}
%p 3000
%a 2500

L	[a-z]
N	[0-9]
C	[A-Z]

%%
(St|Dr|Drs|Mr|Mrs|Ms)"."	{
		OUT1(NOUN);
		}
{C}{L}*'[s]	{
		pos(1);
		if(first==1)first=0;
		}
{C}+['][s]*	{
		OUT1(POS);
		}
(({C}+{L}*)|({C}*{L}+))+([-](({C}*{L}+)|({C}+{L}*))+)+	{
		OUT1(NOUN_ADJ);
		}
{C}{C}+	{
		if((i=input()) == 's'){
			yytext[yyleng++] = 's';
			yytext[yyleng] = '\0';
			OUT1(NOUN);
		}
		else {
			unput(i);
			for(i=0;i<yyleng;i++)yytext[i]+= 'a' - 'A';
				goto wd;
		}
		}
[LD][']{C}{L}*	{
		OUT1(NOUN_ADJ);
		}
{C}{L}*	{
		if(first==1)
			first=0;
		else cap = 1;
		if(yyleng==1 && yytext[0] == 'I'){
			cap = 0;
			goto wd;
		}
		yytext[0]+= 'a' - 'A';
		goto wd;
	}
({N}+[-]{N}+[-]*)+	{
		OUT1(NOUN_ADJ);
		}
({N}+[-]*{L}+[-]*)+	{
		OUT1(NOUN_ADJ);
		}
({N}*[,])*({N}+".")+[ \t\n]+{C}	{
		for(i=yyleng-1;i>0;i--)
			if(yytext[i] == '.')break;
		unput(yytext[yyleng-1]);
		yytext[i] = '\0';
		OUT1(NOUN_ADJ);
		OUTN(fin);
		first = 1;
	}
[ \t`][a-zA-Z0-9.]*("\/"[a-zA-Z0-9]+"."*)+[']*	{
		if(yytext[yyleng-1] == '.')dot=1;
		OUT1(NOUN_ADJ);
		}
{N}+([,]{N}+)*("."{N}+)*[']*[s]*	{
	OUT1(NOUN_ADJ);
	}
{N}*([,]{N}+)*("."{N}+)+[']*[s]*	{
	OUT1(NOUN_ADJ);
	}
{N}+([,]{N}+)*("."{N}*)*[']*[s]*	{
	if(yytext[yyleng-1] == '.')dot=1;
	OUT1(NOUN_ADJ);
	}
{L}+[-]*{N}+	{
		OUT1(NOUN_ADJ);
		}
{C}+[-]*{N}+	{
	OUT1(NOUN_ADJ);
	}
{N}+[-]+{C}+	{
	OUT1(NOUN_ADJ);
		}
{N}+[%]		{
		OUT1(NOUN_ADJ);
		}
"$"{N}+([,]{N}+)*("."{N}*)*	{
		if(yytext[yyleng-1] == '.')dot=1;
		OUT1(NOUN);
		}
[Aa]"."[ ]*[Mm]"."	{
		OUT1(ADJ_ADV);
		}
[Pp]"."[ ]*[Mm]"."	{
		OUT1(ADJ_ADV);
		}
"a."[ ]*"d."	{
		OUT1(ADJ_ADV);
		}
"b."[ ]*"c."	{
		OUT1(ADJ_ADV);
		}
"i."[ ]*"e."	{
		OUT1(PREP);
		}
"e."[ ]*"g."	{
		OUT1(PREP);
		}
"etc."[ \n]*[,)]*	{
		i = yytext[4];
		yytext[4] = '\0';
		OUT1(NOUN);
		yytext[4] = i;
		yytext[0] = yytext[yyleng-1];
		yytext[1] = '\0';
		if(yytext[0] == ',' || yytext[0] == ')')
			OUT1(',');
		else {
			OUTN(fin);
			first = 1;
		}
	}
"et al."	{
		OUT1(NOUN);
		}
[Nn][Oo][s]*"."	{
		OUT1(NOUN_ADJ);
		}
[Ff]ig[s]*"."	{
		OUT1(NOUN_ADJ);
		}
[Dd]ept[s]*"."	{
		OUT1(NOUN_ADJ);
	}
[Ee]q"."	{
		OUT1(NOUN_ADJ);
		}
dB"."	{
		OUT1(NOUN_ADJ);
		}
vs"."	{
	OUT1(PREP);
	}
in"."[ \n]*{C}	{
		unput(yytext[yyleng-1]);
		yytext[2] = '\0';
		OUT1(PREP);
		OUTN(fin);
		first = 1;
		}
(in|ft|yr|ckts|mi)"."	{
		OUT1(NOUN_ADJ);
		}
Ph"."[ ]*[Dd]"."	{
		OUT1(ADJ);
		}
[Jj]r"."	{
	OUT1(ADJ);
	}
[Cc]h"."	{
		OUT1(NOUN_ADJ);
	}
[Rr]ef[s]*"."	{
		OUT1(NOUN_ADJ);
	}
Inc"."	{
		OUT1(ADJ);
	}
[A-Z]"."	{
		dot=1;
		OUT1(NOUN);
		}
can't		{
		yytext[3]='\0';
		yyleng -= 2;
		nflg=1;
		goto wd;
		}
won't		{
		OUT1('X');
		}
{L}+n't		{
		nflg=1;
		yytext[yyleng-3]='\0';
		yyleng -= 3;
		goto wd;
		}
[A-Z]{L}+n't	{
		yytext[0]+= 'a' - 'A';
		nflg=1;
		yytext[yyleng-3]='\0';
		yyleng -= 3;
		goto wd;
		}
o'clock	{
		OUT1(ADV);
	}
{L}+'[s]	{
		pos(0);
		}
'll	{
		OUT1(lookup("will",1,0));
	}
've	{
		OUT1(lookup("have",1,0));
	}
're	{
		OUT1(lookup("are",1,0));
	}
'd	{
		OUT1(lookup("had",1,0));
	}
'm	{
		OUT1(lookup("am",1,0));
	}
'ld	{
		OUT1(lookup("would",1,0));
	}
{L}+	{
wd:
	if((j = lookup(yytext,1,0)) != 0){
		first=0;
		if(cap){
			yytext[0] += 'A' - 'a';
			cap = 0;
			if(dot)OUTN(fin);
		}
		dot=0;
		OUT1(j);
		if(nflg==1){
			nflg=0;
			OUTN(nt);
		}
	}
	else{
		first = dot=0;
		if(yytext[yyleng-1] == 'y' && cap == 0){
			switch(yytext[yyleng-2]){
			case 'c': look(cy,yyleng-2,NOUN);
					break;
			case 'f': look(fy,yyleng-2,VERB);
					break;
			case 'l': look(ly,yyleng-2,ADV);
					break;
			case 'g': if(yytext[yyleng-3] == 'o'){
					OUT1(NOUN);
					break;
				}
				 look(gy,yyleng-2,ADJ);
				break;
			case 'r':	switch(yytext[yyleng-3]){
				case 'a': look(ary,yyleng-3,ADJ);
						break;
				case 'o': look(ory,yyleng-3,ADJ);
						break;
				case 'e': look(ery,yyleng-3,NOUN);
						break;
				default: look(ry,yyleng-2,NOUN);
				}
				break;
			case 't': if(yytext[yyleng-3] == 'i')look(ity,yyleng-3,NOUN);
				else look(ty,yyleng-2,ADJ);
				break;
			default: OUT();
			}
		}
		else {
			if(cap){
				yytext[0] += 'A' - 'a';
				cap = 0;
				OUT1(NOUN_ADJ);
			}
			else {
				OUT();
			}
		}
	}
	}
[\n]	;
[ ]+	;
[\t]+	;
";"	{
	OUT1(';');
	first=1;
	}
(\"|`|')+	{
	if(dot){
		OUTN(fin);
		dot=0;
	}
	if(qflg==1){
		qflg=0;
		OUT1('"');
	}
	else {
		qflg=1;
		first=1;
		OUT1('"');
	}
	}
".\""	{
	qflg=0;
	first=1;
	OUT1(END);
	}
"..."	{
	OUT1(',');
	}
"/."	{
	first = 1;
	OUT1(END);
	}
"."	{
	first=1;
	OUT1(END);
	}
"!\""	{
	qflg=0;
	first=1;
	OUT1(END);
	}
"!"	{
	first=1;
	OUT1(END);
	}
"?\""	{
	qflg=0;
	first=1;
	OUT1(END);
	}
"?"	{
	first=1;
	OUT1(END);
	}
":"	{
	OUT1(',');
	first=1;
	}
[-]+	{
	OUT1(',');
	first=1;
	}
","	{
	OUT1(',');
	}
(\[|\(|\{|\]|\)|\})	{
	OUT1(',');
	}
.	{
/*	fprintf(stderr,"nwords funny char: %c\n",yytext[0])*/ ;
	}
%%
look(f,n,cc)
char (*f)();
int n;
char cc;
{
	int nn;
	char  save;
	save=yytext[n];
	yytext[n] = '\0';
	nn=(*f)(yytext,1,0);
	yytext[n] = save;
	if(nn != 0){
		OUT1(nn);
	}
	else {
		OUT1(cc);
	}
}
pos(flg){
	int ii,j;
	if(flg==1)yytext[0] += 'a' - 'A';
	for(ii=yyleng-1;yytext[ii] != '\''; ii--);
	yytext[ii] = '\0';
	if((j=lookup(yytext,1,0)) != 0){
		yyleng = ii;
		OUT1(j);
		OUTN(qs);
	}
	else{
		if(flg==1)yytext[0] += 'A' - 'a';
		yytext[ii] = '\'';
		OUT1(POS);
	}
}
char	*filename="-";

main(argc,argv)
int	argc;
char	*argv[];
{
	register int rc=0;
	putchar(':'); putchar('\n');
	getd();
	ygetd();
	if(argc<=1) {
		yylex();
	}else{
		while(argc>1) {
			if(freopen(argv[1],"r",stdin)==NULL) {
				fprintf(stderr,"%s: cannot open\n", argv[1]);
				rc++;
			}else{
				filename=argv[1];
				yylex();
			}
			argc--; argv++;
		}
	}
	return(rc);
}
