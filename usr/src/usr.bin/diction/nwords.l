%{
/* break out words, output cap + word(inverted) */

#ifndef lint
static char sccsid[] = "@(#)nwords.l	4.2	(Berkeley)	82/11/06";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#define OUT()	for(i=yyleng-1;i>=0; i--)putchar(yytext[i]); putchar('\n')
#define OUT1(nam)	printf("%c:%s\n",nam,yytext)
#define OUTN(string)	printf("%s\n",string)
#include "names.h"
#include "nhash.c"
#include "dict.c"
#include "ydict.c"
#include "abbrev.c"
char nt[]  = "D:n't";
char qs[]  = "c:'s";
char fin[]  = "E:.";
int NOCAPS = 0;		/* if set all caps are turned to lower case */
int i,j;
int dot = 0;
int first  = 1;
int qflg,nflg;
int cap  = 0;
%}
%p 3000
%a 3300
%o 4500

L	[a-z]
N	[0-9]
C	[A-Z]
A	[a-zA-Z]
P	[a-zA-Z0-9]

%%
^[.!].+[\n]	{
	if(dot){
		OUTN(fin);
		dot = 0;
		first = 1;
	}
	printf(":%s",yytext);
	}
May	{
		if(first == 0){
			OUT1(NOUN);
		}
		else {
			first = 0;
			yytext[0] = tolower(yytext[0]);
			cap = 1;
			goto wd;
		}
	}
"U.S."		{
		OUT1(NOUN);
		}
{C}{L}*'[s]	{
		pos(1);
		if(first==1)first=0;
		}
{C}+['][s]	{
		if(NOCAPS)
			for(i=0;i<yyleng;i++)
				if(isupper(yytext[i]))yytext[i] = tolower(yytext[i]);
		OUT1(POS);
		}
{P}+([-]{P}+)+	{
		if(NOCAPS)
			for(i=0;i<yyleng;i++)
				if(isupper(yytext[i]))yytext[i] = tolower(yytext[i]);
		OUT1(NOUN_ADJ);
		}
{C}{C}+	{
		if(NOCAPS)
			for(i=0;i<yyleng;i++)
				yytext[i] = tolower(yytext[i]);
		if((i=input()) == 's'){
			yytext[yyleng++] = 's';
			yytext[yyleng] = '\0';
			OUT1(PNOUN);
		}
		else {
			unput(i);
			if(!NOCAPS)
				for(i=0;i<yyleng;i++)yytext[i] = tolower(yytext[i]);
				goto wd;
		}
		}
[LD][']{C}{L}*	{
		if(NOCAPS){
			yytext[0] = tolower(yytext[0]);
			yytext[2] = tolower(yytext[2]);
		}
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
		yytext[0] = tolower(yytext[0]);
		goto wd;
	}
{N}":"{N}{N}	{
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
([hH]e"/"[sS]he)|([sS]he"/"[hH]e)		{
	if(NOCAPS)
		if(isupper(yytext[0]))yytext[0] = tolower(yytext[0]);
	OUT1(PRONS);
	}
([hH]is"/"[hH]er)|([hH]er"/"[hH]is)	{
	if(NOCAPS)
		if(isupper(yytext[0]))yytext[0] = tolower(yytext[0]);
	OUT1(POS);
	}
[ \t`]*[a-zA-Z0-9.]*("\/"[a-zA-Z0-9.]+)+[']*	{
		if(yytext[yyleng-1] == '.'){
			if(ahead() == 0)dot=1;
		}
		if(NOCAPS)
			for(i=0;i<yyleng;i++)
				if(isupper(yytext[i]))yytext[i] = tolower(yytext[i]);
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
({A}*{N}+{A}*)+	{
	if(input() == '.')
		ahead();
	if(NOCAPS)
		for(i=0;i<yyleng;i++)
			if(isupper(yytext[i]))yytext[i]=tolower(yytext[i]);
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
in"."[ \n]*{C}	{
		unput(yytext[yyleng-1]);
		yytext[2] = '\0';
		OUT1(PREP);
		OUTN(fin);
		first = 1;
		}
Ph"."[ ]*[Dd]"."	{
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
ain't		{
		OUT1('g');
		}
{L}+n't		{
		nflg=1;
		yytext[yyleng-3]='\0';
		yyleng -= 3;
		goto wd;
		}
[A-Z]{L}+n't	{
		yytext[0] = tolower(yytext[0]);
		nflg=1;
		yytext[yyleng-3]='\0';
		yyleng -= 3;
		goto wd;
		}
o'clock		{
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
			if(!NOCAPS)
				yytext[0] = toupper(yytext[0]);
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
				if(!NOCAPS)yytext[0] = toupper(yytext[0]);
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
{A}{A}+"."	{
		yytext[yyleng-1] = '\0';
		if((j=abbrev(yytext,1,0)) != 0){
			if(isupper(yytext[0])){
				if(NOCAPS)yytext[0] = tolower(yytext[0]);
				if(first == 1)first=0;
			}
			yytext[yyleng-1] = '.';
			OUT1(j);
		}
		else {
			j = ahead();
			if(j == 0)
				yyleng--;
			for(i=0;i<yyleng;i++)
				if(isupper(yytext[i])){
					yytext[i] = tolower(yytext[i]);
					if(i == 0)cap = 1;
					else cap = 0;
				}
			if(j == 0)goto wd;
			OUT1(NOUN_ADJ);
		}
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
	if(flg == 1)yytext[0] = tolower(yytext[0]);
	for(ii=yyleng-1;yytext[ii] != '\''; ii--);
	yytext[ii] = '\0';
	if((j=lookup(yytext,1,0)) != 0){
		yyleng = ii;
		OUT1(j);
		OUTN(qs);
	}
	else{
		if(flg==1 && !NOCAPS)yytext[0] = toupper(yytext[0]);
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
	getab();
	ygetd();
	if(argc<=1) {
		yylex();
		OUTN(fin);
	}else{
		while(argc>1) {
			if(freopen(argv[1],"r",stdin)==NULL) {
				fprintf(stderr,"%s: cannot open\n", argv[1]);
				rc++;
			}else{
				filename=argv[1];
				yylex();
				OUTN(fin);
			}
			argc--; argv++;
		}
	}
	return(rc);
}
ahead(){
	register int c;
	if(isalnum((c=input()))){
		yytext[yyleng++] = '.';
		while(!isspace((c=input() )))
			yytext[yyleng++] = c;
		yytext[yyleng] = '\0';
		unput(c);
		return(1);
	}
	unput(c);
	unput('.');
	return(0);
}
