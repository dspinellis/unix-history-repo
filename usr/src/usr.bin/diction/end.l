%{
#ifndef lint
static char sccsid[] = "@(#)end.l	4.2	(Berkeley)	82/11/06";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include "names.h"
#include "ehash.c"
#include "edict.c"
#define OUT1(c) putchar(c); putchar(':'); for(i=yyleng-1;i>=0;)putchar(yytext[i--])
#define POUT1(c) putchar(c); putchar(':'); for(i=yyleng-1;i>0;)putchar(yytext[i--])
int i;
int nomin = 0;
int NOCAPS = 0;		/*if set, all caps mapped to lower, plurals stripped*/
%}

C	[A-Z0-9a-z",;]
L	[a-zA-Z-]

%%
^{C}:.+	{
	ECHO;
	}
^ci.+	{
	look(ic,2,ADJ);
	}
^de.+	{
	look(ed,2,ED);
	}
^draw.+	{
	if(yytext[5] == 'E'){
		OUT1(NOUN_ADJ);
	}
	OUT1(ADJ_ADV);
	}
^eca.+	{
	look(ace,3,NOUN_VERB);
	}
^eci.+	{
	look(ice,3,NOUN_VERB);
	}
^ecne.+	{
	if(look(ence,4,NOM))nomin++;
	}
^ecna.+	{
	if(look(ance,4,NOM))nomin++;
	}
^ee.+	{
	look(ee,2,NOUN);
	}
^ega.+	{
	look(age,3,NOUN);
	}
^elba.+	{
	look(able,4,ADJ);
	}
^elbi.+	{
	look(ible,4,ADJ);
	}
^elc.+	{
	look(cle,3,NOUN);
	}
^eru.+	{
	look(ure,3,NOUN);
	}
^eti.+	{
	look(ite,3,NOUN);
	}
^evi.+	{
	look(ive,3,ADJ);
	}
^ezi.+	{
	look(ize,3,VERB);
	}
^gni.+	{
	look(ing,3,ING);
	}
^hsi.+	{
	look(ish,3,ADJ);
	}
^lac.+	{
	look(cal,3,ADJ);
	}
^lanoi.+	{
	look(ional,5,ADJ);
	}
^luf.+	{
	look(ful,3,ADJ);
	}
^msi.+	{
	OUT1(NOUN);
	}
^nam.*	{
	look(man,3,NOUN);
	}
^nem.*	{
	OUT1(NV_PL);
	}
^noi.+	{
	if(look(ion,3,NOM))nomin++;
	}
^pihs.+	{
	look(ship,4,NOUN);
	}
^ral.+	{
	look(lar,3,ADJ);
	}
^ronoh	{
	OUT1(NOUN_VERB);
	}
^ro.+	{
	OUT1(NOUN);
	}
^si.+	{
	look(is,2,NOUN);
	}
^ssel.+	{
	look(less,4,ADJ);
	}
^ssen.+	{
	look(ness,4,NOUN);
	}
^sse.+	{
	look(ess,3,NOUN);
	}
^ss.+	{
	look(ss,2,NOUN);
	}
^suo.+	{
	look(ous,3,ADJ);
	}
^su.+	{
	look(us,2,NOUN);
	}
^snoi.+	{
	if(look(ion,4,PNOUN))nomin++;
	}
^stnem.+	{
		if(look(ment,5,PNOUN))nomin++;
		}
^secne.+	{
		if(look(ence,5,PNOUN))nomin++;
		}
^secna.+	{
		if(look(ance,5,PNOUN))nomin++;
		}
^s.+	{
	if(isupper(yytext[yyleng-1])){
		if(NOCAPS){
			yytext[yyleng-1] = tolower(yytext[yyleng-1]);
			POUT1(PNOUN);
		}
		else { OUT1(PNOUN); }
	}
	else {
		if(NOCAPS){POUT1(NV_PL);}
		else{ OUT1(NV_PL); }
	}
	}
^tna.+	{
	look(ant,3,NOUN_ADJ);
	}
^tnem.+	{
	if(look(ment,4,NOM))nomin++;
	}
^tse.+	{
	look(est,3,ADJ);
	}
^tsi.+	{
	look(ist,3,NOUN);
	}
[,\n]	{
	putchar(yytext[0]);
	}
{L}+	{
	if(isupper(yytext[yyleng-1])){
		if(NOCAPS)
			yytext[yyleng-1] = tolower(yytext[yyleng-1]);
		if((yytext[0] == 'n' || yytext[0] == 'l') && yytext[1] == 'a'){
			OUT1(NOUN_ADJ);
		}
		else {
			OUT1(NOUN);
		}
	}
	else {
		OUT1(UNK);
	}
	}
^:[\n]	{
	egetd();
	}
^:.+[\n]	{
	printf("%s",yytext);
	}
%%
look(f,n,nc)
char (*f)();
int n;
char nc;
{
	int in,nn,ret;
	char sch;
	in=0;
	sch=yytext[yyleng-1];
	if(isupper(sch)){
		yytext[yyleng-1] = tolower(sch);
		in=1;
	}
	if((*f)(&yytext[n],1,0) != 0){
		nn = (*f)(&yytext[n],1,0);
		if(nc == PNOUN)
			if(nn == NOUN_VERB){
				if(in == 1)nn=PNOUN;
				else nn=NV_PL;
			}
		ret = 0;
	}
	else {
		nn = nc;
		ret = 1;
	}
	if(in==1){
		if(nn == NOUN_VERB)nn=NOUN;
		if(!NOCAPS)yytext[yyleng-1]=sch;	
	}
	if(nn==PNOUN && yytext[0] == 's' && NOCAPS){
		POUT1(nn);
	}
	else {
		OUT1(nn);
	}
	return(ret);
}
yywrap(){
	printf(";%d\n",nomin);
	return(1);
}
