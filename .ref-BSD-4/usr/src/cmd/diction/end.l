%{
#include <stdio.h>
#include "names.h"
#include "ehash.c"
#include "edict.c"
#define OUT1(c) putchar(c); putchar(':'); for(i=yyleng-1;i>=0;)putchar(yytext[i--])
int i;
int nomin = 0;
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
	OUT1(ADJ_ADV);
	}
^eca.+	{
	look(ace,3,NOUN_VERB);
	}
^eci.+	{
	look(ice,3,NOUN_VERB);
	}
^ecne.+	{
	if(look(ence,4,NOUN))nomin++;
	}
^ecna.+	{
	if(look(ance,4,NOUN))nomin++;
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
	if(look(ure,3,NOUN))nomin++;
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
	if(look(ion,3,NOUN))nomin++;
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
^s.+	{
	if(yytext[yyleng-1] >= 'A' && yytext[yyleng-1]<= 'Z'){
		OUT1(NOUN);
	}
	else {
		OUT1(NV_PL);
	}
	}
^tna.+	{
	look(ant,3,NOUN_ADJ);
	}
^tnem.+	{
	if(look(ment,4,NOUN))nomin++;
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
	if(yytext[yyleng-1] >= 'A' && yytext[yyleng-1] <= 'Z'){
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
:	{
	egetd();
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
	if(sch>= 'A' && sch <= 'Z'){
		yytext[yyleng-1] = sch -'A' +'a';
		in=1;
	}
	if((*f)(&yytext[n],1,0) != 0){
		nn = (*f)(&yytext[n],1,0);
		ret = 0;
	}
	else {
		nn = nc;
		ret = 1;
	}
	if(in==1){
		if(nn == NOUN_VERB)nn=NOUN;
		yytext[yyleng-1]=sch;
	}
	OUT1(nn);
	return(ret);
}
yywrap(){
	printf(";%d\n",nomin);
	return(1);
}
