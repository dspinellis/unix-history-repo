%{
/*******************************************************************
*                                                                  *
*    File: CIFPLOT/pat.l                                           *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#define STRING  2
#define INTEGER 1

#undef input()
#undef unput(x)
#define input()  (((pat_ch = getc(patfile)) == EOF) ? 0:pat_ch)
#define unput(x)  ungetc(x,patfile)

extern int patval;
extern char *patstr;
extern FILE *patfile;

char pat_ch;
int lncnt = 1;
%}

%%
\"[^\"\n]*\"	{ patstr = (char *) unquote(&(yytext[0])); return(STRING); }
0[0-7]*		{ sscanf(&(yytext[0]),"%o",&patval); return(INTEGER); }
0x[0-9a-fA-F]+	{ sscanf(&(yytext[2]),"%x",&patval); return(INTEGER); }
0X[0-9a-fA-F]+	{ sscanf(&(yytext[2]),"%x",&patval); return(INTEGER); }
[1-9][0-9]*	{ sscanf(&(yytext[0]),"%d",&patval); return(INTEGER); }
[\ \t","";""{""}"]+	{}
\n		{ lncnt++; }
.		{ fprintf(stderr,"Unknown character(%o) on line %d in pattern file\n",yytext[0],lncnt);
		  abort();	}
%%
