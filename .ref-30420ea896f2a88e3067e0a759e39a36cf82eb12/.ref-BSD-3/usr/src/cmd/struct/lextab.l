%{
#include "y.tab.h"
#include "b.h"
#undef	input
#define input()	ninput()
#undef	unput
#define unput(c)	nunput(c)
extern int yylval;
#define xxbpmax	1700
char xxbuf[xxbpmax + 2];
int xxbp = -1;
#define xxunmax	200
char xxunbuf[xxunmax + 2];
int xxunbp = -1;


int blflag;
%}

D	[0-9]
A	[0-9a-z]
L	[a-z]
SP	[^0-9a-z]

%%

%{
char *xxtbuff;
int xxj, xxn, xxk;
char *xxp;
%}
[=/,(]{D}+[h]			{
				blflag = 1;
				sscanf(&yytext[1],"%d",&xxn);
				xxtbuff = malloc(2*xxn+3);
				for (xxj = xxk = 1; xxj <= xxn; ++xxj)
					{
					xxtbuff[xxk] = ninput();
					if (xxtbuff[xxk] == '"')
						xxtbuff[++xxk] = '"';
					++xxk;
					}
				xxtbuff[0] = xxtbuff[xxk++] = '"';
				xxtbuff[xxk] = '\0';
				putback(xxtbuff);
				free(xxtbuff);

				backup(yytext[0]);
				blflag = 0;
				xxbp = -1;
				}
IF			{fixval(); xxbp = -1; return(xxif);}
ELSE			{fixval(); xxbp = -1; return(xxelse);}
REPEAT			{fixval(); xxbp = -1; return(xxrept); }
WHILE			{fixval(); xxbp = -1; return(xxwhile); }
UNTIL			{ fixval(); xxbp = -1; return(xxuntil); }
DO			{fixval(); xxbp = -1; return(xxdo); }
SWITCH			{fixval(); xxbp = -1; return(xxswitch); }
CASE			{fixval(); xxbp = -1; return(xxcase); }
DEFAULT			{fixval(); xxbp = -1; return(xxdefault); }
END			{fixval(); xxbp = -1; return(xxend); }

".true."		|
".false."		|

{L}{A}*		{fixval(); xxbp = -1; return(xxident); }
~{D}+			{xxbuf[0] = ' '; fixval(); xxbp = -1; return(xxnum); }
{D}+/"."(ge|gt|le|lt|eq|ne|not|or|and)"."	|
{D}+\.?			|
{D}+\.?[de][+-]?{D}+		|
{D}*\.{D}+[de][+-]?{D}+		|
{D}*\.{D}+			{fixval(); xxbp = -1; return(xxnum); }

".gt."			{ putback(">"); xxbp = -1; }
".ge."			{ putback(">=");xxbp = -1; }
".lt."			{ putback("<"); xxbp = -1; }
".le."			{ putback("<="); xxbp = -1; }
".eq."			{ putback("=="); xxbp = -1; }
".ne."			{ putback("!="); xxbp = -1; }
".not."			{ putback("!"); xxbp = -1; }
".or."			{ putback("||"); xxbp = -1; }
".and."			{ putback("&&"); xxbp = -1; }
">="		{fixval(); xxbp = -1;  return(xxge);  }
"<="		{fixval(); xxbp = -1;  return(xxle); }
==			{fixval(); xxbp = -1; return(xxeq); }
!=			{fixval(); xxbp = -1; return(xxne); }
"||"			{fixval(); xxbp = -1; return('|'); }
"&&"			{fixval(); xxbp = -1;  return('&'); }
"**"			{fixval(); xxbp = -1; return('^'); }

#.*			{fixval(); xxbp = -1; return(xxcom); }
\"([^"]|\"\")*\"		{fixval(); xxbp = -1; return(xxstring); }
'([^']|'')*'				{
					fixval();
					xxp = yylval;
					xxn = slength(xxp);
					xxtbuff = malloc(2*xxn+1);
					xxtbuff[0] = '"';
					for (xxj = xxk = 1; xxj < xxn-1; ++xxj)
						{
						if (xxp[xxj] == '\'' && xxp[++xxj] == '\'')
							xxtbuff[xxk++] = '\'';
						else if (xxp[xxj] == '"')
							{
							xxtbuff[xxk++] = '"';
							xxtbuff[xxk++] = '"';
							}
						else
							xxtbuff[xxk++] = xxp[xxj];
						}
					xxtbuff[xxk++] = '"';
					xxtbuff[xxk] = '\0';
					free(xxp);
					yylval = xxtbuff;
					xxbp = -1;
					return(xxstring);
					}

^\n		xxbp = -1;
\n		{xxbp = -1; if (newflag) {fixval(); return('\n'); }  }
{SP}		{fixval(); xxbp = -1; return(yytext[0]); }

%%

rdchar()
	{
	int c;
	if (xxunbp >= 0)
		return(xxunbuf[xxunbp--]);
	c = getchar();
	if (c == EOF) return('\0');
	else return((char)c);
	}

backup(c)
char c;
	{
	if (++xxunbp > xxunmax)
		{
		xxunbuf[xxunmax + 1] = '\0';
		error("RATFOR beautifying; input backed up too far during lex:\n",
			xxunbuf,"\n");
		}
	xxunbuf[xxunbp] = c;
	}

nunput(c)
char c;
	{
	backup(c);
	if (xxbp < 0) return;
	if (c != xxbuf[xxbp])
		{
		xxbuf[xxbp + 1] = '\0';
		error("RATFOR beautifying; lex call of nunput with wrong char:\n",
			xxbuf,"\n");
		}
	for ( --xxbp; xxbp >= 0 && (xxbuf[xxbp] == ' ' || xxbuf[xxbp] == '\t'); --xxbp)
		backup(xxbuf[xxbp]);
	xxbuf[xxbp+1] = '\0';
	}

ninput()
	{
	char c,d;
	if (blflag) c = rdchar();
	else
		while ( (c = rdchar()) == ' ' || c == '\t')
		addbuf(c);
	if (c != '\n')
		return(addbuf(c));
	while ( (d = rdchar()) == ' ' || d == '\t');
	if (d == '&')
		return(ninput());
	backup(d);
	return(addbuf('\n'));
	}

addbuf(c)
char c;
	{
	if (++xxbp > xxbpmax)
		{
		xxbuf[xxbpmax +1] = '\0';
		error("RATFOR beautifying; buffer xxbuf too small for token beginning:\n",
			xxbuf,"\n");
		}
	xxbuf[xxbp] = c;
	xxbuf[xxbp + 1] = '\0';
	return(c);
	}


fixval()
	{
	int i, j, k;
	for (j = 0; xxbuf[j] == ' ' || xxbuf[j] == '\t'; ++j);
	for (k = j; xxbuf[k] != '\0'; ++k);
	for (--k; k > j && xxbuf[k] == ' ' || xxbuf[k]  == '\t'; --k);
	xxbuf[k+1] = '\0';
	i = slength(&xxbuf[j]) + 1;
	yylval = malloc(i);
	str_copy(&xxbuf[j],yylval,i);
	}



putback(str)
char *str;
	{
	int i;
	for (i = 0; str[i] != '\0'; ++i);
	for (--i; i >= 0; --i)
		backup(str[i]);
	}

