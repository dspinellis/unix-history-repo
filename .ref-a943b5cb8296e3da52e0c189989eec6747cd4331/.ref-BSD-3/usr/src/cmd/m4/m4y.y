%{
extern long	evalval;
#define	YYSTYPE	long
%}

%term DIGITS
%left '|'
%left '&'
%right '!'
%nonassoc GT GE LT LE NE EQ
%left '+' '-'
%left '*' '/' '%'
%right POWER
%right UMINUS
%%

s	: e	={ evalval = $1; }
	|	={ evalval = 0; }
	;

e	: e '|' e	={ $$ = ($1!=0 || $3!=0) ? 1 : 0; }
	| e '&' e	={ $$ = ($1!=0 && $3!=0) ? 1 : 0; }
	| '!' e		={ $$ = $2 == 0; }
	| e EQ e	={ $$ = $1 == $3; }
	| e NE e	={ $$ = $1 != $3; }
	| e GT e	={ $$ = $1 > $3; }
	| e GE e	={ $$ = $1 >= $3; }
	| e LT e	={ $$ = $1 < $3; }
	| e LE e	={ $$ = $1 <= $3; }
	| e '+' e	={ $$ = ($1+$3); }
	| e '-' e	={ $$ = ($1-$3); }
	| e '*' e	={ $$ = ($1*$3); }
	| e '/' e	={ $$ = ($1/$3); }
	| e '%' e	={ $$ = ($1%$3); }
	| '(' e ')'	={ $$ = ($2); }
	| e POWER e	={ for ($$=1; $3-->0; $$ *= $1); }	
	| '-' e %prec UMINUS	={ $$ = $2-1; $$ = -$2; }
	| '+' e %prec UMINUS	={ $$ = $2-1; $$ = $2; }
	| DIGITS	={ $$ = evalval; }
	;

%%

yylex() {
	extern char *pe;

	while (*pe==' ' || *pe=='\t' || *pe=='\n')
		pe++;
	switch(*pe) {
	case '\0':
	case '+':
	case '-':
	case '/':
	case '%':
	case '(':
	case ')':
		return(*pe++);
	case '^':
		pe++;
		return(POWER);
	case '*':
		return(peek('*', POWER, '*'));
	case '>':
		return(peek('=', GE, GT));
	case '<':
		return(peek('=', LE, LT));
	case '=':
		return(peek('=', EQ, EQ));
	case '|':
		return(peek('|', '|', '|'));
	case '&':
		return(peek('&', '&', '&'));
	case '!':
		return(peek('=', NE, '!'));
	default:
		evalval = 0;
		while (*pe >= '0' && *pe <= '9')
			evalval = evalval*10 + *pe++ - '0';
		return(DIGITS);
	}
}

peek(c, r1, r2)
{
	if (*++pe != c)
		return(r2);
	++pe;
	return(r1);
}

yyerror(s)
char *s;
{
}
