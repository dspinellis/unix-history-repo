/*	idyac.y	(CWI)	1.1	85/03/01	*/
%{
#include "ideal.h"

extern BOXPTR boxlist;

yyerror (message)
char *message;
{
	fprintf (stderr, "ideal: ");
	fprintf (stderr, "%s ", message);
	fprintf (stderr, "near line %d in file %s\n",
		lineno,
		filename
	);
}
%}
%token	BOX VAR BDLIST PUT CONN TO USING
%token	CONSTRUCT DRAW OPAQUE LEFT CENTER RIGHT AT
%token	NAME CONST STRING
%token	LINE CIRCLE ARC SPLINE
%token	PATH
%token	INTERIOR EXTERIOR
%token	LBRACE RBRACE

%left '^'
%left '+' '-'
%left '*' '/'
%left '['
%left UMINUS

%%
stuff	:figspec
	|	/*empty*/
	;

figspec	:boxdef	{
		forget (((BOXPTR)$1)->name);
		((BOXPTR) $1)->next = boxlist;
		boxlist = (BOXPTR) $1;
	}
	|figspec boxdef	{
		forget (((BOXPTR)$2)->name);
		((BOXPTR)$2)->next = boxlist;
		boxlist = (BOXPTR) $2;
	}
	;

boxdef	:body		{$$ = $1;}
	|BOX body	{$$ = $2;}
	;

body	:NAME LBRACE stmts RBRACE	{$$ = (int) boxgen ($1, (STMTPTR) $3);}
	|NAME LBRACE RBRACE		{$$ = (int) boxgen ($1, (STMTPTR) NULL);}
	;

stmts   :stmt		{$$ = $1;}
	|stmts stmt	{
		if ($2) {
			((STMTPTR)$2)->next = (STMTPTR)$1;
			$$ = $2;
		} else
			$$ = $1;
	}
	;

stmt	:varstmt ';'	{$$ = (int) stmtgen (VAR, (char *) $1);}
	|eqnstmt ';'	{$$ = (int) stmtgen ('=', (char *) $1);}
	|bdlist	';'	{$$ = (int) stmtgen (BDLIST, (char *) $1);}
	|putstmt	{$$ = (int) stmtgen (PUT, (char *) $1);}
	|connstmt ';'	{$$ = (int) stmtgen (CONN, (char *) $1);}
	|penstmt ';'	{if ($1) {
				$$ = (int) stmtgen (USING, (char *) $1);
			} else {
				$$ = (int) NULL;
			}
		}
	|drawstmt ';'	{$$ = (int) stmtgen (DRAW, (char *) $1);}
	|opaque	';'	{$$ = (int) stmtgen (OPAQUE, (char *) $1);}
	|strstmt ';'	{$$ = (int) stmtgen (STRING, (char *) $1);}
	|splstmt ';'	{$$ = (int) stmtgen (SPLINE, (char *) $1);}
	|';'		{$$ = (int) NULL;}
	|error ';'	{fprintf (stderr, "ideal: syntax error near line %d in file %s\n",
				lineno, filename);
			yyerrok;
			$$ = NULL;
	}
	;

varstmt	:VAR varlist	{$$ = (int) $2;}
	;

varlist	:NAME	{$$ = (int) namegen ($1);}
	|varlist ',' NAME	{
		NAMEPTR temp;
		temp = (NAMEPTR) namegen ($3);
		temp->next = (NAMEPTR)$1;
		$$ = (int) temp;
	}
	;

eqnstmt	:expr '=' expr		{$$ = (int) intlgen ('=', (EXPR) $1, (EXPR) $3);}
	|expr '~' expr		{$$ = (int) intlgen ('~', (EXPR) $1, (EXPR) $3);}
	|eqnstmt '=' expr	{$$ = (int) intlgen ('=', (EXPR) $1, (EXPR) $3);}
	|eqnstmt '~' expr	{$$ = (int) intlgen ('~', (EXPR) $1, (EXPR) $3);}
	;

bdlist	:BDLIST '=' exprlist	{$$ = (int) $3;}
	;

exprlist:expr		{$$ = (int) exprgen ((EXPR) $1);}
	|exprlist ',' expr	{
		EXPRPTR temp;
		temp = (EXPRPTR) exprgen ((EXPR) $3);
		temp->next = (EXPRPTR)$1;
		$$ = (int) temp;
	}
	;

putstmt	:putword NAME ':' body	{$$ = (int) putgen ($2, (BOXPTR) $4, $1);}
	|putword body		{$$ = (int) putgen (NULL, (BOXPTR) $2, $1);}
	|NAME ':' putword body	{$$ = (int) putgen ($1, (BOXPTR) $4, $3);}
	;

putword	:PUT		{$$ = $1;}
	|CONSTRUCT	{$$ = $1;}
	;

connstmt:CONN knotlist	{$$ = (int) $2;}
	;

penstmt	:CONN knotlist USING expr body '<' expr ',' expr '>'
		{if (!((EXPRPTR) $2)->next || ((EXPRPTR) $2)->next->next) {
			fprintf (stderr, "ideal: improper pen statement\n   >>>pen ignored\n");
			$$ = (int) NULL;
		} else {
			$$ = (int) pengen (
				((EXPRPTR) $2)->next->expr,
				((EXPRPTR) $2)->expr,
				(EXPR) $4,
				(EXPR) $7,
				(EXPR) $9,
				(BOXPTR) $5
			);
			tryfree(((EXPRPTR) $2)->next);
			tryfree((EXPRPTR) $2);
		}
	}
	;

drawstmt:DRAW NAME	{$$ = (int) miscgen ($2);}
	;

opaque	:OPAQUE	{$$ = (int) miscgen (INTERIOR);}
	|OPAQUE INTERIOR	{$$ = (int) miscgen (INTERIOR);}
	|OPAQUE EXTERIOR	{$$ = (int) miscgen (EXTERIOR);}
	;

strstmt	:LEFT STRING AT expr	{$$ = (int) strgen (LEFT, (char *) $2, (EXPR) $4);}
	|CENTER STRING AT expr	{$$ = (int) strgen (CENTER, (char *) $2, (EXPR) $4);}
	|STRING AT expr	{$$ = (int) strgen (CENTER, (char *) $1, (EXPR) $3);}
	|RIGHT STRING AT expr	{$$ = (int) strgen (RIGHT, (char *) $2, (EXPR) $4);}
	;

splstmt	:SPLINE knotlist	{$$ = (int) $2;}

knotlist:expr		{$$ = (int) exprgen ((EXPR) $1);}
	|knotlist TO expr	{
		EXPRPTR temp;
		temp = (EXPRPTR) exprgen ((EXPR) $3);
		temp->next = (EXPRPTR) $1;
		$$ = (int) temp;
	}
	;

expr	:'-' expr	%prec UMINUS
		{$$ = (int) intlgen ('-', (EXPR) NULL, (EXPR) $2);}
	|expr '+' expr	{$$ = (int) intlgen ('+', (EXPR) $1, (EXPR) $3);}
	|expr '-' expr	{$$ = (int) intlgen ('-', (EXPR) $1, (EXPR) $3);}
	|expr '*' expr	{$$ = (int) intlgen ('*', (EXPR) $1, (EXPR) $3);}
	|expr '/' expr	{$$ = (int) intlgen ('/', (EXPR) $1, (EXPR) $3);}
	|'^' expr	{$$ = (int) intlgen ('^', (EXPR) NULL, (EXPR) $2);}
	|pathname		{$$ = (int) extlgen ((NAMEPTR) $1);}
	|CONST			{$$ = $1;}
	|'(' expr ')'		{$$ = $2;}
	|'(' expr ',' expr ')'	{$$ = (int) intlgen (',', (EXPR) $2, (EXPR) $4);}
	|NAME '(' exprlist ')'	{$$ = (int) intlgen (NAME, (EXPR) $1, (EXPR) $3);}
	|expr '[' expr ',' expr ']'	{
		$$ = (int) bracket (
			(EXPR) $1,
			(EXPR) $3,
			(EXPR) $5
		);
	}
	;

pathname:NAME	{$$ = (int) namegen ($1);}
	|NAME '.' pathname	{
		NAMEPTR temp;
		temp = (NAMEPTR) namegen($1);
		temp->next = (NAMEPTR)$3;
		$$ = (int) temp;
	}
	;
