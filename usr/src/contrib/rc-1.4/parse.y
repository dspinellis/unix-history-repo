/* parse.y */

/*
 * Adapted from rc grammar, v10 manuals, volume 2.
 */

%{
#include "rc.h"
#ifndef lint
#define lint		/* hush up gcc -Wall, leave out the dumb sccsid's. */
#endif
static Node *star, *nolist;
Node *parsetree;	/* not using yylval because bison declares it as an auto */
%}

%token ANDAND BACKBACK BANG CASE COUNT DUP ELSE END FLAT FN FOR IF IN
%token OROR PIPE REDIR SREDIR SUB SUBSHELL SWITCH TWIDDLE WHILE WORD HUH

%left WHILE ')' ELSE
%left ANDAND OROR '\n'
%left BANG SUBSHELL
%left PIPE
%right '$' 
%left SUB
/*
*/

%union {
	struct Node *node;
	struct Redir redir;
	struct Pipe pipe;
	struct Dup dup;
	struct Word word;
	char *keyword;
}

%type <redir> REDIR SREDIR
%type <pipe> PIPE
%type <dup> DUP
%type <word> WORD
%type <keyword> keyword
%type <node> assign body brace case cbody cmd cmdsa cmdsan comword epilog
	     first line nlwords paren redir sword simple iftail word words

%start rc

%%

rc	: line end		{ parsetree = $1; YYACCEPT; }
	| error end		{ yyerrok; parsetree = NULL; YYABORT; }

/* an rc line may end in end-of-file as well as newline, e.g., rc -c 'ls' */
end	: END	/* EOF */	{ if (!heredoc(1)) YYABORT; } /* flag error if there is a heredoc in the queue */
	| '\n'			{ if (!heredoc(0)) YYABORT; } /* get heredoc on \n */

/* a cmdsa is a command followed by ampersand or newline (used in "line" and "body") */
cmdsa	: cmd ';'
	| cmd '&'		{ $$ = ($1 != NULL ? mk(nNowait,$1) : $1); }

/* a line is a single command, or a command terminated by ; or & followed by a line (recursive) */
line	: cmd
	| cmdsa line		{ $$ = ($1 != NULL ? mk(nBody,$1,$2) : $2); }

/* a body is like a line, only commands may also be terminated by newline */
body	: cmd
	| cmdsan body		{ $$ = ($1 == NULL ? $2 : $2 == NULL ? $1 : mk(nBody,$1,$2)); }

cmdsan	: cmdsa
	| cmd '\n'		{ $$ = $1; if (!heredoc(0)) YYABORT; } /* get h.d. on \n */

brace	: '{' body '}'		{ $$ = $2; }

paren	: '(' body ')'		{ $$ = $2; }

assign	: first '=' word	{ $$ = mk(nAssign,$1,$3); }

epilog	:			{ $$ = NULL; }
	| redir epilog		{ $$ = mk(nEpilog,$1,$2); }

/* a redirection is a dup (e.g., >[1=2]) or a file redirection. (e.g., > /dev/null) */
redir	: DUP			{ $$ = mk(nDup,$1.type,$1.left,$1.right); }
	| REDIR word		{ $$ = mk(nRedir,$1.type,$1.fd,$2);
				  if ($1.type == rHeredoc && !qdoc($2, $$)) YYABORT; /* queue heredocs up */
				}
	| SREDIR word		{ $$ = mk(nRedir,$1.type,$1.fd,$2);
				  if ($1.type == rHeredoc && !qdoc($2, $$)) YYABORT; /* queue heredocs up */
				}

case	: CASE words ';'	 		{ $$ = mk(nCase, $2); }
	| CASE words '\n'	 		{ $$ = mk(nCase, $2); }

cbody	: cmd					{ $$ = mk(nCbody, $1, NULL); }
	| case cbody				{ $$ = mk(nCbody, $1, $2); }
	| cmdsan cbody				{ $$ = mk(nCbody, $1, $2); }

iftail	: cmd		%prec ELSE
	| brace ELSE optnl cmd			{ $$ = mk(nElse,$1,$4); }

cmd	: /* empty */	%prec WHILE		{ $$ = NULL; }
	| simple
	| brace epilog				{ $$ = mk(nBrace,$1,$2); }
	| IF paren optnl iftail			{ $$ = mk(nIf,$2,$4); }
	| FOR '(' word IN words ')' optnl cmd	{ $$ = mk(nForin,$3,$5,$8); }
	| FOR '(' word ')' optnl cmd		{ $$ = mk(nForin,$3,star,$6); }
	| WHILE paren optnl cmd			{ $$ = mk(nWhile,$2,$4); }
	| SWITCH '(' word ')' optnl '{' cbody '}' { $$ = mk(nSwitch,$3,$7); }
	| TWIDDLE optcaret word words		{ $$ = mk(nMatch,$3,$4); }
	| cmd ANDAND optnl cmd			{ $$ = mk(nAndalso,$1,$4); }
	| cmd OROR optnl cmd			{ $$ = mk(nOrelse,$1,$4); }
 	| cmd PIPE optnl cmd			{ $$ = mk(nPipe,$2.left,$2.right,$1,$4); }
	| redir cmd	%prec BANG		{ $$ = ($2 != NULL ? mk(nPre,$1,$2) : $1); }
	| assign cmd	%prec BANG		{ $$ = ($2 != NULL ? mk(nPre,$1,$2) : $1); }
	| BANG optcaret cmd			{ $$ = mk(nBang,$3); }
	| SUBSHELL optcaret cmd			{ $$ = mk(nSubshell,$3); }
	| FN words brace			{ $$ = mk(nNewfn,$2,$3); }
	| FN words				{ $$ = mk(nRmfn,$2); }

optcaret : /* empty */
	| '^'

simple	: first
	| simple word			{ $$ = ($2 != NULL ? mk(nArgs,$1,$2) : $1); }
	| simple redir			{ $$ = mk(nArgs,$1,$2); }

first	: comword
	| first '^' sword		{ $$ = mk(nConcat,$1,$3); }

sword	: comword
	| keyword			{ $$ = mk(nWord,$1, NULL); }

word	: sword
	| word '^' sword		{ $$ = mk(nConcat,$1,$3); }

comword	: '$' sword			{ $$ = mk(nVar,$2); }
	| '$' sword SUB words ')'	{ $$ = mk(nVarsub,$2,$4); }
	| COUNT sword			{ $$ = mk(nCount,$2); }
	| FLAT sword			{ $$ = mk(nFlat, $2); }
	| '`' sword			{ $$ = mk(nBackq,nolist,$2); }
	| '`' brace			{ $$ = mk(nBackq,nolist,$2); }
	| BACKBACK word	brace		{ $$ = mk(nBackq,$2,$3); }
	| BACKBACK word	sword		{ $$ = mk(nBackq,$2,$3); }
	| '(' nlwords ')'		{ $$ = $2; }
	| REDIR brace			{ $$ = mk(nNmpipe,$1.type,$1.fd,$2); }
	| WORD				{ $$ = ($1.w[0] == '\'') ? mk(nQword, $1.w+1, NULL) : mk(nWord,$1.w, $1.m); }

keyword	: FOR		{ $$ = "for"; }
	| IN		{ $$ = "in"; }
	| WHILE		{ $$ = "while"; }
	| IF		{ $$ = "if"; }
	| SWITCH	{ $$ = "switch"; }
	| FN		{ $$ = "fn"; }
	| ELSE		{ $$ = "else"; }
	| CASE		{ $$ = "case"; }
	| TWIDDLE	{ $$ = "~"; }
	| BANG		{ $$ = "!"; }
	| SUBSHELL	{ $$ = "@"; }

words	:		{ $$ = NULL; }
	| words word	{ $$ = ($1 != NULL ? ($2 != NULL ? mk(nLappend,$1,$2) : $1) : $2); }

nlwords :		{ $$ = NULL; }
	| nlwords '\n'
	| nlwords word	{ $$ = ($1 != NULL ? ($2 != NULL ? mk(nLappend,$1,$2) : $1) : $2); }

optnl	: /* empty */
	| optnl '\n'

%%

void initparse() {
	star = treecpy(mk(nVar,mk(nWord,"*",NULL)), ealloc);
	nolist = treecpy(mk(nVar,mk(nWord,"ifs",NULL)), ealloc);
}

