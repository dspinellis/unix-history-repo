%{
#ifndef lint
static	char *sccsid = "@(#)gram.y	4.3 (Berkeley) 83/10/10";
#endif

#include "defs.h"

struct	block *lastn;
struct	block *lastc;

%}

%term EQUAL 1
%term LP 2
%term RP 3
%term ARROW 4
%term DCOLON 5
%term NAME 6
%term INSTALL 7
%term NOTIFY 8
%term EXCEPT 9
%term OPTION 10

%union {
	struct block *blk;
	int intval;
}

%type <blk> NAME, INSTALL, NOTIFY, EXCEPT, namelist, names, cmdlist, cmd
%type <intval> OPTION, options

%%

file:		  /* VOID */
		| file command
		;

command:	  NAME EQUAL namelist = {
			$1->b_args = $3;
			(void) lookup($1->b_name, $1, 1);
		}
		| namelist ARROW namelist cmdlist = {
			dohcmds($1, $3, $4);
		}
		| namelist DCOLON namelist cmdlist = {
			dofcmds($1, $3, $4);
		}
		| error
		;

namelist:	  NAME = {
			$$ = $1;
		}
		| LP names RP = {
			$$ = $2;
		}
		;

names:		  /* VOID */ {
			$$ = lastn = NULL;
		}
		| names NAME = {
			if (lastn == NULL)
				$$ = lastn = $2;
			else {
				lastn->b_next = $2;
				lastn = $2;
				$$ = $1;
			}
		}
		;

cmdlist:	  /* VOID */ {
			$$ = lastc = NULL;
		}
		| cmdlist cmd = {
			if (lastc == NULL)
				$$ = lastc = $2;
			else {
				lastc->b_next = $2;
				lastc = $2;
				$$ = $1;
			}
		}
		;

cmd:		  INSTALL options NAME = {
			register struct block *b;

			$1->b_options = $2;
			b = expand($3, 0);
			if (b == NULL || b->b_next != NULL)
				fatal("exactly one name allowed\n");
			$1->b_name = b->b_name;
			$$ = $1;
		}
		| NOTIFY namelist = {
			$1->b_args = expand($2, 1);
			$$ = $1;
		}
		| EXCEPT namelist = {
			$1->b_args = expand($2, 0);
			$$ = $1;
		}
		;
options:	  /* VOID */ = {
			$$ = 0;
		}
		| options OPTION = {
			$$ |= $2;
		}
		;

%%

int	yylineno = 1;
extern	FILE *fin;

yylex()
{
	static char yytext[INMAX];
	register int c;
	register char *cp1, *cp2;
	static char quotechars[] = "[]{}*?$";
	
again:
	switch (c = getc(fin)) {
	case EOF:  /* end of file */
		return(0);

	case '#':  /* start of comment */
		while ((c = getc(fin)) != EOF && c != '\n')
			;
		if (c == EOF)
			return(0);
	case '\n':
		yylineno++;
	case ' ':
	case '\t':  /* skip blanks */
		goto again;

	case '=':  /* EQUAL */
		return(EQUAL);

	case '(':  /* LP */
		return(LP);

	case ')':  /* RP */
		return(RP);

	case '-':  /* -> */
		if ((c = getc(fin)) == '>')
			return(ARROW);
		ungetc(c, fin);
		c = '-';
		break;

	case ':':  /* :: */
		if ((c = getc(fin)) == ':')
			return(DCOLON);
		ungetc(c, fin);
		c = ':';
	}
	cp1 = yytext;
	cp2 = &yytext[INMAX - 1];
	for (;;) {
		if (cp1 >= cp2) {
			fatal("input line too long\n");
			break;
		}
		if (c == '\\') {
			if ((c = getc(fin)) != EOF) {
				if (any(c, quotechars))
					c |= QUOTE;
			} else {
				*cp1++ = '\\';
				break;
			}
		}
		*cp1++ = c;
		c = getc(fin);
		if (c == EOF || any(c, " \t()=\n")) {
			ungetc(c, fin);
			break;
		}
	}
	*cp1 = '\0';
	if (yytext[0] == '-' && yytext[2] == '\0') {
		switch (yytext[1]) {
		case 'v':
			yylval.intval = VERIFY;
			return(OPTION);

		case 'w':
			yylval.intval = WHOLE;
			return(OPTION);

		case 'y':
			yylval.intval = YOUNGER;
			return(OPTION);
		}
	}
	if (!strcmp(yytext, "install"))
		c = INSTALL;
	else if (!strcmp(yytext, "notify"))
		c = NOTIFY;
	else if (!strcmp(yytext, "except"))
		c = EXCEPT;
	else
		c = NAME;
	yylval.blk = makeblock(c, yytext);
	return(c);
}

any(c, str)
	register int c;
	register char *str;
{
	while (*str)
		if (c == *str++)
			return(1);
	return(0);
}

/*
 * Error printing routine in parser.
 */
yyerror(s)
	char *s;
{
	extern int yychar;

	fprintf(stderr, "rdist: line %d: %s\n", yylineno, s);
}
