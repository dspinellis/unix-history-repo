%{
#ifndef lint
static	char *sccsid = "@(#)gram.y	4.1 (Berkeley) 83/09/07";
#endif

#include "defs.h"

struct	block *last;

%}

%term EQUAL 1
%term ARROW 2
%term LP 3
%term RP 4
%term NAME 5
%term INSTALL 6
%term VERIFY 7
%term NOTIFY 8
%term EXCEPT 9

%union
	{
	struct block *blk;
	}

%type <blk> NAME, INSTALL, VERIFY, NOTIFY, EXCEPT, namelist, names, cmdlist, cmd

%%

file:		  /* VOID */
		| file command
		;

command:	  NAME EQUAL namelist = {
			$1->b_args = $3;
			(void) lookup($1->b_name, 1, $1);
		}
		| namelist ARROW namelist cmdlist = {
			docmd($1, $3, $4);
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
			$$ = last = NULL;
		}
		| names NAME = {
			if (last == NULL)
				$$ = last = $2;
			else {
				last->b_next = $2;
				last = $2;
				$$ = $1;
			}
		}
		;

cmdlist:	  /* VOID */ {
			$$ = last = NULL;
		}
		| cmdlist cmd = {
			if (last == NULL)
				$$ = last = $2;
			else {
				last->b_next = $2;
				last = $2;
				$$ = $1;
			}
		}
		;

cmd:		  INSTALL NAME = {
			$1->b_name = $2->b_name;
			free($2);
			$$ = $1;
		}
		| VERIFY NAME = {
			$1->b_name = $2->b_name;
			free($2);
			$$ = $1;
		}
		| NOTIFY namelist = {
			$1->b_args = $2;
			$$ = $1;
		}
		| EXCEPT namelist = {
			$1->b_args = expand($2);
			$$ = $1;
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
	register struct block *bp;
	
	for (;;) {
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
			continue;

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
		}
		/*
		 * Start of a name.
		 */
		cp1 = yytext;
		cp2 = &yytext[INMAX - 1];
		for (;;) {
			if (cp1 >= cp2) {
				fatal("input line too long\n");
				break;
			}
			*cp1++ = c;
			c = getc(fin);
			if (c == EOF || any(c, " \t()=\n")) {
				ungetc(c, fin);
				break;
			}
		}
		*cp1 = '\0';
		yylval.blk = bp = ALLOC(block);
		if (bp == NULL)
			fatal("ran out of memory\n");
		if (!strcmp(yytext, "install"))
			c = INSTALL;
		else if (!strcmp(yytext, "verify"))
			c = VERIFY;
		else if (!strcmp(yytext, "notify"))
			c = NOTIFY;
		else if (!strcmp(yytext, "except"))
			c = EXCEPT;
		else
			c = NAME;
		bp->b_type = c;
		bp->b_next = bp->b_args = NULL;
		if (c == NAME) {
			c = strlen(yytext) + 1;
			bp->b_name = cp1 = (char *) malloc(c);
			if (cp1 == NULL)
				fatal("ran out of memory\n");
			for (cp2 = yytext; *cp1++ = *cp2++; )
				;
		} else
			bp->b_name = NULL;
		return(bp->b_type);
	}
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
