%{
#ifndef lint
static char sccsid[] = "@(#)hyr_lex.l	4.2 (Berkeley) 7/8/83";
static char origsccsid[] = "@(#)hyr_lex.l	2.1 Hyperchannel Routing Daemon 82/11/29";
#endif
%}

%Start GATE
ws	[ \t\n]*
s	[ \t\n]+
num	[0123456789abcdefABCDEF]+
name	[a-zA-Z.\-_][a-zA-Z0-9.\-_]*
other	[^ \t\na-zA-Z.\-_0-9;]

%{
#include "hyr_sym.h"
#include <ctype.h>
#undef output

struct sym *curgate = NULL;
struct sym *sym_head = NULL;

char name[32];

int lexdebug = 0;

extern struct sym *lookup();
%}

%%
^\*.*$	;
^{ws}$	;

{ws}direct{s}{name}{s}{num}{s}{num}{s}{num}{ws};	{
	register struct sym *s;
	unsigned dst, ctl, access;

	outwrap();
	sscanf(yytext, " direct %s %x %x %x ", name, &dst, &ctl, &access);
	if (lexdebug)
		fprintf(stderr, "DIRECT %s %04x %04x %04x\n", name, dst, ctl, access);
	if (s = lookup(name)) {
		s->s_dst = htons(dst);
		s->s_ctl = htons(ctl);
		s->s_access = htons(access);
		s->s_flags = HS_DIR;
	}
}

{ws}gateway{s}{name}	{
	register struct sym *s;

	outwrap();
	sscanf(yytext, " gateway %s ", name);
	if (lexdebug)
		fprintf(stderr, "GATEWAY %s ->", name);
	if (s = lookup(name)) {
		s->s_flags = HS_INDIR;
		curgate = s;
	}
	BEGIN GATE;
}

<GATE>{s}{name}		{
	register struct sym *s;

	outwrap();
	sscanf(yytext, " %s ", name);
	if (lexdebug)
		fprintf(stderr, " %s", name);
	if (s = lookup(name)) {
		if (curgate->s_ngate < 32)
			curgate->s_gate[curgate->s_ngate++] = s;
		s->s_flags |= HS_GATE;
	}
}

<GATE>{ws};	{
	outwrap();
	if (lexdebug)
		fprintf(stderr, "\n");
	curgate = NULL;
	BEGIN 0;
}
;	;	/* ignore extra ';'s */

%%

#include <sys/types.h>
#include <netdb.h>

struct sym *
lookup(cp)
	char *cp;
{
	struct sym *s;
	struct hostent *hp;
	extern char *malloc();

	for (s = sym_head; s != NULL; s = s->s_next) {
		if (strncmp(cp, s->s_name, sizeof(s->s_name)) == 0)
			return(s);
	}
	hp = gethostbyname(cp);
	if (hp == 0)
		printf("%s: warning, unknown host\n", cp);
	s = (struct sym *) malloc(sizeof *s);
	if (s != NULL) {
		s->s_lastok = 0;
		s->s_flags = 0;
		s->s_ngate = 0;
		s->s_dst = s->s_ctl = s->s_access = 0;
		if (hp)
			bcopy(hp->h_addr, (char *)&s->s_fulladdr, hp->h_length);
		s->s_next = sym_head;
		sym_head = s;
		strncpy(s->s_name, cp, sizeof(s->s_name));
	}
	return(s);
}

sym_print(s)
	register struct sym *s;
{
	register int i;

	printf("Host %s:\t%08x\n", s->s_name, s->s_fulladdr);
	if (s->s_flags & HS_DIR) {
		printf("\tdst %04x ctl %04x access %04x %s\n",
			s->s_dst, s->s_ctl, s->s_access,
			(s->s_flags & HS_GATE) ? "gateway" : "");
	} else if (s->s_flags & HS_INDIR) {
		printf("\t%d gateways:", s->s_ngate);
		for (i = 0; i < s->s_ngate; i++)
			printf(" %s", s->s_gate[i]->s_name);
		printf("\n");
	}
}

symtab_print()
{
	register struct sym *s;

	printf("\n");
	for (s = sym_head; s != NULL; s = s->s_next)
		sym_print(s);
	printf("End of Symbol table\n");
}

char outbuf[128] = "";
char *outbufp = outbuf;

/*
 * record characters in the output buffer for later error message
 */
output(c)
	int c;
{
	if ((isprint(c) || c == ' ') && isascii(c) && outbufp < &outbuf[sizeof(outbuf)])
		*outbufp++ = c;
	*outbufp = '\0';
}

/*
 * if there are any characters in the output buffer, print an error message
 */
outwrap()
{
	if (outbufp != outbuf) {
		fprintf(stderr, "hyroute: syntax error on \"%s\"\n", outbuf);
		outbufp = outbuf;
		lex_error++;
	}
}

/*
 * flush last error message out then return saying no more input available
 */
yywrap() {
	outwrap();
	return(1);
}
