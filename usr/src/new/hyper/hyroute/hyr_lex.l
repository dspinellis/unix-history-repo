%Start GATE
ws	[ \t\n]*
s	[ \t\n]+
num	[0123456789abcdefABCDEF]+
name	[a-zA-Z.\-_][a-zA-Z0-9.\-_]*
other	[^ \t\na-zA-Z.\-_0-9;]

%{
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "hyr_sym.h"
#undef output

struct sym *curgate = NULL;
struct sym *sym_head = NULL;
static char rcsid[] = "$Header: hyr_lex.l,v 2.4 84/05/04 12:15:34 steveg Exp $$Locker:  $";

char name[32];

int lexdebug = 0;

extern struct sym *lookup();
%}

%%
^\*.*$	;
^#.*$	;
^{ws}$	;

{ws}direct{s}{name}{s}{num}{s}{num}{s}{num}{ws};	{
	register struct sym *s;
	unsigned dst, ctl, access;

	outwrap();
	sscanf(yytext, " direct %s %x %x %x ", name, &dst, &ctl, &access);
	if (lexdebug)
		fprintf(stderr, "DIRECT %s %04x %04x %04x\n", name, dst, ctl, access);
	if (s = lookup(name)) {
		s->sym_dst = htons(dst);
		s->sym_ctl = htons(ctl);
		s->sym_access = htons(access);
		s->sym_flags = HS_DIR;
	}
}

{ws}loop{s}{name}{s}{num}{s}{num}{s}{num}{ws};	{
	register struct sym *s;
	unsigned dst, ctl, access;

	outwrap();
	sscanf(yytext, " loop %s %x %x %x ", name, &dst, &ctl, &access);
	if (lexdebug)
		fprintf(stderr, "LOOP %s %04x %04x %04x\n", name, dst, ctl, access);
	if (s = lookup(name)) {
		s->sym_dst = htons(dst);
		s->sym_ctl = htons(ctl);
		s->sym_access = htons(access);
		s->sym_flags = HS_DIR + HS_LOOP;
	}
}

{ws}remloop{s}{name}{s}{num}{s}{num}{s}{num}{ws};	{
	register struct sym *s;
	unsigned dst, ctl, access;

	outwrap();
	sscanf(yytext, " remloop %s %x %x %x ", name, &dst, &ctl, &access);
	if (lexdebug)
		fprintf(stderr, "REMLOOP %s %04x %04x %04x\n", name, dst, ctl, access);
	if (s = lookup(name)) {
		s->sym_dst = htons(dst);
		s->sym_ctl = htons(ctl);
		s->sym_access = htons(access);
		s->sym_flags = HS_DIR + HS_RLOOP;
	}
}

{ws}gateway{s}{name}	{
	register struct sym *s;

	outwrap();
	sscanf(yytext, " gateway %s ", name);
	if (lexdebug)
		fprintf(stderr, "GATEWAY %s ->", name);
	if (s = lookup(name)) {
		s->sym_flags = HS_INDIR;
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
		if (curgate->sym_ngate < 32)
			curgate->sym_gate[curgate->sym_ngate++] = s;
		s->sym_flags |= HS_GATE;
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

struct sym *
lookup(cp)
	char *cp;
{
	register struct sym *s;
	register struct hostent *hp;
	struct hostent *gethostbyname();
	extern char *malloc();

	for (s = sym_head; s != NULL; s = s->sym_next) {
		if (strncmp(cp, s->sym_name, sizeof(s->sym_name)) == 0 ||
		    strncmp(cp, s->sym_nickname, sizeof(s->sym_nickname)) == 0) {
			return(s);
		}
	}
	hp = gethostbyname(cp);
	if (hp == NULL) {
		printf("Host %s not found\n", cp);
		return(NULL);
	}
	if (hp->h_addrtype != AF_INET) {
		printf("Host %s not Internet\n", cp);
		return(NULL);
	}
	s = (struct sym *) malloc(sizeof *s);
	if (s != NULL) {
		s->sym_lastok = 0;
		s->sym_flags = 0;
		s->sym_ngate = 0;
		s->sym_dst = s->sym_ctl = s->sym_access = 0;
		s->sym_inaddr = *(struct in_addr *)hp->h_addr;
		s->sym_next = sym_head;
		sym_head = s;
		strncpy(s->sym_name, hp->h_name, sizeof(s->sym_name));
		strncpy(s->sym_nickname, cp, sizeof(s->sym_nickname));
	}
	return(s);
}

sym_print(s)
	register struct sym *s;
{
	register int i;
	char *str = " ";

	printf("Host %s", s->sym_name);
	if (strcmp(s->sym_name, s->sym_nickname) != 0)
		printf(" (aka %s)", s->sym_nickname);
	printf("\n\tInternet address: ");
	in_print(s->sym_inaddr.s_addr);
	printf("\n");
	if (s->sym_flags & HS_DIR) {
		if (s->sym_flags & HS_LOOP)
			printf("\tLoop:    ");
		else if (s->sym_flags & HS_RLOOP)
			printf("\tRemloop: ");
		else
			printf("\tDirect:  ");
		printf("dst %04x ctl %04x access %04x%s\n",
			ntohs(s->sym_dst),
			ntohs(s->sym_ctl),
			ntohs(s->sym_access),
			(s->sym_flags & HS_GATE) ? "\tused as a gateway" : "");
	} else if (s->sym_flags & HS_INDIR) {
		printf("\tGatewayed through:");
		for (i = 0; i < s->sym_ngate; i++) {
			printf("%s%s", str, s->sym_gate[i]->sym_name);
			str = ", ";
		}
		printf("\n");
	}
}

symtab_print()
{
	register struct sym *s;

	printf("\n");
	for (s = sym_head; s != NULL; s = s->sym_next)
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

/*
 * print an internet format address in "dot" notation
 * input address is in network byte ordering
 */
in_print(n)
	register unsigned long n;
{
	n = ntohl(n);
	printf("%d.%d.%d.%d",	(n >> 24) & 0xff, (n >> 16) & 0xff, (n >> 8) & 0xff, n & 0xff);
}
