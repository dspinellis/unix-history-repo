%{

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)lang.l	5.6 (Berkeley) 5/16/90
 */

#include <ctype.h>
#include "y.tab.h"
#include "config.h"

#define tprintf if (do_trace) printf

/*
 * Key word table
 */

struct kt {
	char *kt_name;
	int kt_val;
} key_words[] = {
	{ "and",	AND },
	{ "args",	ARGS },
	{ "at",		AT },
	{ "config",	CONFIG },
	{ "controller",	CONTROLLER },
	{ "cpu",	CPU },
	{ "csr",	CSR },
	{ "device",	DEVICE },
	{ "disk",	DISK },
	{ "drive",	DRIVE },
	{ "dst",	DST },
	{ "dumps",	DUMPS },
	{ "flags",	FLAGS },
	{ "hz",		HZ },
	{ "ident",	IDENT },
	{ "machine",	MACHINE },
	{ "major",	MAJOR },
	{ "makeoptions", MAKEOPTIONS },
	{ "master",	MASTER },
	{ "maxusers",	MAXUSERS },
	{ "minor",	MINOR },
	{ "nexus",	NEXUS },
	{ "on",		ON },
	{ "options",	OPTIONS },
	{ "priority",	PRIORITY },
	{ "pseudo-device",PSEUDO_DEVICE },
	{ "root",	ROOT },
#if MACHINE_HP300
	{ "scode",	NEXUS },
#endif
	{ "size",	SIZE },
	{ "slave",	SLAVE },
	{ "swap",	SWAP },
	{ "tape",	DEVICE },
	{ "timezone",	TIMEZONE },
	{ "trace",	TRACE },
	{ "vector",	VECTOR },
	{ 0, 0 },
};
%}
WORD	[A-Za-z_][-A-Za-z_]*
%%
{WORD}		{
			int i;

			if ((i = kw_lookup(yytext)) == -1)
			{
				yylval.str = yytext;
				tprintf("id(%s) ", yytext);
				return ID;
			}
			tprintf("(%s) ", yytext);
			return i;
		}
\"[^"]+\"	{
			yytext[strlen(yytext)-1] = '\0';
			yylval.str = yytext + 1;
			return ID;
		}
0[0-7]*		{
			yylval.val = octal(yytext);
			tprintf("#O:%o ", yylval.val);
			return NUMBER;
		}
0x[0-9a-fA-F]+	{
			yylval.val = hex(yytext);
			tprintf("#X:%x ", yylval.val);
			return NUMBER;
		}
[1-9][0-9]*	{
			yylval.val = atoi(yytext);
			tprintf("#D:%d ", yylval.val);
			return NUMBER;
		}
[0-9]"."[0-9]*	{
			double atof();
			yylval.val = (int) (60 * atof(yytext) + 0.5);
			return FPNUMBER;
		}
"-"		{
			return MINUS;
		}
"?"		{
			yylval.val = -1;
			tprintf("? ");
			return NUMBER;
		}
\n/[ \t]	{
			yyline++;
			tprintf("\n... ");
		}
\n		{
			yyline++;
			tprintf("\n");
			return SEMICOLON;
		}
#.*		{	/* Ignored (comment) */;	}
[ \t]*		{	/* Ignored (white space) */;	}
";"		{	return SEMICOLON;		}
","		{	return COMMA;			}
"="		{	return EQUALS;			}
"@"		{	return AT;			}
.		{	return yytext[0];		}

%%
/*
 * kw_lookup
 *	Look up a string in the keyword table.  Returns a -1 if the
 *	string is not a keyword otherwise it returns the keyword number
 */

kw_lookup(word)
register char *word;
{
	register struct kt *kp;

	for (kp = key_words; kp->kt_name != 0; kp++)
		if (eq(word, kp->kt_name))
			return kp->kt_val;
	return -1;
}

/*
 * Number conversion routines
 */

octal(str)
char *str;
{
	int num;

	(void) sscanf(str, "%o", &num);
	return num;
}

hex(str)
char *str;
{
	int num;

	(void) sscanf(str+2, "%x", &num);
	return num;
}
