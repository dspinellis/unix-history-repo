/*-
 * Copyright (c) 1992 Henry Spencer.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)regerror.c	5.2 (Berkeley) %G%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)regerror.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <regex.h>

#include "utils.h"

/*
 = #define	REG_NOMATCH	 1
 = #define	REG_BADPAT	 2
 = #define	REG_ECOLLATE	 3
 = #define	REG_ECTYPE	 4
 = #define	REG_EESCAPE	 5
 = #define	REG_ESUBREG	 6
 = #define	REG_EBRACK	 7
 = #define	REG_EPAREN	 8
 = #define	REG_EBRACE	 9
 = #define	REG_BADBR	10
 = #define	REG_ERANGE	11
 = #define	REG_ESPACE	12
 = #define	REG_BADRPT	13
 = #define	REG_EMPTY	14
 = #define	REG_ASSERT	15
 */
static struct rerr {
	int code;
	char *name;
	char *explain;
} rerrs[] = {
	REG_NOMATCH,	"NOMATCH",	"regexec() failed to match",
	REG_BADPAT,	"BADPAT",	"invalid regular expression",
	REG_ECOLLATE,	"ECOLLATE",	"invalid collating element",
	REG_ECTYPE,	"ECTYPE",	"invalid character class",
	REG_EESCAPE,	"EESCAPE",	"trailing backslash (\\)",
	REG_ESUBREG,	"ESUBREG",	"invalid backreference number",
	REG_EBRACK,	"EBRACK",	"brackets ([ ]) not balanced",
	REG_EPAREN,	"EPAREN",	"parentheses not balanced",
	REG_EBRACE,	"EBRACE",	"braces not balanced",
	REG_BADBR,	"BADBR",	"invalid repetition count(s)",
	REG_ERANGE,	"ERANGE",	"invalid character range",
	REG_ESPACE,	"ESPACE",	"out of memory",
	REG_BADRPT,	"BADRPT",	"repetition-operator operand invalid",
	REG_EMPTY,	"EMPTY",	"empty (sub)expression",
	REG_ASSERT,	"ASSERT",	"\"can't happen\" -- you found a bug",
	0,		"",		"*** unknown regexp error code ***",
};

/*
 - regerror - the interface to error numbers
 = extern size_t regerror(int errcode, const regex_t *preg, char *errbuf, \
 =							size_t errbuf_size);
 */
/* ARGSUSED */
size_t
regerror(errcode, preg, errbuf, errbuf_size)
int errcode;
const regex_t *preg;
char *errbuf;
size_t errbuf_size;
{
	register struct rerr *r;
	register size_t len;

	for (r = rerrs; r->code != 0; r++)
		if (r->code == errcode)
			break;

	len = strlen(r->explain) + 1;
	if (errbuf_size > 0) {
		if (errbuf_size > len)
			(void) strcpy(errbuf, r->explain);
		else {
			(void) strncpy(errbuf, r->explain, errbuf_size-1);
			errbuf[errbuf_size-1] = '\0';
		}
	}

	return(len);
}

#ifdef REDEBUG
/*
 - eprint - express an error number as a string
 */
char *
eprint(eno)
int eno;
{
	register struct rerr *r;
	static char eval[10];

	for (r = rerrs; r->code != 0; r++)
		if (r->code == eno)
			return(r->name);
	sprintf(eval, "#%d", r->code);
	return(eval);
}

/*
 - efind - find an error name
 */
int
efind(ename)
char *ename;
{
	register struct rerr *r;

	for (r = rerrs; r->code != 0; r++)
		if (strcmp(r->name, ename) == 0)
			return(r->code);
	return(0);		/* it'll do */
}
#endif
