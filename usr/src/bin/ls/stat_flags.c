/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)stat_flags.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <stddef.h>
#include <string.h>

#define	SAPPEND(s) {							\
	if (prefix != NULL)						\
		(void)strcat(string, prefix);				\
	(void)strcat(string, s);					\
	prefix = ",";							\
}

/*
 * flags_to_string --
 *	Convert stat flags to a comma-separated string.  If no flags
 *	are set, return the default string.
 */
char *
flags_to_string(flags, def)
	u_long flags;
	char *def;
{
	static char string[128];
	char *prefix;

	string[0] = '\0';
	prefix = NULL;
	if (flags & ARCHIVED)
		SAPPEND("arch");
	if (flags & NODUMP)
		SAPPEND("nodump");
	if (flags & USR_IMMUTABLE)
		SAPPEND("uchg");
	if (flags & USR_APPEND)
		SAPPEND("uappnd");
	if (flags & SYS_IMMUTABLE)
		SAPPEND("schg");
	if (flags & SYS_APPEND)
		SAPPEND("sappnd");
	return (prefix == NULL && def != NULL ? def : string);
}

#define	TEST(a, b, f) {							\
	if (!memcmp(a, b, sizeof(b))) {					\
		if (clear) {						\
			if (clrp)					\
				*clrp |= (f);				\
		} else if (setp)					\
			*setp |= (f);					\
		break;							\
	}								\
}

/*
 * string_to_flags --
 *	Take string of arguments and return stat flags.  Return 0 on
 *	success, 1 on failure.  On failure, stringp is set to point
 *	to the offending token.
 */
int
string_to_flags(stringp, setp, clrp)
	char **stringp;
	u_long *setp, *clrp;
{
	int clear;
	char *string, *p;

	clear = 0;
	if (setp)
		*setp = 0;
	if (clrp)
		*clrp = 0;
	string = *stringp;
	while ((p = strsep(&string, "\t ,")) != NULL) {
		*stringp = p;
		if (*p == '\0')
			continue;
		if (p[0] == 'n' && p[1] == 'o') {
			clear = 1;
			p += 2;
		}
		switch (p[0]) {
		case 'a':
			TEST(p, "arch", ARCHIVED);
			TEST(p, "archived", ARCHIVED);
			return (1);
		case 'd':
			clear = !clear;
			TEST(p, "dump", NODUMP);
			return (1);
		case 's':
			TEST(p, "sappnd", SYS_APPEND);
			TEST(p, "sappend", SYS_APPEND);
			TEST(p, "schg", SYS_IMMUTABLE);
			TEST(p, "schange", SYS_IMMUTABLE);
			TEST(p, "simmutable", SYS_IMMUTABLE);
			return (1);
		case 'u':
			TEST(p, "uappnd", USR_APPEND);
			TEST(p, "uappend", USR_APPEND);
			TEST(p, "uchg", USR_IMMUTABLE);
			TEST(p, "uchange", USR_IMMUTABLE);
			TEST(p, "uimmutable", SYS_IMMUTABLE);
			/* FALLTHROUGH */
		default:
			return (1);
		}
	}
	return (0);
}
