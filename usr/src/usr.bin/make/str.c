/*-
 * Copyright (c) 1988, 1989, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char     sccsid[] = "@(#)str.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include "make.h"

/*-
 * str_concat --
 *	concatenate the two strings, inserting a space or slash between them,
 *	freeing them if requested.
 *
 * returns --
 *	the resulting string in allocated space.
 */
char *
str_concat(s1, s2, flags)
	char *s1, *s2;
	int flags;
{
	register int len1, len2;
	register char *result;

	/* get the length of both strings */
	len1 = strlen(s1);
	len2 = strlen(s2);

	/* allocate length plus separator plus EOS */
	result = emalloc((u_int)(len1 + len2 + 2));

	/* copy first string into place */
	memcpy(result, s1, len1);

	/* add separator character */
	if (flags & STR_ADDSPACE) {
		result[len1] = ' ';
		++len1;
	} else if (flags & STR_ADDSLASH) {
		result[len1] = '/';
		++len1;
	}

	/* copy second string plus EOS into place */
	memcpy(result + len1, s2, len2 + 1);

	/* free original strings */
	if (flags & STR_DOFREE) {
		(void)free(s1);
		(void)free(s2);
	}
	return(result);
}

/*-
 * brk_string --
 *	Fracture a string into an array of words (as delineated by tabs or
 *	spaces) taking quotation marks into account.  Leading tabs/spaces
 *	are ignored.
 *
 * returns --
 *	Pointer to the array of pointers to the words.  To make life easier,
 *	the first word is always the value of the .MAKE variable.
 */
char **
brk_string(str, store_argc)
	register char *str;
	int *store_argc;
{
	static int argmax, curlen;
	static char **argv, *buf;
	register int argc, ch;
	register char inquote, *p, *start, *t;
	int len;

	/* save off pmake variable */
	if (!argv) {
		argv = (char **)emalloc((argmax = 50) * sizeof(char *));
		argv[0] = Var_Value(".MAKE", VAR_GLOBAL);
	}

	/* skip leading space chars. */
	for (; *str == ' ' || *str == '\t'; ++str)
		continue;

	/* allocate room for a copy of the string */
	if ((len = strlen(str) + 1) > curlen)
		buf = emalloc(curlen = len);

	/*
	 * copy the string; at the same time, parse backslashes,
	 * quotes and build the argument list.
	 */
	argc = 1;
	inquote = '\0';
	for (p = str, start = t = buf;; ++p) {
		switch(ch = *p) {
		case '"':
		case '\'':
			if (inquote)
				if (inquote == ch)
					inquote = NULL;
				else
					break;
			else
				inquote = (char) ch;
			continue;
		case ' ':
		case '\t':
			if (inquote)
				break;
			if (!start)
				continue;
			/* FALLTHROUGH */
		case '\n':
		case '\0':
			/*
			 * end of a token -- make sure there's enough argv
			 * space and save off a pointer.
			 */
			*t++ = '\0';
			if (argc == argmax) {
				argmax *= 2;		/* ramp up fast */
				if (!(argv = (char **)realloc(argv,
				    argmax * sizeof(char *))))
				enomem();
			}
			argv[argc++] = start;
			start = (char *)NULL;
			if (ch == '\n' || ch == '\0')
				goto done;
			continue;
		case '\\':
			switch (ch = *++p) {
			case '\0':
			case '\n':
				/* hmmm; fix it up as best we can */
				ch = '\\';
				--p;
				break;
			case 'b':
				ch = '\b';
				break;
			case 'f':
				ch = '\f';
				break;
			case 'n':
				ch = '\n';
				break;
			case 'r':
				ch = '\r';
				break;
			case 't':
				ch = '\t';
				break;
			}
			break;
		}
		if (!start)
			start = t;
		*t++ = (char) ch;
	}
done:	argv[argc] = (char *)NULL;
	*store_argc = argc;
	return(argv);
}

/*
 * Str_FindSubstring -- See if a string contains a particular substring.
 * 
 * Results: If string contains substring, the return value is the location of
 * the first matching instance of substring in string.  If string doesn't
 * contain substring, the return value is NULL.  Matching is done on an exact
 * character-for-character basis with no wildcards or special characters.
 * 
 * Side effects: None.
 */
char *
Str_FindSubstring(string, substring)
	register char *string;		/* String to search. */
	char *substring;		/* Substring to find in string */
{
	register char *a, *b;

	/*
	 * First scan quickly through the two strings looking for a single-
	 * character match.  When it's found, then compare the rest of the
	 * substring.
	 */

	for (b = substring; *string != 0; string += 1) {
		if (*string != *b)
			continue;
		a = string;
		for (;;) {
			if (*b == 0)
				return(string);
			if (*a++ != *b++)
				break;
		}
		b = substring;
	}
	return((char *) NULL);
}

/*
 * Str_Match --
 * 
 * See if a particular string matches a particular pattern.
 * 
 * Results: Non-zero is returned if string matches pattern, 0 otherwise. The
 * matching operation permits the following special characters in the
 * pattern: *?\[] (see the man page for details on what these mean).
 * 
 * Side effects: None.
 */
int
Str_Match(string, pattern)
	register char *string;		/* String */
	register char *pattern;		/* Pattern */
{
	char c2;

	for (;;) {
		/*
		 * See if we're at the end of both the pattern and the
		 * string. If, we succeeded.  If we're at the end of the
		 * pattern but not at the end of the string, we failed.
		 */
		if (*pattern == 0)
			return(!*string);
		if (*string == 0 && *pattern != '*')
			return(0);
		/*
		 * Check for a "*" as the next pattern character.  It matches
		 * any substring.  We handle this by calling ourselves
		 * recursively for each postfix of string, until either we
		 * match or we reach the end of the string.
		 */
		if (*pattern == '*') {
			pattern += 1;
			if (*pattern == 0)
				return(1);
			while (*string != 0) {
				if (Str_Match(string, pattern))
					return(1);
				++string;
			}
			return(0);
		}
		/*
		 * Check for a "?" as the next pattern character.  It matches
		 * any single character.
		 */
		if (*pattern == '?')
			goto thisCharOK;
		/*
		 * Check for a "[" as the next pattern character.  It is
		 * followed by a list of characters that are acceptable, or
		 * by a range (two characters separated by "-").
		 */
		if (*pattern == '[') {
			++pattern;
			for (;;) {
				if ((*pattern == ']') || (*pattern == 0))
					return(0);
				if (*pattern == *string)
					break;
				if (pattern[1] == '-') {
					c2 = pattern[2];
					if (c2 == 0)
						return(0);
					if ((*pattern <= *string) &&
					    (c2 >= *string))
						break;
					if ((*pattern >= *string) &&
					    (c2 <= *string))
						break;
					pattern += 2;
				}
				++pattern;
			}
			while ((*pattern != ']') && (*pattern != 0))
				++pattern;
			goto thisCharOK;
		}
		/*
		 * If the next pattern character is '/', just strip off the
		 * '/' so we do exact matching on the character that follows.
		 */
		if (*pattern == '\\') {
			++pattern;
			if (*pattern == 0)
				return(0);
		}
		/*
		 * There's no special character.  Just make sure that the
		 * next characters of each string match.
		 */
		if (*pattern != *string)
			return(0);
thisCharOK:	++pattern;
		++string;
	}
}
