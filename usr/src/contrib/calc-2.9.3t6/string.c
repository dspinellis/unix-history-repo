/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * String list routines.
 */

#include "calc.h"
#include "string.h"

#define STR_TABLECHUNK	100	/* how often to reallocate string table */
#define STR_CHUNK	2000	/* size of string storage allocation */
#define STR_UNIQUE	100	/* size of string to allocate separately */


static char *chartable;		/* single character string table */

static struct {
	long l_count;		/* count of strings in table */
	long l_maxcount;	/* maximum strings storable in table */
	long l_avail;		/* characters available in current string */
	char *l_alloc;		/* next available string storage */
	char **l_table;		/* current string table */
} literals;


/*
 * Initialize or reinitialize a string header for use.
 */
void
initstr(hp)
	register STRINGHEAD *hp;	/* structure to be inited */
{
	if (hp->h_list == NULL) {
		hp->h_list = (char *)malloc(2000);
		hp->h_avail = 2000;
		hp->h_used = 0;
	}
	hp->h_avail += hp->h_used;
	hp->h_used = 0;
	hp->h_count = 0;
	hp->h_list[0] = '\0';
	hp->h_list[1] = '\0';
}


/*
 * Copy a string to the end of a list of strings, and return the address
 * of the copied string.  Returns NULL if the string could not be copied.
 * No checks are made to see if the string is already in the list.
 * The string cannot be null or have imbedded nulls.
 */
char *
addstr(hp, str)
	register STRINGHEAD *hp;	/* header of string storage */
	char *str;		/* string to be added */
{
	char *retstr;		/* returned string pointer */
	char *list;		/* string list */
	long newsize;		/* new size of string list */
	long len;		/* length of current string */

	if ((str == NULL) || (*str == '\0'))
		return NULL;
	len = strlen(str) + 1;
	if (hp->h_avail <= len) {
		newsize = len + 2000 + hp->h_used + hp->h_avail;
		list = (char *)realloc(hp->h_list, newsize);
		if (list == NULL)
			return NULL;
		hp->h_list = list;
		hp->h_avail = newsize - hp->h_used;
	}
	retstr = hp->h_list + hp->h_used;
	hp->h_used += len;
	hp->h_avail -= len;
	hp->h_count++;
	strcpy(retstr, str);
	retstr[len] = '\0';
	return retstr;
}


/*
 * Return a null-terminated string which consists of a single character.
 * The table is initialized on the first call.
 */
char *
charstr(ch)
	int ch;
{
	char *cp;
	int i;

	if (chartable == NULL) {
		cp = (char *)malloc(512);
		if (cp == NULL)
			math_error("Cannot allocate character table");
		for (i = 0; i < 256; i++) {
			*cp++ = (char)i;
			*cp++ = '\0';
		}
		chartable = cp - 512;
	}
	return &chartable[(ch & 0xff) * 2];
}


/*
 * Find a string with the specified name and return its number in the
 * string list.  The first string is numbered zero.  Minus one is returned
 * if the string is not found.
 */
long
findstr(hp, str)
	STRINGHEAD *hp;		/* header of string storage */
	register char *str;	/* string to be added */
{
	register char *test;	/* string being tested */
	long len;		/* length of string being found */
	long testlen;		/* length of test string */
	long index;		/* index of string */

	if ((hp->h_count <= 0) || (str == NULL))
		return -1;
	len = strlen(str);
	test = hp->h_list;
	index = 0;
	while (*test) {
		testlen = strlen(test);
		if ((testlen == len) && (*test == *str) && (strcmp(test, str) == 0))
			return index;
		test += (testlen + 1);
		index++;
	}
	return -1;
}


/*
 * Return the name of a string with the given index.
 * If the index is illegal, a pointer to an empty string is returned.
 */
char *
namestr(hp, n)
	STRINGHEAD *hp;		/* header of string storage */
	long n;
{
	register char *str;	/* current string */

	if ((unsigned long)n >= hp->h_count)
		return "";
	str = hp->h_list;
	while (*str) {
		if (--n < 0)
			return str;
		str += (strlen(str) + 1);
	}
	return "";
}


/*
 * Useful routine to return the index of one string within another one
 * which has the format:  "str1\0str2\0str3\0...strn\0\0".  Index starts
 * at one for the first string.  Returns zero if the string being checked
 * is not contained in the formatted string.
 */
long
stringindex(format, test)
	register char *format;	/* string formatted into substrings */
	char *test;		/* string to be found in formatted string */
{
	long index;		/* found index */
	long len;		/* length of current piece of string */
	long testlen;		/* length of test string */

	testlen = strlen(test);
	index = 1;
	while (*format) {
		len = strlen(format);
		if ((len == testlen) && (*format == *test) &&
			(strcmp(format, test) == 0))
				return index;
		format += (len + 1);
		index++;
	}
	return 0;
}


/*
 * Add a possibly new literal string to the literal string pool.
 * Returns the new string address which is guaranteed to be always valid.
 * Duplicate strings will repeatedly return the same address.
 */
char *
addliteral(str)
	char *str;
{
	register char **table;	/* table of strings */
	char *newstr;		/* newly allocated string */
	long count;		/* number of strings */
	long len;		/* length of string to allocate */

	len = strlen(str);
	if (len <= 1)
		return charstr(*str);
	/*
	 * See if the string is already in the table.
	 */
	table = literals.l_table;
	count = literals.l_count;
	while (count-- > 0) {
		if ((str[0] == table[0][0]) && (str[1] == table[0][1]) &&
			(strcmp(str, table[0]) == 0))
				return table[0];
		table++;
	}
	/*
	 * Make the table of string pointers larger if necessary.
	 */
	if (literals.l_count >= literals.l_maxcount) {
		count = literals.l_maxcount + STR_TABLECHUNK;
		if (literals.l_maxcount)
			table = (char **) realloc(literals.l_table, count * sizeof(char *));
		else
			table = (char **) malloc(count * sizeof(char *));
		if (table == NULL)
			math_error("Cannot allocate string literal table");
		literals.l_table = table;
		literals.l_maxcount = count;
	}
	table = literals.l_table;
	/*
	 * If the new string is very long, allocate it manually.
	 */
	len = (len + 2) & ~1;	/* add room for null and round up to word */
	if (len >= STR_UNIQUE) {
		newstr = (char *)malloc(len);
		if (newstr == NULL)
			math_error("Cannot allocate large literal string");
		strcpy(newstr, str);
		table[literals.l_count++] = newstr;
		return newstr;
	}
	/*
	 * If the remaining space in the allocate string is too small,
	 * then allocate a new one.
	 */
	if (literals.l_avail < len) {
		newstr = (char *)malloc(STR_CHUNK);
		if (newstr == NULL)
			math_error("Cannot allocate new literal string");
		literals.l_alloc = newstr;
		literals.l_avail = STR_CHUNK;
	}
	/*
	 * Allocate the new string from the allocate string.
	 */
	newstr = literals.l_alloc;
	literals.l_avail -= len;
	literals.l_alloc += len;
	table[literals.l_count++] = newstr;
	strcpy(newstr, str);
	return newstr;
}


/*
 * Calculate a trivial hash value for a string.
 */
HASH
hashstr(cp)
	char *cp;
{
	int len;
	HASH hash;

	len = strlen(cp);
	hash = len * 300007;
	while (len-- > 0)
		/* ignore Saber-C warning about Over/underflow */
		hash = hash * 300017 + *cp++ + 300043;
	return hash;
}

/* END CODE */
