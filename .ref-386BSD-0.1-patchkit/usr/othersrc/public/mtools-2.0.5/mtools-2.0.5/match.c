/*
 * Do shell-style pattern matching for '?', '\', '[..]', and '*' wildcards.
 * Returns 1 if match, 0 if not.
 */

#include <stdio.h>

int
match(s, p)
char *s, *p;
{
	int matched, reverse;
	char first, last;

	for (; *p != '\0'; s++, p++) {
		switch (*p) {
			case '?':	/* match any one character */
				if (*s == '\0')
					return(0);
				break;
			case '*':	/* match everything */
				while (*p == '*')
					p++;

					/* if last char in pattern */
				if (*p == '\0')
					return(1);

					/* search for next char in pattern */
				matched = 0;
				while (*s != '\0') {
					if (*s == *p) {
						matched = 1;
						break;
					}
					s++;
				}
				if (!matched)
					return(0);
				break;
			case '[':	 /* match range of characters */
				first = '\0';
				matched = 0;
				reverse = 0;
				while (*++p != ']') {
					if (*p == '^') {
						reverse = 1;
						p++;
					}
					first = *p;
					if (first == ']' || first == '\0')
						return(0);

					/* if 2nd char is '-' */
					if (*(p + 1) == '-') {
						p++;
					/* set last to 3rd char ... */
						last = *++p;
						if (last == ']' || last == '\0')
							return(0);
					/* test the range of values */
						if (*s >= first && *s <= last) {
							matched = 1;
							p++;
							break;
						}
						return(0);
					}
					if (*s == *p)
						matched = 1;
				}
				if (matched && reverse)
					return(0);
				if (!matched)
					return(0);
				break;
			case '\\':	/* Literal match with next character */
				p++;
				/* fall thru */
			default:
				if (*s != *p)
					return(0);
				break;
		}
	}
					/* string ended prematurely ? */
	if (*s != '\0')
		return(0);
	else
		return(1);
}
