/* match.c: pattern matching routines */

#include "rc.h"

static int rangematch(char *, char);

enum { RANGE_FAIL = -1, RANGE_ERROR = -2 };

/* match() matches a single pattern against a single string. */

extern bool match(char *p, char *m, char *s) {
	int i, j;
	if (m == NULL)
		return streq(p, s);
	i = 0;
	while (1) {
		if (p[i] == '\0')
			return *s == '\0';
		else if (m[i]) {
			switch (p[i++]) {
			case '?':
				if (*s++ == '\0')
					return FALSE;
				break;
			case '*':
				while (p[i] == '*' && m[i] == 1)	/* collapse multiple stars */
					i++;
				if (p[i] == '\0') 	/* star at end of pattern? */
					return TRUE;
				while (*s != '\0')
					if (match(p + i, m + i, s++))
						return TRUE;
				return FALSE;
			case '[':
				if (*s == '\0')
					return FALSE;
				switch (j = rangematch(p + i, *s)) {
				default:
					i += j;
					break;
				case RANGE_FAIL:
					return FALSE;
				case RANGE_ERROR:
					if (*s != '[')
						return FALSE;
				}
				s++;
				break;
			default:
				panic("bad metacharacter in match");
				/* NOTREACHED */
				return FALSE; /* hush up gcc -Wall */
			}
		} else if (p[i++] != *s++)
			return FALSE;
	}
}

/*
   From the ed(1) man pages (on ranges):

	The `-' is treated as an ordinary character if it occurs first
	(or first after an initial ^) or last in the string.

	The right square bracket does not terminate the enclosed string
	if it is the first character (after an initial `^', if any), in
	the bracketed string.

   rangematch() matches a single character against a class, and returns
   an integer offset to the end of the range on success, or -1 on
   failure.
*/

static int rangematch(char *p, char c) {
	char *orig = p;
	bool neg = (*p == '~');
	bool matched = FALSE;
	if (neg)
		p++;
	if (*p == ']') {
		p++;
		matched = (c == ']');
	}
	for (; *p != ']'; p++) {
		if (*p == '\0')
			return RANGE_ERROR;	/* bad syntax */
		if (p[1] == '-' && p[2] != ']') { /* check for [..-..] but ignore [..-] */
			if (c >= *p)
				matched |= (c <= p[2]);
			p += 2;
		} else {
			matched |= (*p == c);
		}
	}
	if (matched ^ neg)
		return p - orig + 1; /* skip the right-bracket */
	else
		return RANGE_FAIL;
}
