#include "mh.h"

/* switch match, or any unambiguous abbreviation */
/* exact match always wins, even if shares same root */
/* returns subscript in zero-terminated tbl[] of strings */
/* returns -1 if no match, -2 if ambiguous */

#define abs(i) (i < 0 ? -i : i)

smatch(string, swp)
char *string;
struct swit *swp;
{
	register char *sp, *tcp;
	struct swit *tp;
	int firstone, stringlen;

	firstone = -1;

	for (stringlen = strlen(string), tp = swp; tcp = tp->sw; tp++) {
		if(stringlen < abs(tp->minchars)) continue;      /* no match */
		for (sp = string; *sp == *tcp++; ) {
			if (*sp++ == 0) return(tp-swp); /* exact match */
		}
		if (*sp != 0) {
			if (*sp != ' ') continue; /* no match */
			if (*--tcp == 0) return(tp-swp); /* exact match */
		}
		if (firstone == -1) firstone = tp-swp; /* possible match */
		else firstone = -2;	/* ambiguous */
	}

	return (firstone);
}
