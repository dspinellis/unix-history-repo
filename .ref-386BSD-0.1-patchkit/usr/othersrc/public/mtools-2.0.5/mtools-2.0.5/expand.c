/*
 * Do filename expansion with the shell.  Returns a pointer to a
 * static area.
 */

#define EXPAND_BUF	2048

#include <stdio.h>

char *
expand(input)
char *input;
{
	FILE *fp, *popen();
	int last;
	char buf[256], *strpbrk(), *strcpy();
	static char ans[EXPAND_BUF];

	if (input == NULL)
		return(NULL);
	if (*input == '\0')
		return("");
					/* any thing to expand? */
	if (!strpbrk(input, "$*{}[]\\?~")) {
		strcpy(ans, input);
		return(ans);
	}
					/* popen an echo */
	sprintf(buf, "echo %s", input);

	fp = popen(buf, "r");
	fgets(ans, EXPAND_BUF, fp);
	pclose(fp);

	if (!strlen(ans)) {
		strcpy(ans, input);
		return(ans);
	}

	/*
	 * A horrible kludge...  if the last character is not a line feed,
	 * then the csh has returned an error message.  Otherwise zap the
	 * line feed.
	 */
	last = strlen(ans) - 1;
	if (ans[last] != '\n') {
		strcpy(ans, input);
		return(ans);
	}
	else
		ans[last] = '\0';

	return(ans);
}

/*
 * Miscellaneous routines probably missing from Bezerkely
 */

#ifdef BSD
/*
 * Return ptr to first occurrence of any character from `brkset'
 * in the character string `string'; NULL if none exists.
 */

char *
strpbrk(string, brkset)
register char *string, *brkset;
{
	register char *p;

	if (!string || !brkset)
		return(0);
	do {
		for (p = brkset; *p != '\0' && *p != *string; ++p)
			;
		if (*p != '\0')
			return(string);
	}
	while (*string++);
	return(0);
}

/*
 * Copies the character c, n times to string s
 */

char *
memset(s, c, n)
char *s, c;
int n;
{
	char *s1 = s;

	while (n > 0) {
		--n;
		*s++ = c;
	}
	return(s1);
}

/*
 * Copy contents of memory (with possible overlapping).
 */

char *
memcpy(s1, s2, n)
char *s1, *s2;
int n;
{
	bcopy(s2, s1, n);
	return(s1);
}
#endif /* BSD */
