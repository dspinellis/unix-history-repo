#ifndef lint
static char sccsid[] = "@(#)getline.c	1.1	(Berkeley)	6/8/88";
#endif not lint

#include <stdio.h>

getline(s, lim)	/* get line into s, return length */
char s[];
int lim;
{
	int c, i;

	i = 0;
	while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (c == '\n')
		s[i++] = c;
	s[i] = '\0';
	return(i);
}
