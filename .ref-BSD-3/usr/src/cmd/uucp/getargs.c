#include <stdio.h>


/*******
 *	getargs(s, arps)
 *	char *s, *arps[];
 *
 *	getargs  -  this routine will generate a vector of
 *	pointers (arps) to the substrings in string "s".
 *	Each substring is separated by blanks and/or tabs.
 *
 *	return - the number of subfields.
 */

getargs(s, arps)
char *s, *arps[];
{
	int i;

	i = 0;
	while (1) {
		arps[i] = NULL;
		while (*s == ' ' || *s == '\t')
			*s++ = '\0';
		if (*s == '\n')
			*s = '\0';
		if (*s == '\0')
			break;
		arps[i++] = s++;
		while (*s != '\0' && *s != ' '
			&& *s != '\t' && *s != '\n')
				s++;
	}
	return(i);
}
