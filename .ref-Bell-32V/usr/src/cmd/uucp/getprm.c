#include <stdio.h>


/*******
 *	char *
 *	getprm(s, prm)	get next parameter from s
 *	char *s, *prm;
 *
 *	return - pointer to next character in s
 */

char *
getprm(s, prm)
char *s, *prm;
{

	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;

	*prm = '\0';
	if (*s == '\0')
		return(NULL);

	if (*s == '>' || *s == '<' || *s == '|'
	|| *s == ';') {
		*prm++ = *s++;
		*prm = '\0';
		return(s);
	}

	while (*s != ' ' && *s != '\t' && *s != '<'
	&& *s != '>' && *s != '|' && *s != '\0'
	&& *s != ';' && *s != '\n')
		*prm++ = *s++;
	*prm = '\0';

	return(s);
}
