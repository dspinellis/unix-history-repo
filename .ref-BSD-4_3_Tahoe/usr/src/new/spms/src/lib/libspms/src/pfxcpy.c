/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pfxcpy() copies the prefix of s2 to s1.
 */
char *
pfxcpy(s1, s2)
	register char *s1;		/* target string */
	register char *s2;		/* source string */
{
	char *ss1 = s1;			/* start of s1 */

	while ((*s1 = *s2) && *s2 != '.')
		s1++, s2++;
	*s1 = '\0';
	return(ss1);
}
