# include	<stdio.h>
# include	<ctype.h>

# define	reg	register

# define	makelower(c)	(isupper(c) ? tolower(c) : c)

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */

strcmp(s1, s2)
reg char	*s1, *s2; {

	while (makelower(*s1) == makelower(*s2)) {
		if (*s1 == '\0')
			return 0;
		s1++, s2++;
	}
	return *s1 - *s2;
}
