#ifndef lint
static char *rcsid = "$Header: misc.c,v 1.2 87/03/31 14:29:13 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	misc.c,v $
 * Revision 1.2  87/03/31  14:29:13  ed
 * Minor change.
 * 
 * Revision 1.1  87/01/14  11:26:10  ed
 * Initial revision
 * 
 *
 */

#include <ctype.h>

/* change case to upper*/
char *uppercase(string1)
char	*string1;

{
	register char *string2 = string1;


	while (*string2) {
		if (islower(*string2))
			*string2 = *string2 - 040;
		string2++;
	}

	return (string1);
}

/* change counted string to upper */
char *uppercasen(string1, count)
char *string1;
int count;

{
	register char *string2= string1;

	while (count--) {
		if (islower(*string2))
			*string2= *string2 - 040;
		string2++;
	}

	return(string1);
}

/* change case to lower */
char *lowercase(string1)
char	*string1;

{
	register char *string2 = string1;


	while (*string2) {
		if (isupper(*string2))
			*string2 = *string2 + 040;
		string2++;
	}

	return (string1);

}

/* change counted string to lower */
char *lowercasen(string1, count)
char *string1;
int count;

{
	register char *string2= string1;

	while (count--) {
		if (isupper(*string2))
			*string2= *string2 + 040;
		string2++;
	}

	return(string1);
}
