#ifndef lint
static char sccsid[] = "@(#)getcom.c	1.1 %G%";
#endif

#include <stdio.h>
#include <ctype.h>

char *
getcom(buf, size, prompt, error)
	char *buf;
	int size;
	char *prompt, *error;
{
	for (;;) {
		fputs(prompt, stdout); 
		if (fgets(buf, size, stdin) == 0)
			exit(1);
		while (isspace(*buf))
			buf++;
		if (*buf)
			break;
		if (error)
			puts(error);
	}
	return (buf);
}

char *
getword(buf1, buf2, flag)
	char *buf1, *buf2;
{
	/* shifts to UPPERCASE if flag > 0, lowercase if    */
	/* flag < 0, and leaves it unchanged if flag = 0    */

	while (isspace(*buf1))
		buf1++;
	if (*buf1 != ',') {
		if (!*buf1) {
			*buf2 = 0;
			return (0);
		}
		while (*buf1 && !isspace(*buf1) && *buf1 != ',')
			*buf2++ = shift(*buf1++, flag);
	} else
		*buf2++ = *buf1++;
	*buf2 = 0;
	while (isspace(*buf1))
		buf1++;
	return (*buf1 ? buf1 : 0);
}

shift(c, flg)
	char c;
	int flg;
{
	if (flg < 0)
		return isupper(c) ? tolower(c) : c;
	if (flg > 0)
		return islower(c) ? toupper(c) : c;
	return c;
}
