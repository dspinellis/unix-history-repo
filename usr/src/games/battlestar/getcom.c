/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)getcom.c	5.2 (Berkeley) 6/19/88";
#endif /* not lint */

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
		if (fgets(buf, size, stdin) == 0) {
			clearerr(stdin);
			continue;
		}
		while (isspace(*buf))
			buf++;
		if (*buf)
			break;
		if (error)
			puts(error);
	}
	return (buf);
}


/*
 * shifts to UPPERCASE if flag > 0, lowercase if flag < 0,
 * and leaves it unchanged if flag = 0
 */
char *
getword(buf1, buf2, flag)
	register char *buf1, *buf2;
	register flag;
{
	while (isspace(*buf1))
		buf1++;
	if (*buf1 != ',') {
		if (!*buf1) {
			*buf2 = 0;
			return (0);
		}
		while (*buf1 && !isspace(*buf1) && *buf1 != ',')
			if (flag < 0)
				if (isupper(*buf1))
					*buf2++ = tolower(*buf1++);
				else
					*buf2++ = *buf1++;
			else if (flag > 0)
				if (islower(*buf1))
					*buf2++ = toupper(*buf1++);
				else
					*buf2++ = *buf1++;
			else
				*buf2++ = *buf1++;
	} else
		*buf2++ = *buf1++;
	*buf2 = 0;
	while (isspace(*buf1))
		buf1++;
	return (*buf1 ? buf1 : 0);
}
