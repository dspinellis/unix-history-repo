/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getenv.c	5.7 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <stddef.h>

/*
 * getenv --
 *	Returns ptr to value associated with name, if any, else NULL.
 */
char *
getenv(name)
	char *name;
{
	int offset;
	char *_findenv();

	return(_findenv(name, &offset));
}

/*
 * _findenv --
 *	Returns pointer to value associated with name, if any, else NULL.
 *	Sets offset to be the offset of the name/value combination in the
 *	environmental array, for use by setenv(3) and unsetenv(3).
 *	Explicitly removes '=' in argument name.
 *
 *	This routine *should* be a static; don't use it.
 */
char *
_findenv(name, offset)
	register char *name;
	int *offset;
{
	extern char **environ;
	register int len;
	register char **P, *C;

	for (C = name, len = 0; *C && *C != '='; ++C, ++len);
	for (P = environ; *P; ++P)
		if (!strncmp(*P, name, len))
			if (*(C = *P + len) == '=') {
				*offset = P - environ;
				return(++C);
			}
	return(NULL);
}
