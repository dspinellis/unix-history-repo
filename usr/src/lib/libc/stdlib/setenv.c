/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setenv.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stdio.h>

/*
 * setenv --
 *	Set the value of the environmental variable "name" to be
 *	"value".  If rewrite is set, replace any current value.
 */
setenv(name, value, rewrite)
	register char *name, *value;
	int rewrite;
{
	extern char **environ;
	static int alloced;			/* if allocated space before */
	register char *C;
	int l_value, offset;
	char *malloc(), *realloc(), *_findenv();

	if (*value == '=')			/* no `=' in value */
		++value;
	l_value = strlen(value);
	if ((C = _findenv(name, &offset))) {	/* find if already exists */
		if (!rewrite)
			return(0);
		if (strlen(C) >= l_value) {	/* old larger; copy over */
			while (*C++ = *value++);
			return(0);
		}
	}
	else {					/* create new slot */
		register int	cnt;
		register char	**P;

		for (P = environ, cnt = 0; *P; ++P, ++cnt);
		if (alloced) {			/* just increase size */
			environ = (char **)realloc((char *)environ,
			    (u_int)(sizeof(char *) * (cnt + 2)));
			if (!environ)
				return(-1);
		}
		else {				/* get new space */
			alloced = 1;		/* copy old entries into it */
			P = (char **)malloc((u_int)(sizeof(char *) *
			    (cnt + 2)));
			if (!P)
				return(-1);
			bcopy(environ, P, cnt * sizeof(char *));
			environ = P;
		}
		environ[cnt + 1] = NULL;
		offset = cnt;
	}
	for (C = name; *C && *C != '='; ++C);	/* no `=' in name */
	if (!(environ[offset] =			/* name + `=' + value */
	    malloc((u_int)((int)(C - name) + l_value + 2))))
		return(-1);
	for (C = environ[offset]; (*C = *name++) && *C != '='; ++C);
	for (*C++ = '='; *C++ = *value++;);
	return(0);
}

/*
 * unsetenv(name) --
 *	Delete environmental variable "name".
 */
void
unsetenv(name)
	char	*name;
{
	extern	char	**environ;
	register char	**P;
	int	offset;

	while (_findenv(name, &offset))		/* if set multiple times */
		for (P = &environ[offset];; ++P)
			if (!(*P = *(P + 1)))
				break;
}
