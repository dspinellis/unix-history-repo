/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)exec.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/types.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <string.h>
#include <stdio.h>
#include <paths.h>

extern char **environ;

static char **
buildargv(ap, arg, envpp)
	va_list ap;
	const char *arg;
	char ***envpp;
{
	register size_t max, off;
	register char **argv = NULL;

	for (off = max = 0;; ++off) {
		if (off >= max) {
			max += 50;	/* Starts out at 0. */
			max *= 2;	/* Ramp up fast. */
			if (!(argv = realloc(argv, max * sizeof(char *))))
				return(NULL);
			if (off == 0) {
				argv[0] = (char *)arg;
				off = 1;
			}
		}
		if (!(argv[off] = va_arg(ap, char *)))
			break;
	}
	/* Get environment pointer if need user supposed to provide one. */
	if (envpp)
		*envpp = va_arg(ap, char **);
	return(argv);
}

int
#if __STDC__
execl(const char *name, const char *arg, ...)
#else
execl(name, arg, va_alist)
	const char *name;
	const char *arg;
	va_dcl
#endif
{
	va_list ap;
	int sverrno;
	char **argv;

#if __STDC__
	va_start(ap, arg);
#else
	va_start(ap);
#endif
	if (argv = buildargv(ap, arg, (char ***)NULL))
		(void)execve(name, argv, environ);
	va_end(ap);
	sverrno = errno;
	free(argv);
	errno = sverrno;
	return(-1);
}

int
#if __STDC__
execle(const char *name, const char *arg, ...)
#else
execle(name, arg, va_alist)
	const char *name;
	const char *arg;
	va_dcl
#endif
{
	va_list ap;
	int sverrno;
	char **argv, **envp;

#if __STDC__
	va_start(ap, arg);
#else
	va_start(ap);
#endif
	if (argv = buildargv(ap, arg, &envp))
		(void)execve(name, argv, envp);
	va_end(ap);
	sverrno = errno;
	free(argv);
	errno = sverrno;
	return(-1);
}

int
#if __STDC__
execlp(const char *name, const char *arg, ...)
#else
execlp(name, arg, va_alist)
	const char *name;
	const char *arg;
	va_dcl
#endif
{
	va_list ap;
	int sverrno;
	char **argv;

#if __STDC__
	va_start(ap, arg);
#else
	va_start(ap);
#endif
	if (argv = buildargv(ap, arg, (char ***)NULL))
		(void)execvp(name, argv);
	va_end(ap);
	sverrno = errno;
	free(argv);
	errno = sverrno;
	return(-1);
}

int
execv(name, argv)
	const char *name;
	char * const *argv;
{
	(void)execve(name, argv, environ);
	return(-1);
}

int
execvp(name, argv)
	const char *name;
	char * const *argv;
{
	register char *p;
	int eacces, etxtbsy;
	char *cur, *path, buf[MAXPATHLEN];

	/* If it's an absolute or relative path name, it's easy. */
	if (index(name, '/')) {
		(void)execve(name, argv, environ);
		return(-1);
	}

	/* Get the path we're searching. */
	if (!(path = getenv("PATH")))
		path = _PATH_DEFPATH;
	cur = path = strdup(path);

	eacces = etxtbsy = 0;
	while (p = strsep(&cur, ":")) {
		/*
		 * It's a SHELL path -- double, leading and trailing colons
		 * mean the current directory.
		 */
		if (!*p)
			p = ".";
		(void)snprintf(buf, sizeof(buf), "%s/%s", p, name);

retry:		(void)execve(buf, argv, environ);
		switch(errno) {
		case EACCES:
			eacces = 1;
			break;
		case ENOENT:
			break;
		case ENOEXEC: {
			register size_t cnt;
			register char **ap;

			for (cnt = 0, ap = (char **)argv; *ap; ++ap, ++cnt);
			if (ap = malloc((cnt + 2) * sizeof(char *))) {
				bcopy(argv, ap + 2, cnt * sizeof(char *));
				ap[0] = "sh";
				ap[1] = buf;
				(void)execve(_PATH_BSHELL, ap, environ);
				free(ap);
			}
			goto done;
		}
		case ETXTBSY:
			if (etxtbsy < 3)
				(void)sleep(++etxtbsy);
			goto retry;
		default:
			goto done;
		}
	}
	if (eacces)
		errno = EACCES;
	else if (!errno)
		errno = ENOENT;
done:	free(path);
	return(-1);
}
