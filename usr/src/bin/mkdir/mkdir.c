/*
 * Copyright (c) 1983, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1983, 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdir.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int	build __P((char *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, exitval, pflag;

	pflag = 0;
	while ((ch = getopt(argc, argv, "p")) != EOF)
		switch(ch) {
		case 'p':
			pflag = 1;
			break;
		case '?':
		default:
			usage();
		}

	if (!*(argv += optind))
		usage();

	for (exitval = 0; *argv; ++argv)
		if (pflag)
			exitval |= build(*argv);
		else if (mkdir(*argv, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
			warn("%s", *argv);
			exitval = 1;
		}
	exit(exitval);
}

int
build(path)
	char *path;
{
	register char *p;
	struct stat sb;
	int create, savech;

	p = path;
	if (*p)				/* Skip leading '/'. */
		++p;
	for (create = 0;; ++p)
		if (!*p || *p == '/') {
			savech = *p;
			*p = '\0';
			if (stat(path, &sb)) {
				if (errno != ENOENT || mkdir(path,
				    S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
					warn("%s", path);
					return (1);
				}
				create = 1;
			}
			if (!(*p = savech))
				break;
		}
	if (!create) {
		warnx("%s: %s", path, strerror(EEXIST));
		return (1);
	}
	return (0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: mkdir [-p] directory ...\n");
	exit (1);
}
