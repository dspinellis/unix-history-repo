/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vipw.c	5.16 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *progname = "vipw";
char *tempname;

main()
{
	register int pfd, tfd;
	struct stat begin, end;

	pw_init();
	pfd = pw_lock();
	tfd = pw_tmp();
	copyfile(pfd, tfd);
	(void)close(tfd);

	for (;;) {
		if (stat(tempname, &begin))
			pw_error(tempname, 1, 1);
		pw_edit(0);
		if (stat(tempname, &end))
			pw_error(tempname, 1, 1);
		if (begin.st_mtime == end.st_mtime) {
			(void)fprintf(stderr, "vipw: no changes made\n");
			pw_error((char *)NULL, 0, 0);
		}
		if (pw_mkdb())
			break;
		pw_prompt();
	}
	exit(0);
}

copyfile(from, to)
	register int from, to;
{
	register int nr, nw, off;
	char buf[8*1024];
	
	while ((nr = read(from, buf, sizeof(buf))) > 0)
		for (off = 0; off < nr; nr -= nw, off += nw)
			if ((nw = write(to, buf + off, nr)) < 0)
				pw_error(tempname, 1, 1);
	if (nr < 0)
		pw_error(_PATH_MASTERPASSWD, 1, 1);
}
