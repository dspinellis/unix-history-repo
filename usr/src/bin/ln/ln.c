static	char sccsid[] = "@(#)ln.c 3.1 %G%";
/*
 * ln [ -f ] file ... name
 *
 * name must be dir if more than one file given
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

struct stat	stbuf;

int		fflag;		/* force flag set? */

char		_sobuf[BUFSIZ];

main(argc, argv)
register int	argc;
register char	*argv[];
{
	register	i, r;

	if (argc > 1 && strcmp(argv[1], "-f") == 0) {
		fflag++;
		argv++;
		argc--;
	}
	if (argc < 2) 
		goto usage;
	else if (argc == 2)
		argv[argc] = ".";
	if (argc > 3) {
		if (stat(argv[argc-1], &stbuf) < 0)
			goto usage;
		if ((stbuf.st_mode&S_IFMT) != S_IFDIR) 
			goto usage;
	}
	r = 0;
	for(i=1; i<argc-1;i++)
		r |= linkit(argv[i], argv[argc-1]);
	exit(r);
usage:
	fprintf(stderr, "Usage: ln: f1 f2; or ln f1 ... fn d2\n");
	exit(1);
}

linkit(from, to)
register char	*from, *to;
{
	register char	*p1, *p2, *bp;

	/* is target a directory? */
	if (fflag == 0 && stat(from, &stbuf) >= 0
	    && (stbuf.st_mode&S_IFMT) == S_IFDIR) {
		printf("%s is a directory\n", from);
		return 1;
	}
	if (stat(to, &stbuf) >=0 && (stbuf.st_mode&S_IFMT) == S_IFDIR) {
		p1 = from;
		p2 = to;
		bp = _sobuf;
		while(*bp++ = *p2++)
			continue;
		bp[-1] = '/';
		p2 = bp;
		while(*bp = *p1++)
			if (*bp++ == '/')
				bp = p2;
		to = _sobuf;
	}
	else
		p1 = NULL;
	if (link(from, to) < 0) {
		perror(from);
		return 1;
	}
	return(0);
}
