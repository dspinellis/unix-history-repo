#ifndef lint
static char sccsid[] = "@(#)fingerd.c	1.1 (Berkeley) %G%";
#endif

/*
 * Finger server.
 */
#include <sys/types.h>
#include <netinet/in.h>

#include <stdio.h>
#include <ctype.h>

main(argc, argv)
	char *argv[];
{
	register char **fargp;
	register char *cp;
	char *fargv[80], line[512];
	struct sockaddr_in sin;
	int i;

	i = sizeof (sin);
	if (getpeername(0, &sin, &i) < 0)
		fatal(argv[0], "getpeername");
	line[0] = '\0';
	gets(line);
	fargp = fargv;
	*fargp++ = "finger";
	cp = line;
	while (1) {
		while (isspace(*cp))
			cp++;
		if (!*cp)
			break;
		*fargp++ = cp;
		if (*cp == '/' && (cp[1] == 'W' || cp[1] == 'w')) {
			*cp++ = '-';
			*cp++ = 'l';
		}
		while (!isspace(*cp))
			cp++;
		*cp++ = '\0';
	}
	*fargp = NULL;
	execv("/usr/ucb/finger", fargv);
	execvp("finger", fargv);
	fatal(argv[0], "/usr/ucb/finger");
}

fatal(prog, s)
	char *prog, *s;
{

	fprintf(stderr, "%s: ", prog);
	perror(s);
	exit(1);
}
