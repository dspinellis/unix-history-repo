#ifndef lint
static char sccsid[] = "@(#)fingerd.c	1.2 (Berkeley) %G%";
#endif

/*
 * Finger server.
 */
#include <sys/types.h>
#include <netinet/in.h>

#include <stdio.h>
#include <ctype.h>

FILE	*popen();

main(argc, argv)
	char *argv[];
{
	register char *cp, *sp;
	char cmdbuf[512], line[512];
	struct sockaddr_in sin;
	int i;
	FILE *fp;

	i = sizeof (sin);
	if (getpeername(0, &sin, &i) < 0)
		fatal(argv[0], "getpeername");
	line[0] = '\0';
	gets(line);
	sp = line;
	strcpy(cmdbuf, "/usr/ucb/finger");
	cp = cmdbuf + strlen(cmdbuf);
	while (1) {
		while (isspace(*sp))
			sp++;
		if (!*sp)
			break;
		*cp++ = ' ';
		if (*sp == '/' && (sp[1] == 'W' || sp[1] == 'w')) {
			sp += 2;
			*cp++ = '-';
			*cp++ = 'l';
		}
		while (!isspace(*sp))
			*cp++ = *sp++;
	}
	*cp++ = '\0';
	if ((fp = popen(cmdbuf, "r")) == NULL)
		fatal(argv[0], "/usr/ucb/finger");
	while ((i = getc(fp)) != EOF) {
		if (i == '\n')
			putchar('\r');
		putchar(i);
	}
	pclose(fp);
	return(0);
}

fatal(prog, s)
	char *prog, *s;
{

	fprintf(stderr, "%s: ", prog);
	perror(s);
	exit(1);
}
