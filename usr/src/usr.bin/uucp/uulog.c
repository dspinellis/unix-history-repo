#ifndef lint
static char sccsid[] = "@(#)uulog.c	5.6	(Berkeley) 5/4/88";
#endif

#include "uucp.h"

struct timeb Now;

main(argc, argv)
char *argv[];
{
#ifndef LOGBYSITE
	FILE *plogf;
	char u[64], s[64];
#endif /* !LOGBYSITE */
	char *sys, *user;
	int c;
	extern char *optarg;
	extern int optind;

	char buf[BUFSIZ];

	strcpy(Progname, "uulog");
	sys = user = NULL;

	while ((c = getopt(argc, argv, "s:u:")) != EOF)
		switch (c) {
		case 's':
			sys = optarg;
			if (strlen(sys) > MAXBASENAME)
				sys[MAXBASENAME] = '\0';
			if (versys(&sys) != SUCCESS){
				fprintf(stderr,"uulog: unknown system %s\n", sys);
				sys = NULL;
			}
			break;
		case 'u':
			user = optarg;
			break;
		case '?':
		default:
			fprintf(stderr, "unknown flag %s\n", argv[optind-1]);
			break;
		}

	if (user == NULL && sys == NULL) {
		fprintf(stderr, "usage: uulog [-u user] [-s sys]\n");
		exit(1);
	}

#ifdef LOGBYSITE
	if (chdir(SPOOL) < 0) {
		perror(SPOOL);
		exit(1);
	}
	/* this program is really obsolete, this is a rude backward compat */
	if (user) {
		sprintf(buf, "exec cat LOG/uu*/* | egrep '^%s '", user);
		system(buf);
	}
	if (sys) {
		sprintf(buf,"exec cat LOG/uu*/%s", sys);
		system(buf);
	}
#else !LOGBYSITE
	plogf = fopen(LOGFILE, "r");
	if (plogf == NULL) {
		syslog(LOG_WARNING, "fopen(%s) failed: %m", LOGFILE);
		cleanup(1);
	}
	while (fgets(buf, BUFSIZ, plogf) != NULL) {
		sscanf(buf, "%s%s", u, s);
		if (user != NULL && !(prefix(user, u) || prefix(u, user)))
			continue;
		if (sys != NULL && !(prefix(sys, s) || prefix(s, sys)))
			continue;
		fputs(buf, stdout);
		fflush(stdout);
	}
#endif !LOGBYSITE
	exit(0);
}

cleanup(code)
int code;
{
	exit(code);
}
