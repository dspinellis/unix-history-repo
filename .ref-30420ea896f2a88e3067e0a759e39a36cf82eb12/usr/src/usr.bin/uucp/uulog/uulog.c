#ifndef lint
static char sccsid[] = "@(#)uulog.c	5.1 (Berkeley) %G%";
#endif

#include "uucp.h"

#ifndef	SYSBUF
static char SYSBUF[BUFSIZ];
#endif

/*******
 *
 *	uulog  -  
 *
 *	options:
 *		-s  -  system name for search
 *		-u  -  user name for search
 *
 *	exit codes:
 *		0  -  normal
 *
 */

main(argc, argv)
char *argv[];
{
	FILE *plogf;
	char *system, *user;

	char buf[BUFSIZ], u[20], s[20];

	setbuf(stdout, SYSBUF);
	strcpy(Progname, "uulog");
	system = user = NULL;


	while (argc>1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 's':
			system = &argv[1][2];
			if (*system == NULL && argc > 2 && argv[2][0] != '-') {
				system = &argv[2][0];
				argv++;
				argc--;
			}
			if (strlen(system) > 7)
				system[7] = 0;
			break;
		case 'u':
			user = &argv[1][2];
			if (*user == NULL && argc > 2 && argv[2][0] != '-') {
				user = &argv[2][0];
				argv++;
				argc--;
			}
			break;
		default:
			printf("unknown flag %s\n", argv[1]); break;
		}
		--argc;  argv++;
	}


	if (user == NULL && system == NULL) {
		fprintf(stderr, "usage: uulog [-u user] [-s system]\n");
		exit(1);
	}
/*	chmod(LOGFILE, 0666);	rm-ed by rti!trt */

	plogf = fopen(LOGFILE, "r");
	ASSERT(plogf != NULL, "CAN NOT OPEN", LOGFILE, 0);
	while (fgets(buf, BUFSIZ, plogf) != NULL) {
		sscanf(buf, "%s%s", u, s);
		if (user != NULL && !prefix(user, u))
			continue;
		if (system != NULL && !prefix(system, s))
			continue;
		fputs(buf, stdout);
		fflush(stdout);
	}
	exit(0);
}

cleanup(code)
int code;
{
	exit(code);
}
