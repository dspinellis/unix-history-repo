#ifndef lint
static char sccsid[] = "@(#)uupoll.c	5.6	(Berkeley) 4/5/88";
#endif

/*
 * Poll named system(s).
 *
 * The poll occurs even if recent attempts have failed,
 * but not if L.sys prohibits the call (e.g. wrong time of day).
 *
 * Original Author: Tom Truscott (rti!trt)
 */

#include "uucp.h"

int TransferSucceeded = 1;
struct timeb Now;

main(argc, argv)
int argc;
char **argv;
{
	char wrkpre[MAXFULLNAME];
	char file[MAXFULLNAME];
	char grade = 'A';
	int nocall = 0;
	int c;
	char *sysname;
	extern char *optarg;
	extern int optind;

	if (argc < 2) {
		fprintf(stderr, "usage: uupoll [-gX] [-n] system ...\n");
		cleanup(1);
	}

	if (chdir(Spool) < 0) {
		syslog(LOG_WARNING, "chdir(%s) failed: %m", Spool);
		cleanup(1);
	}
	strcpy(Progname, "uupoll");
	uucpname(Myname);

	while ((c = getopt(argc, argv, "g:n")) != EOF)
		switch(c) {
			case 'g':
				grade = *optarg;
				break;
			case 'n':
				nocall++;
				break;
			case '?':
			default:
				fprintf(stderr, "unknown option %s\n",
					argv[optind-1]);
		}

	while(optind < argc) {
		sysname = argv[optind++];
		if (strcmp(sysname, Myname) == SAME) {
			fprintf(stderr, "This *is* %s!\n", Myname);
			continue;
		}

		if (versys(&sysname)) {
			fprintf(stderr, "%s: unknown system.\n", sysname);
			continue;
		}
		/* Remove any STST file that might stop the poll */
		sprintf(wrkpre, "%s/LCK..%.*s", LOCKDIR, MAXBASENAME, sysname);
		if (access(wrkpre, 0) < 0)
			rmstat(sysname);
		sprintf(wrkpre, "%c.%.*s", CMDPRE, SYSNSIZE, sysname);
		if (!iswrk(file, "chk", Spool, wrkpre)) {
			sprintf(file, "%s/%c.%.*s%cPOLL", subdir(Spool, CMDPRE),
				CMDPRE, SYSNSIZE, sysname, grade);
			close(creat(file, 0666));
		}
		/* Attempt the call */
		if (!nocall)
			xuucico(sysname);
	}
	cleanup(0);
}

cleanup(code)
int code;
{
	exit(code);
}
