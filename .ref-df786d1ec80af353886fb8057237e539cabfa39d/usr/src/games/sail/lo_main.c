#ifndef lint
static	char *sccsid = "@(#)lo_main.c	1.3 83/07/20";
#endif

/*
 * Print out the top ten SAILors
 *
 * sail.log [-s/l]
 *
 *  -s force a short listing (without real usernames)
 *  -l force a long listing (print out real usernames)
 */
#include <pwd.h>
#include "externs.h"

char *title[] = {
	"Admiral", "Commodore", "Captain", "Captain",
	"Captain", "Captain", "Captain", "Commander",
	"Commander", "Lieutenant"
};

main(argc, argv)
int argc;
char **argv;
{
	FILE *fp;
	char sbuf[32];
	int n = 0, people;
	int usrnam = 0;
	struct passwd *getpwuid(), *pass;
	struct logs log;
	struct ship *ship;

	if (argc > 1 && argc == 2)
		if (strcmp(argv[1], "-s") == 0)
			usrnam = 0;
		else if (strcmp(argv[1], "-l") == 0)
			usrnam = 1;
		else {
			fprintf(stderr, "usage: %s: [-s/l]\n", argv[0]);
			exit(1);
		}
	if ((fp = fopen(LOGFILE, "r")) == 0) {
		perror(LOGFILE);
		exit(1);
	}
	switch (fread((char *)&people, sizeof people, 1, fp)) {
	case 0:
		printf("Nobody has sailed yet.\n");
		exit(0);
	case 1:
		break;
	default:
		perror(LOGFILE);
		exit(1);
	}
	while (fread((char *)&log, sizeof log, 1, fp) == 1
	       && log.l_name[0] != '\0') {
		if (usrnam && (pass = getpwuid(log.l_uid)) != NULL)
			(void) sprintf(sbuf, "%10.10s (%s)",
				log.l_name, pass->pw_name);
		else
			(void) sprintf(sbuf, "%20.20s", log.l_name);
		ship = &scene[log.l_gamenum].ship[log.l_shipnum];
		printf("%-10s %21s of the %15s %3d points, %5.2f equiv\n",
			title[n++], sbuf, ship->shipname, log.l_netpoints,
			(float) log.l_netpoints / ship->specs->pts);
	}
	printf("\n%d people have played.\n", people);
}
