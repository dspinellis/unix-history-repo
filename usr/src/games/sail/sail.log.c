#ifndef lint
static	char *sccsid = "@(#)sail.log.c	1.3 83/06/02";
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
    int usrnam = SAILLOGDEF;
    struct passwd *getpwuid(), *pass;
    struct logs flog;

    if (argc > 1)
	if (argc == 2)
	    if (!strcmp(argv[1], "-s"))
		usrnam = 0;
	    else if (!strcmp(argv[1], "-l"))
		usrnam = 1;
	else {
	    fprintf(stderr, "usage: %s: [-s/l]\n", argv[0]);
	    exit(1);
	}
    if((fp = fopen(LOGFILE, "r")) == 0) {
	printf("%s: Error opening logfile - %s\n", argv[0], LOGFILE);
	exit(1);
    }
    if (fread(&people, sizeof(people), 1, fp) == 0) {
	printf("%s: Error reading logfile.\n", argv[0]);
	exit(1);
    }
    while ((fread(&flog, sizeof(flog), 1, fp) != 0) && (flog.fname[0] != '\0')) {
	if (usrnam && ((pass = getpwuid(flog.uid)) != NULL))
	    sprintf(sbuf, "%10.10s (%s)", flog.fname, pass->pw_name);
	else
	    sprintf(sbuf, "%20.20s", flog.fname);
	printf("%-10s %21s of the %15s %3d points, %5.2f equiv\n",
	  title[n++], sbuf,
	  scene[flog.fgamenum].ship[flog.fshipnum].shipname,
	  flog.netpoints,
	  (float) flog.netpoints /
	  specs[scene[flog.fgamenum].ship[flog.fshipnum].shipnum].pts);
    }
    printf("\n%d people have played.\n", people);
}
