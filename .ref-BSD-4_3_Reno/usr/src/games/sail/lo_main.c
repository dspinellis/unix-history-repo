/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lo_main.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Print out the top ten SAILors
 *
 * -l force a long listing (print out real usernames)
 */
#include <sys/types.h>
#include <pwd.h>
#include "externs.h"
#include "pathnames.h"

char *title[] = {
	"Admiral", "Commodore", "Captain", "Captain",
	"Captain", "Captain", "Captain", "Commander",
	"Commander", "Lieutenant"
};

lo_main()
{
	FILE *fp;
	char sbuf[32];
	int n = 0, people;
	struct passwd *getpwuid(), *pass;
	struct logs log;
	struct ship *ship;

	if ((fp = fopen(_PATH_LOGFILE, "r")) == 0) {
		perror(_PATH_LOGFILE);
		exit(1);
	}
	switch (fread((char *)&people, sizeof people, 1, fp)) {
	case 0:
		printf("Nobody has sailed yet.\n");
		exit(0);
	case 1:
		break;
	default:
		perror(_PATH_LOGFILE);
		exit(1);
	}
	while (fread((char *)&log, sizeof log, 1, fp) == 1 &&
	       log.l_name[0] != '\0') {
		if (longfmt && (pass = getpwuid(log.l_uid)) != NULL)
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
	return 0;
}
