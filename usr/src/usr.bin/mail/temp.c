/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)temp.c	5.17 (Berkeley) %G%";
#endif /* not lint */

#include "rcv.h"
#include <errno.h>
#include "extern.h"

/*
 * Mail -- a mail program
 *
 * Give names to all the temporary files that we will need.
 */

char	tempMail[24];
char	tempQuit[24];
char	tempEdit[24];
char	tempResid[24];
char	tempMesg[24];
char	*tmpdir;

void
tinit()
{
	register char *cp;
	int len;

	if ((tmpdir = getenv("TMPDIR")) == NULL)
		tmpdir = _PATH_TMP;
	else {
		len = strlen(tmpdir);
		if ((cp = malloc(len + 2)) == NULL) {
			(void)fprintf(stderr, "mail: %s\n", strerror(errno));
			exit (1);
		}
		(void)strcpy(cp, tmpdir);
		cp[len] = '/';
		cp[len + 1] = '\0';
		tmpdir = cp;
	}
			
	strcpy(tempMail, tmpdir);
	mktemp(strcat(tempMail, "RsXXXXXX"));
	strcpy(tempResid, tmpdir);
	mktemp(strcat(tempResid, "RqXXXXXX"));
	strcpy(tempQuit, tmpdir);
	mktemp(strcat(tempQuit, "RmXXXXXX"));
	strcpy(tempEdit, tmpdir);
	mktemp(strcat(tempEdit, "ReXXXXXX"));
	strcpy(tempMesg, tmpdir);
	mktemp(strcat(tempMesg, "RxXXXXXX"));

	/*
	 * It's okay to call savestr in here because main will
	 * do a spreserve() after us.
	 */
	if (myname != NOSTR) {
		if (getuserid(myname) < 0) {
			printf("\"%s\" is not a user of this system\n",
			    myname);
			exit(1);
		}
	} else {
		if ((cp = username()) == NOSTR) {
			myname = "ubluit";
			if (rcvmode) {
				printf("Who are you!?\n");
				exit(1);
			}
		} else
			myname = savestr(cp);
	}
	if ((cp = getenv("HOME")) == NOSTR)
		cp = ".";
	homedir = savestr(cp);
	if (debug)
		printf("user = %s, homedir = %s\n", myname, homedir);
}
