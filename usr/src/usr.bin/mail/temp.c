/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
static char sccsid[] = "@(#)temp.c	5.5 (Berkeley) %G%";
#endif /* notdef */

#include "rcv.h"

/*
 * Mail -- a mail program
 *
 * Give names to all the temporary files that we will need.
 */

char	tempMail[14];
char	tempQuit[14];
char	tempEdit[14];
char	tempSet[14];
char	tempResid[14];
char	tempMesg[14];

tinit()
{
	register char *cp;
	char uname[PATHSIZE];
	register int pid;
	uid_t getuid();

	pid = getpid();
	sprintf(tempMail, "/tmp/Rs%05d", pid);
	sprintf(tempResid, "/tmp/Rq%05d", pid);
	sprintf(tempQuit, "/tmp/Rm%05d", pid);
	sprintf(tempEdit, "/tmp/Re%05d", pid);
	sprintf(tempSet, "/tmp/Rx%05d", pid);
	sprintf(tempMesg, "/tmp/Rx%05d", pid);

	if (strlen(myname) != 0) {
		uid = getuserid(myname);
		if (uid == -1) {
			printf("\"%s\" is not a user of this system\n",
			    myname);
			exit(1);
		}
	}
	else {
		uid = getuid();
		if (username(uid, uname) < 0) {
			strcpy(myname, "ubluit");
			if (rcvmode) {
				printf("Who are you!?\n");
				exit(1);
			}
		} else
			strcpy(myname, uname);
	}
	if ((cp = value("HOME")) == NOSTR)
		cp = ".";
	strcpy(homedir, cp);
	strcpy(copy(homedir, mailrc), "/.mailrc");
	strcpy(copy(homedir, deadletter), "/dead.letter");
	if (debug) {
		printf("uid = %d, user = %s\n", uid, myname);
		printf("deadletter = %s, mailrc = %s\n", deadletter, mailrc);
	}
}
