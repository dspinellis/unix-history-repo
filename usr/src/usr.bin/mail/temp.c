/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)temp.c	5.8 (Berkeley) %G%";
#endif /* not lint */

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
	int uid;

	mktemp(strcpy(tempMail, "/tmp/RsXXXXXX"));
	mktemp(strcpy(tempResid, "/tmp/RqXXXXXX"));
	mktemp(strcpy(tempQuit, "/tmp/RmXXXXXX"));
	mktemp(strcpy(tempEdit, "/tmp/ReXXXXXX"));
	mktemp(strcpy(tempSet, "/tmp/RxXXXXXX"));
	mktemp(strcpy(tempMesg, "/tmp/RxXXXXXX"));

	if (strlen(myname) != 0) {
		uid = getuserid(myname);
		if (uid == -1) {
			printf("\"%s\" is not a user of this system\n",
			    myname);
			exit(1);
		}
	} else {
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
