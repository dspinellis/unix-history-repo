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
static char sccsid[] = "@(#)temp.c	5.10 (Berkeley) %G%";
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
	char buf[PATHSIZE];

	mktemp(strcpy(tempMail, "/tmp/RsXXXXXX"));
	mktemp(strcpy(tempResid, "/tmp/RqXXXXXX"));
	mktemp(strcpy(tempQuit, "/tmp/RmXXXXXX"));
	mktemp(strcpy(tempEdit, "/tmp/ReXXXXXX"));
	mktemp(strcpy(tempSet, "/tmp/RxXXXXXX"));
	mktemp(strcpy(tempMesg, "/tmp/RxXXXXXX"));

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
	if ((cp = value("HOME")) == NOSTR)
		cp = ".";
	homedir = savestr(cp);
	sprintf(buf, "%s/.mailrc", homedir);
	mailrc = savestr(buf);
	if (debug)
		printf("user = %s, mailrc = %s\n", myname, mailrc);
}
