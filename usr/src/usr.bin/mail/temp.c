/*
 * Copyright (c) 1980 Regents of the University of California.
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
static char sccsid[] = "@(#)temp.c	5.14 (Berkeley) 6/1/90";
#endif /* not lint */

#include "rcv.h"

/*
 * Mail -- a mail program
 *
 * Give names to all the temporary files that we will need.
 */

char	tempMail[24];
char	tempQuit[24];
char	tempEdit[24];
char	tempSet[24];
char	tempResid[24];
char	tempMesg[24];

tinit()
{
	register char *cp;

	strcpy(tempMail, _PATH_TMP);
	mktemp(strcat(tempMail, "RsXXXXXX"));
	strcpy(tempResid, _PATH_TMP);
	mktemp(strcat(tempResid, "RqXXXXXX"));
	strcpy(tempQuit, _PATH_TMP);
	mktemp(strcat(tempQuit, "RmXXXXXX"));
	strcpy(tempEdit, _PATH_TMP);
	mktemp(strcat(tempEdit, "ReXXXXXX"));
	strcpy(tempSet, _PATH_TMP);
	mktemp(strcat(tempSet, "RxXXXXXX"));
	strcpy(tempMesg, _PATH_TMP);
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
