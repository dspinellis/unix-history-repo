/* $Header: getXNSuser.c,v 1.2 87/07/28 08:39:23 ed Exp $ */

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	getXNSuser.c,v $
 * Revision 1.2  87/07/28  08:39:23  ed
 * fprintf to stderr instead of stdin (how did it work?).
 * 
 * Revision 1.1  87/03/23  10:26:08  ed
 * Initial revision
 * 
 */

#include <stdio.h>
#include <xnscourier/Authentication2.h>
#include <xnscourier/Clearinghouse2.h>
#include <ctype.h>

			/* This should be constant somewhere ?? */
static char name[85];	/* 80 is max three part name + 3 for separators(:) */
static Cardinal passwd;

getXNSuser(xnsname, xnspwd)
char **xnsname;
Cardinal **xnspwd;
{
	char *pwd, *cp, *username;
	char *getXNSpass(), *getenv(), *index();
	FILE *tty, *fopen();

	if ( ((username= getenv("XNSNAME")) == NULL) || (username[0] == '\0') ) {
		if ( (tty= fopen("/dev/tty", "r+")) == NULL ) {
			tty= stdin;
		} else {
			setbuf(tty, (char *)NULL);
		}
		fprintf(stderr, "Enter XNS name: ");
		fgets(name, sizeof(name), tty);
		if ( (cp= index(name, '\n')) )
			*cp= '\0';
		if ( name[0] == '\0' )
			*xnsname= "Unknown";
		else
			*xnsname= name;
		fclose(tty);
	} else {
		*xnsname= username;
	}

	if ( ((pwd= getenv("XNSPASSWD")) == NULL) || (*pwd == '\0') ) {
		pwd= getXNSpass("Enter XNS password: ");
		passwd= hashpass(pwd);
	} else {
		passwd= atoi(pwd);
	}

	*xnspwd= &passwd;
}

