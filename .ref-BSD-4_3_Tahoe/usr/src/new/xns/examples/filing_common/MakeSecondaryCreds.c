#ifndef lint
static char *rcsid = "$Header: MakeSecondaryCreds.c,v 1.5 87/04/16 15:47:29 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corp.
 */

/*
 *  $Log:	MakeSecondaryCreds.c,v $
 * Revision 1.5  87/04/16  15:47:29  ed
 * length becomes a Cardinal.
 * 
 * Revision 1.4  87/03/31  14:20:31  ed
 * Prompt with host name.
 * 
 * Revision 1.3  87/03/27  15:21:10  ed
 * Corrected missing argument on index().
 * 
 * Revision 1.2  87/03/18  08:42:08  ed
 * Minor changes.
 * 
 * Revision 1.1  86/12/31  11:26:19  ed
 * Initial revision
 * 
 * 
 */

#include <stdio.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include <xnscourier/FilingSubset1.h>
#include <xnscourier/CH.h>

MakeSecondaryCreds(host, user, pwd, creds)
char *host;
char *user;
char *pwd;
FilingSubset1_SecondaryCredentials *creds;
{
	Cardinal length;
	Unspecified buf[2048], *bp;
	static FilingSubset1_SecondaryItem items[2];
	FILE *tty;
	char *cp, buffer[128], *index(), *getXNSpass();
	char *userpwd, *username;

	if ( user == 0 ) {
		if ( (tty= fopen("/dev/tty", "r+")) == NULL )
			tty= stdin;
		else
			setbuf(tty, (char *)NULL);
		fprintf(tty, "Enter %s username: ", host);
		fgets(buffer, sizeof(buffer), tty);
		if ( (cp= index(buffer, '\n')) ) *cp= '\0';
		username= buffer;
		fclose(tty);
	} else {
		username= user;
	}

	if ( pwd == 0 ) {
		char prompt[100];
		sprintf(prompt,"Enter %s password: ", host);
		userpwd= getXNSpass(prompt);
	} else {
		userpwd= pwd;
	}

	if ( creds != 0 ) {
		creds->designator= FilingSubset1_simple;
		items[0].type= FilingSubset1_userName;
		items[0].value.sequence= Allocate(sizeof_String(&username));
		bp= buf + sizeof_Cardinal(length);
		length= externalize_String(&username, bp);
		externalize_Cardinal(&length, buf);
		internalize_Clearinghouse3_Item(&(items[0].value), buf);

		items[1].type= FilingSubset1_userPassword;
		items[1].value.sequence= Allocate(sizeof_String(&userpwd));
		bp= buf + sizeof_Cardinal(length);
		length= externalize_String(&userpwd, bp);
		externalize_Cardinal(&length, buf);
		internalize_Clearinghouse3_Item(&(items[1].value), buf);

		creds->designator= FilingSubset1_simple;
		creds->FilingSubset1_simple_case.length= 2;
		creds->FilingSubset1_simple_case.sequence= items;
	}		

	return;
}
