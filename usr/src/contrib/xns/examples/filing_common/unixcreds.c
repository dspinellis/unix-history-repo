#ifndef lint
static char *rcsid = "$Header: unixcreds.c,v 1.2 87/04/01 10:12:52 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	unixcreds.c,v $
 * Revision 1.2  87/04/01  10:12:52  ed
 * added Filing version 5.
 * 
 * Revision 1.1  87/01/14  11:28:25  ed
 * Initial revision
 * 
 */

#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#ifdef FILING4
#include "filingV4.h"
#include "clearinghouseV2.h"
#include "authenticationV2.h"
#endif FILING4
#ifdef FILING5
#include "filingV5.h"
#include "clearinghouseV2.h"
#include "authenticationV2.h"
#endif FILING5
#ifdef FILING6
#include "filingV6.h"
#include "clearinghouseV3.h"
#include "authenticationV3.h"
#endif FILING6
#ifdef FILINGSUBSET1
#include "filingsubsetV1.h"
#include "clearinghouseV3.h"
#include "authenticationV3.h"
#endif FILINGSUBSET1
#include <xnscourier/filing_server.h>
#include <xnscourier/CH.h>

extern char *AttrToString();

#ifdef DEBUG
FILE *msgs;
#endif DEBUG

get_name_and_pwd(creds, name, pwd)
FILING_SecondaryCredentials *creds;
char *name;
char *pwd;

{
	int i;
	FILING_SecondaryItem *item;
	Boolean gotname= FALSE, gotpwd= FALSE;
	char *user, *pass;

	 if ( creds->designator != FILING_simple ) {
			ReturnAuthenticationError(FILING_secondaryCredentialsRequired);
			/* NOT REACHED */
	}

#ifdef DEBUG
	fprintf(msgs, "%n secondary types\n", creds->FILING_simple_case.length);
#endif DEBUG

	if ( creds->FILING_simple_case.length <= 0 ) {
		ReturnAuthenticationError(FILING_secondaryCredentialsRequired);
		/* NOT REACHED */
	}

	for ( i= 0 ; i < creds->FILING_simple_case.length ; i++ ) {
		item= &creds->FILING_simple_case.sequence[i];

		if ( item->type == FILING_userName ) {
			gotname= TRUE;
			user= AttrToString(item);
			strcpy(name, user);
			clear_String(&user);
		} else if ( item->type == FILING_userPassword ) {
			gotpwd= TRUE;
			pass= AttrToString(item);
			strcpy(pwd, pass);
			clear_String(&pass);
		} else {
			ReturnAuthenticationError(FILING_secondaryCredentialsTypeInvalid);
			/* NOT REACHED */
		}

	}

	if ( !gotname && !gotpwd ) {
		ReturnAuthenticationError(FILING_secondaryCredentialsRequired);
		/* NOT REACHED */
	}

	return(-1);
}
