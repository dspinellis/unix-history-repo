#include <stdio.h>
/* $Header: Authcredcheck.c,v 1.3 87/03/23 10:25:03 ed Exp $ */

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* contains:
 * Auth_Credcheck
 */

/* $Log:	Authcredcheck.c,v $
 * Revision 1.3  87/03/23  10:25:03  ed
 * Minor change.
 * 
 * Revision 1.2  87/01/13  16:37:16  ed
 * Added checks for null credentials formats
 * 
 * Revision 1.1  87/01/05  11:50:10  ed
 * Initial revision
 * 
 */
#include <sys/types.h>
#include <netns/ns.h>
#include "Authentication2_defs.h"
#include <xnscourier/courier.h>
#include <xnscourier/except.h>

/*
 * This module contains the routine:
 * Boolean Auth_CredCheck(creds, verifier)
 *	Credentials creds;
 *	Verifier verifier;
 */

Boolean Auth_CredCheck(creds, verifier)
	Credentials creds;
	Verifier verifier;
{
	CourierConnection *conn;
	extern CourierConnection *Auth_GetFirstAuth();
	CheckSimpleCredentialsResults result;
	Boolean retval;
	Unspecified *bp, buffer[SPPMAXDATA];
	ThreePartName chs_name;
	Cardinal len;
	
	if ( creds.type != simpleCredentials )
		return(FALSE);

	if ( creds.value.length == 0 )			/* nullCredentials */
		return(TRUE);

	externalize_Sequence_of_Unspecified(&creds.value, buffer);
	bp= buffer;
	bp += internalize_Cardinal(&len, bp);
	internalize_SimpleCredentials(&chs_name, bp);
	if ( chs_name.organization[0] == '\0' && chs_name.domain[0] == '\0' &&
			chs_name.object[0] == '\0' )
		return(TRUE);				/* is this valid ?? */

	if ((conn = Auth_GetFirstAuth()) == NULL) {
		fprintf(stderr,"Can't open connection to local Authentication service\n");
		return(FALSE);
	}

	DURING {
		result= CheckSimpleCredentials(conn, NULL, creds, verifier);
	} HANDLER {
		return(FALSE);
	} END_HANDLER;

	if ( result.ok == TRUE )
		retval= TRUE;
	else
		retval= FALSE;

	CourierClose(conn);
	return(retval);
}
