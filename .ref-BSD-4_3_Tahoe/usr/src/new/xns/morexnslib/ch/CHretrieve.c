#include <stdio.h>

/* contains:
 * CH_RetrieveItem
 */

#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/CHEntries.h>
#include <xnscourier/except.h>

/*
 * This module contains the routine:
 * CH_RetrieveItem(pattern,property,result)
 *	ObjectNamePattern pattern;
 *	Property property;
 *	RetrieveItemResults *result;
 */

static Cardinal nullhash = 0;
static Authenticator nullagent = {{0,{0,(Unspecified*) 0}},
				  {1,&nullhash}};
static ObjectName currentname;
extern struct ns_addr *LookupCHAddr();

CH_RetrieveItem(pattern,property,result)
	ObjectNamePattern pattern;
	Property property;
	RetrieveItemResults *result;
{
	CourierConnection *conn, *ch2conn;
	extern CourierConnection *CH_GetFirstCH(), *CH_GetOtherCH();
	RetrieveItemResults riresult;
	ObjectName hint;		/* from WrongServer errors */

	if (pattern.object == NULL ||
	    pattern.domain == NULL ||
	    pattern.organization == NULL) {
		return(1);
	    }

	if ((conn = CH_GetFirstCH()) == NULL) {
		fprintf(stderr,"Can't open connection to local Clearinghouse\n");
		return(1);
	}
	DURING {
		riresult= RetrieveItem(conn, NULL,
				pattern,property,nullagent);
	} HANDLER {
		if (Exception.Code == REJECT_ERROR) {
		    CourierClose(conn);
		    fprintf(stderr,"Problem with clearinghouse.addresses. Local CH rejected request\n");
		    return(1);
		}

		if (Exception.Code != WrongServer) {
		    CourierClose(conn);
		    return(1);	/* some random error */
		}
		hint = CourierErrArgs(WrongServerArgs,hint);
		ch2conn = CH_GetOtherCH(conn,hint);
		CourierClose(conn);
		if (ch2conn == NULL) return(1);
		conn = ch2conn;
		/* probe the second clearinghouse */
		DURING
			riresult = RetrieveItem(conn,NULL,
				pattern, property, nullagent);
		HANDLER {
			/* should be smarter is WrongServer here */
			CourierClose(conn);
			return(1);
		} END_HANDLER;
		/* we got it */
	} END_HANDLER;

	CourierClose(conn);

	bcopy(&riresult, result, sizeof(Clearinghouse2_RetrieveItemResults));

	return(0);
}


