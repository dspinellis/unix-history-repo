#include <stdio.h>
/* $Header: CHenumerate.c,v 2.2 87/04/01 12:42:12 jqj Exp $ */

/* contains:
 * CH_Enumerate
 */

/* $Log:	CHenumerate.c,v $
 * Revision 2.2  87/04/01  12:42:12  jqj
 * reworked wrongserver code to correspond to new GetOtherCH semantics
 * 
 * Revision 2.1  87/03/17  09:59:08  ed
 * webster changes: handle WrongServer error
 * 
 * Revision 2.1  87/03/17  09:59:08  ed
 * Handle WrongServer error from first Clearinghouse
 * 
 * Revision 2.0  85/11/21  07:22:30  jqj
 * *** empty log message ***
 * 
 * Revision 2.0  85/11/21  07:22:30  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/03/26  06:26:59  jqj
 * Initial revision
 * 
 * Revision 1.1  85/03/26  06:26:59  jqj
 * Initial revision
 * 
 */
#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/CHEntries.h>
#include <xnscourier/except.h>

/*
 * This module contains the routine:
 * int CH_Enumerate(pattern,property,eachName)
 *	ObjectNamePattern pattern;
 *	Property property;
 *	NameProcedure eachName;
 * where NameProcedure is a procedure of the form:
 * int eachName(name)
 *	Object name;
 */
#define MAXPACKS 5

static (*ProcEachName)();		/* use:  (*ProcEachName)(arg); */
static Cardinal nullhash = 0;
static Authenticator nullagent = {{0,{0,(Unspecified*) 0}},
				  {1,&nullhash}};
static ObjectName currentname;
extern struct ns_addr *LookupCHAddr();

static
listObject(name)
	ObjectName name;
{
	printf("%s:%s:%s\n", name.object, name.domain, name.organization);
}

static
GetObjects(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	StreamOfObject obnames;
	
	bufend = buffer;
	bp = buffer+((MAXWORDS-1)*MAXPACKS);    /* end of available space */
	while (count = BDTread(conn, (char*)bufend, 
				MAXWORDS*sizeof(Unspecified))
		) {
		bufend += count/sizeof(Unspecified);
		if (bufend > bp) {
			fprintf(stderr,"BDT read too big to fit\n");
			BDTabort(conn);
			/* should clear out stuff here if we knew how much */
		}
	}
	bp = buffer;
	while (bp < bufend) {
		bp += internalize_StreamOfObject(&obnames,bp);
		if (0 == (int) obnames.designator) {
		   for (i=0; i < obnames.nextSegment_case.segment.length; i++) {
			currentname.object =
				obnames.nextSegment_case.segment.sequence[i];
			(*ProcEachName)(currentname);
		   }
		   free(obnames.nextSegment_case.segment.sequence);
		} else {
		   for (i = 0; i < obnames.lastSegment_case.length; i++) {
			currentname.object =
				obnames.lastSegment_case.sequence[i];
			(*ProcEachName)(currentname);
		   }
		   free(obnames.lastSegment_case.sequence);
		   return;
		}
	}
}

CH_Enumerate(pattern,property,eachName)
	ObjectNamePattern pattern;
	Property property;
	int (*eachName)();
{
	CourierConnection *conn, *ch2conn;
	Clearinghouse2_ObjectName hint;		/* from WrongServer errors */
	extern CourierConnection *CH_GetFirstCH(), *CH_GetOtherCH();
	int tries, retval;

	if (eachName != NULL)
		ProcEachName = eachName;
	else
		ProcEachName = listObject;
	if (pattern.object == NULL ||
	    pattern.domain == NULL ||
	    pattern.organization == NULL)
		return(1);
	currentname.domain = pattern.domain;
	currentname.organization = pattern.organization;

	if ((conn = CH_GetFirstCH()) == NULL) {
		fprintf(stderr,"Can't open connection to local Clearinghouse\n");
		return(1);
	}
	retval = -1;
	for (tries = 5; tries > 0 && retval == -1; tries--) {
	    DURING {
		ListObjects(conn, GetObjects, pattern, property,
				BulkData1_immediateSink, nullagent);
		retval = 0;
	    } HANDLER {
		if (Exception.Code == REJECT_ERROR) {
			if ((ch2conn = CH_GetOtherCH(conn, NULL)) == NULL)
				retval = Exception.Code;
			else {
				CourierClose(conn);
				conn = ch2conn;
			}
		} else if (Exception.Code == Clearinghouse2_WrongServer) {
			hint = CourierErrArgs(Clearinghouse2_WrongServerArgs, hint);
			if ((ch2conn = CH_GetOtherCH(conn, hint)) == NULL)
				retval = 1;
			else {
				CourierClose(conn);
				conn = ch2conn;
			}
		} else {
			retval = Exception.Code;
		}
	    } END_HANDLER;
	}
	CourierClose(conn);
	return(retval);
}
