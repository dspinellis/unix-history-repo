/* $Header: bfsgetdoms.c,v 1.1 86/06/27 13:14:35 jqj Exp $ */
/* $Log:	bfsgetdoms.c,v $
 * Revision 1.1  86/06/27  13:14:35  jqj
 * Initial revision
 * 
 */
 
/* get domains served by a CHS server */

#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <xnscourier/Clearinghouse2.h>
#include <xnscourier/except.h>

static Cardinal nullhash = 0;
static Clearinghouse2_Authenticator nullagent = {{0,{0,(Unspecified*)0}},
						 {1,&nullhash}};
FILE *outfile;

#define MAXPACKS 20
static
listproc(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	Clearinghouse2_StreamOfDomainName names;

	bufend = buffer;
	bp = buffer+((MAXWORDS-1)*MAXPACKS);	/* end of avail. space */
	while ((count = BDTread(conn, (char*)bufend,
				MAXWORDS*sizeof(Unspecified))) > 0) {
		bufend += count/sizeof(Unspecified);
		if (bufend > bp) {
			fprintf(stderr,"BDT read too big to fit\n");
			BDTabort(conn);
		}
	}
	bp = buffer;
	while (bp < bufend) {
		bp += internalize_Clearinghouse2_StreamOfDomainName(&names,bp);
		if (0 == (int) names.designator) {
		    for (i=0; i < names.nextSegment_case.segment.length; i++) {
			printout(names.nextSegment_case.segment.sequence[i]);
		    }
		    free(names.nextSegment_case.segment.sequence);
		} else {
		    for (i=0; i < names.lastSegment_case.length; i++) {
			printout(names.lastSegment_case.sequence[i]);
		    }
		    free(names.lastSegment_case.sequence);
		    return;
		}
	}
}

static
printout(dname)
	Clearinghouse2_DomainName dname;
{
	/* print out a 2-part domain name */
	if (strcmp(dname.domain,"...") == 0 &&
	    strcmp(dname.organization,"...") == 0)
		return;
	fprintf(outfile," \":%s:%s\"",dname.domain,dname.organization);
}

printdomains(sns)
	struct ns_addr sns;
{
	CourierConnection *ccon;
	extern char *ns_ntoa();

	sns.x_port = 0;
	ccon = CourierOpen(&sns);
	if (ccon == (CourierConnection *)NULL) return;
	fprintf(outfile,"%s", ns_ntoa(sns));
	DURING
		Clearinghouse2_ListDomainServed(ccon, listproc,
				BulkData1_immediateSink, nullagent);
	HANDLER {
	} END_HANDLER;
	CourierClose(ccon);
	fprintf(outfile,"\n");
}
