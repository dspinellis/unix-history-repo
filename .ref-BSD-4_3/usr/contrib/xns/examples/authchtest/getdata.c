#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include "Clearinghouse_support.c"
#include <xnscourier/except.h>
#define MAXPACKS 5
#define nextSegment_case nextSegment2_33_case
#define lastSegment_case lastSegment2_34_case

ProcessObjectName(obj)
	ObjectName obj;
{
	printf("\t%s:%s:%s\n", obj.object, obj.domain, obj.organization);
}

GetData(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	StreamOfObjectName obnames;
	
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
		bp += internalize_StreamOfObjectName(&obnames,bp);
		if (0 == (int) obnames.designator)
		   for (i = 0; i < obnames.nextSegment_case.segment.length; i++)
			ProcessObjectName(
				obnames.nextSegment_case.segment.sequence[i]);
		else {
		   for (i = 0; i < obnames.lastSegment_case.length; i++)
			ProcessObjectName(
				obnames.lastSegment_case.sequence[i]);
		   return;
		}
	}
}
