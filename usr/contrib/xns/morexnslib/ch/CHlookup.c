/* $Header: CHlookup.c,v 2.0 85/11/21 07:22:32 jqj Exp $ */

/* contains:
 * CH_LookupAddr
 * CH_GetFirstCH
 * CH_GetOtherCH
 * CH_NameDefault
 */

/* $Log:	CHlookup.c,v $
 * Revision 2.0  85/11/21  07:22:32  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/11/21  07:14:19  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.2  85/10/21  12:41:55  jqj
 * Gould version:  work around a Gould compiler bug.  Default to /usr/new
 * for consistency.
 * 
 * Revision 1.1  85/10/18  09:14:53  jqj
 * Initial revision
 * 
 * Revision 1.1  85/03/26  06:27:00  jqj
 * Initial revision
 * 
 */
#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <xnscourier/Clearinghouse2.h>
#include <xnscourier/CHEntries.h>
#include <xnscourier/except.h>
#define MAXPACKS 5
#define NLSIZE 50

static Clearinghouse2_ObjectName *namelist[NLSIZE];
static int nlcount = 0;

static Cardinal nullpasswd = 0;
static Clearinghouse2_Authenticator nullagent = {{0,{0,0}},{1,&nullpasswd}};

static
GetData(conn)
	CourierConnection *conn;
{
	int count, i;
	Unspecified buffer[MAXWORDS*MAXPACKS], *bp, *bufend;
	Clearinghouse2_StreamOfObjectName obnames;
	
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
		bp += internalize_Clearinghouse2_StreamOfObjectName(&obnames,bp);
		if (0 == (int) obnames.designator) {
		   for (i = 0; i < obnames.nextSegment_case.segment.length; i++)
			if (nlcount < NLSIZE) namelist[nlcount++] =
				&obnames.nextSegment_case.segment.sequence[i];
		} else {
		   for (i = 0; i < obnames.lastSegment_case.length; i++)
			if (nlcount < NLSIZE) namelist[nlcount++] =
				&obnames.lastSegment_case.sequence[i];
		   return;
		}
	}
}

static struct ns_addr*
itemtonsaddr(itemptr)
	Clearinghouse2_Item *itemptr;	/* i.e. a sequence of Unspecified */
{
	static union {
		struct ns_addr addr;
		u_short shrt[6];
	} result;
	register int i;
	register Unspecified *seq;

	if (itemptr->length < 7)
		return(0);

	seq = itemptr->sequence +1;
	/* itemptr->sequence[0] == number of addresses */
	for (i = 0; i < 6; i++, seq++)
		result.shrt[i] = ntohs(*seq);
	return(&result.addr);
}

/*
 * path to look for the addresses file on
 * (I wish I'd picked a shorter name, so we could rendezvous in /etc.
 *  But long file names in /etc are very unpopular!).
 */
static char *chaddrpath[] = {
#ifdef CHADDRS
			CHADDRS,
#endif
			"/usr/new/lib/xnscourier/clearinghouse.addresses",
			"/etc/clearinghouse.addresses",
			"/usr/local/lib/xnscourier/clearinghouse.addresses",
			0 };


/*
 * Set defaults for organization and domain, based on the file
 * /usr/local/lib/xnscourier/clearinghouse.addresses
 * (should get the local clearinghouse and set defaults based on
 * ListDomainsServed
 */
CH_NameDefault(defaultobjnamep)
	Clearinghouse2_ObjectName *defaultobjnamep;
{
	FILE *chfile;
	int i, nmatch;
	static char orgbuf[21], domainbuf[21];
	
	defaultobjnamep->object = "";
	for (i=0; chaddrpath[i] != NULL; i++) {
		chfile = fopen(chaddrpath[i],"r");
		if (chfile != (FILE*)0) {
			nmatch = fscanf(chfile,"%*[^ \t\n] \":%[^:]:%[^\"]",
					domainbuf, orgbuf);
			fclose(chfile);
			if (nmatch == 2) {
				defaultobjnamep->domain = domainbuf;
				defaultobjnamep->organization = orgbuf;
				return; /* done */
			}
		}
	}
	defaultobjnamep->organization = "";
	defaultobjnamep->domain = "";
}


CourierConnection*
CH_GetFirstCH()
{
	struct ns_addr *chaddr;
	extern struct ns_addr *getXNSaddr();
	char buf[BUFSIZ];
	CourierConnection *result;
	FILE *chfile;
	int i;

	/* for now, use hard-coded CH */
	/* eventually we'll do an expanding-ring or something */
	result = (CourierConnection *) NULL;
	for (i = 0; chaddrpath[i] != NULL; i++) {
		if ((chfile = fopen(chaddrpath[i],"r")) != NULL) {
			while (fgets(buf, BUFSIZ, chfile))
				if ((chaddr = getXNSaddr(buf)) &&
				    (result = CourierOpen(chaddr))) {
					fclose(chfile);
					return(result);
				}
			fclose(chfile);
			break;	/* don't look for other files */
		}
	}
	/* try expanding-ring here */
	return(result);
}

CourierConnection *
CH_GetOtherCH(conn,hint)
	CourierConnection *conn;
	Clearinghouse2_ObjectName hint;
{
	struct ns_addr *ch2addr;
	CourierConnection *ch2conn;
	Clearinghouse2_RetrieveItemResults riresult;
	Clearinghouse2_RetrieveMembersResults rmresult;
	int i;

	Clearinghouse2_Property clearinghouseNames = 3;
	Clearinghouse2_Property clearinghouseAddresses = 4;
	Clearinghouse2_ObjectName hint1;

	DURING
		/* get list of possible other clearinghouses */
		rmresult = Clearinghouse2_RetrieveMembers(conn, GetData, hint, 
			clearinghouseNames, BulkData1_immediateSink,
			nullagent);
	HANDLER {
		/* some race condition */
		return(NULL);
	} END_HANDLER;
	/* throw away the distinguished name, which we don't need */
	clear_Clearinghouse2_RetrieveMembersResults(&rmresult);
	/* for each member of potentials list, probe it */
	ch2conn = NULL;
	for (i = 0; i < nlcount; i++) {
		Clearinghouse2_ObjectName thisname;
		thisname = *namelist[i];
		/* get its address */
		DURING
			riresult = Clearinghouse2_RetrieveItem(conn, NULL, 
				thisname,
				clearinghouseAddresses, nullagent);
		HANDLER
			continue;	/* try next in namelist */
		END_HANDLER;
		ch2addr = itemtonsaddr(&riresult.value);
		clear_Clearinghouse2_RetrieveItemResults(&riresult);
		ch2conn = CourierOpen(ch2addr);
		if (ch2conn == NULL) continue;
		/* make sure the second CH is really there */
		DURING
			(void) Clearinghouse2_RetrieveAddresses(ch2conn, NULL);
		HANDLER {
			CourierClose(ch2conn);
			ch2conn = NULL;
			continue;
		} END_HANDLER;
		/* we got it! */
		break;
	}
	for (i = 0; i < nlcount; i++) {
			/* free namelist[i] components */
			clear_Clearinghouse2_ObjectName(namelist[i]);
			/* free the top level thing too */
			Deallocate((Unspecified*) namelist[i]);
		}
	return(ch2conn);
}

/*
 * Given a Clearinghouse three part name (possibly containing wild cards)
 * and the property number on which a NetworkAddress is expected to occur,
 * returns the ns_addr structure associated with that name.
 * Note that the ns_addr structure is statically allocated!
 * Resets pattern to be the distinguished name of the object found.
 */
struct ns_addr *
CH_LookupAddr(pattern,property)
	Clearinghouse2_ObjectNamePattern pattern;
	Clearinghouse2_Property property;
{
	struct ns_addr* CH_LookupAddrDN();
	return(CH_LookupAddrDN(pattern,property,0,0));
}

/*
 * Lookup a clearinghouse address, returning the address and the
 * distinguished name of the object found.
 */
struct ns_addr *
CH_LookupAddrDN(pattern,property,distnamep,distnamelen)
	Clearinghouse2_ObjectNamePattern pattern;
	Clearinghouse2_Property property;
	char *distnamep;
	int distnamelen;
{
	struct ns_addr *chaddr, *resultaddr;
	CourierConnection *conn, *ch2conn;
	Clearinghouse2_ObjectName hint;		/* from WrongServer errors */
	Clearinghouse2_RetrieveItemResults riresult;


	if ((long) property <= 0) 	/* default to addressList i.e. 4 */
		property = CHEntries0_addressList;
	if (pattern.object == NULL ||
	    pattern.domain == NULL ||
	    pattern.organization == NULL)
		return(NULL);			/* can't handle defaults */
	
	if ((conn = CH_GetFirstCH()) == NULL) {
		fprintf(stderr,"Can't open connection to local Clearinghouse\n");
		exit(1);
	}
	/* ask our primary clearinghouse for the address of this thing */
	DURING {
		riresult = Clearinghouse2_RetrieveItem(conn, NULL,
				pattern, property, nullagent);
	} HANDLER {
		if (Exception.Code != Clearinghouse2_WrongServer) {
			CourierClose(conn);
			return(0);	/* some random error */
		}
		hint = CourierErrArgs(Clearinghouse2_WrongServerArgs, hint);
		ch2conn = CH_GetOtherCH(conn, hint);
		CourierClose(conn);
		if (ch2conn == NULL) return(0);
		conn = ch2conn;
		/* probe the second clearinghouse */
		DURING
			riresult = Clearinghouse2_RetrieveItem(conn, NULL,
				pattern, property, nullagent);
		HANDLER {
			/* should be smarter if WrongServer here */
			CourierClose(conn);
			return(0);
		} END_HANDLER;
		/* we got it */
	} END_HANDLER;
	resultaddr = itemtonsaddr(&riresult.value);
	if (distnamep != NULL &&
	    distnamelen >= (2+strlen(riresult.distinguishedObject.object)+
			    strlen(riresult.distinguishedObject.domain)+
			    strlen(riresult.distinguishedObject.organization)))
		sprintf(distnamep,"%s:%s:%s",
			riresult.distinguishedObject.object,
			riresult.distinguishedObject.domain,
			riresult.distinguishedObject.organization);
	clear_Clearinghouse2_RetrieveItemResults(&riresult);
	CourierClose(conn);
	return(resultaddr);
}
