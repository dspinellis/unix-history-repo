/* $Header: registerhost.c,v 2.0a 85/11/21 07:22:36 jqj,sklower Exp $ */

/*
 * This program enters a Unix host into the Clearinghouse as a server and
 * workstation, hence eligible for gap telnet 
 */
/*
 * $Log:	registerhost.c,v $
 * Revision 2.0  85/11/21  07:22:36  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  13:54:08  jqj
 * Initial revision
 * 
 */

#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/CHEntries.h>
#include <xnscourier/CH.h>
#include <xnscourier/except.h>



char *
ItemToString(item)
	Item item;
{
	char *strval;

	Unspecified buf[501], *bp;
	Cardinal len;

	externalize_Item(&item, buf);
	bp = buf;
	bp += internalize_Cardinal(&len, bp);
	bp += internalize_String(&strval, bp);	
	return(strval);
}

Item
StringToItem(strval)
	String strval;
{
	Unspecified buf[501], *bp;
	Item item;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_String(&strval, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Item(&item, buf);
	return(item);
}

Item
addrToItem(addr)
	struct ns_addr *addr;
{
	Unspecified buf[501], *bp;
	Item item;
	Cardinal len;
	NetworkAddressList nalist;
	NetworkAddress naddr;

	nalist.length = 1;
	nalist.sequence = &naddr;
	naddr.network[0] = htons(addr->x_net.s_net[0]);
	naddr.network[1] = htons(addr->x_net.s_net[1]);
	naddr.host[0] = htons(addr->x_host.s_host[0]);
	naddr.host[1] = htons(addr->x_host.s_host[1]);
	naddr.host[2] = htons(addr->x_host.s_host[2]);
	naddr.socket = 0;
	
	bp = buf + sizeof_Cardinal(len);
	len = externalize_NetworkAddressList(&nalist, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Item(&item, buf);
	return(item);
}

int
isprop(prop, proplist)
	Property prop;
	Properties proplist;
{
	int i;

	prop = prop<<16;	/* correct for bug in Clearinghouse v 2 */
	for (i=0; i < proplist.length; i++) {
		if (proplist.sequence[i] == prop) return(1);
	}
	return(0);		/* not found */
}

char *
XNSaddrToString(addr)
	struct ns_addr *addr;
{
	u_char *s;
	static char buf[21];

	s = addr->x_host.c_host;
	sprintf(buf,"%lx#%x.%x.%x.%x.%x.%x#%x",
		ntohl(ns_netof(*addr)),
		s[0], s[1], s[2], s[3], s[4], s[5],
		ntohs(addr->x_port));
	return(buf);
}


main(argc, argv)
	int argc;
	char *argv[];
{
	ObjectName myname, name, defname;
	struct ns_addr *destaddr, *getXNSaddr(), *myaddr;
	struct sockaddr_ns sns;
	Authenticator agent;
	CourierConnection *conn;
	int i;
	ListPropertiesResults LPresult;
	RetrieveItemResults RIresult;
	ChangeItemResults CIresult;
	AddItemPropertyResults AIPresult;
	char *pwd, *propval, *getXNSpass(), *malloc(), *gets(), 
		mystrname[200], addrstr[200], *myhostname;
	String ItemToString();
	Item item, StringToItem(), addrToItem();
	Property propnum;
	struct ns_addr *addr;
	static Boolean authseq[2] = {1, 0};	/* simple, not strong */

	if (argc < 1 || argc >4) {
		fprintf(stderr,"Usage: %s [hostname]\n",argv[0]);
		exit(1);
	}
	CH_NameDefault(&defname);
	if (argc == 1) {
	    gethostname(myhostname=malloc(100), 100);
	  name = CH_StringToName(myhostname, &defname);
	} else
	  name = CH_StringToName(argv[1], &defname);
	if (argc > 2)
		name.domain = argv[2];
	if (argc > 3)
		name.organization = argv[3];
	printf("Registering host %s:%s:%s\n",
	       name.object,name.domain,name.organization);
	printf("XNS UserName: ");
	gets(mystrname);
	myname = CH_StringToName(mystrname, &name);
	pwd = getXNSpass("Password:");
	MakeSimpleCredsAndVerifier(&myname,pwd,
			&agent.credentials, &agent.verifier);
	conn = CH_GetFirstCH();
	i = sizeof(sns); myaddr = &sns.sns_addr;
	getsockname(*(int *)conn, &sns, &i); myaddr->x_port = 0;
	DURING {
		LPresult = ListProperties(conn,NULL,name,agent);
	} HANDLER {
		if (Exception.Code == ArgumentError) {
		  DURING CreateObject(conn,NULL,name,agent);
		  HANDLER {
			fprintf(stderr,"Can't create object. Error %d\n",
				Exception.Code);
			exit(1);
		  } END_HANDLER;
		  LPresult.properties.length = 0;
		}
		else {
		  fprintf(stderr,"ListProperties failed.  Error %d\n",
			Exception.Code);
		  exit(1);
		}
	} END_HANDLER;
	/* AddressList property */
	if (isprop(4,LPresult.properties)) {
		addr = CH_LookupAddr(name,4);
		if (addr==0) {
		    fprintf(stderr,"NetworkAddress is in property list, but CH_LookupAddr failed\n");
		    exit(1);
		}
		printf("Old address: %s\n",XNSaddrToString(addr));
		printf("New address: ");
		gets(addrstr);
		if (*addrstr==0 && addr->x_port!=0) {
		    printf("Warning:  port should be 0 for gaptelnet\n");
		    printf("New address: ");
		    gets(addrstr);
		}
		if (*addrstr!=0) {
		    addr = getXNSaddr(addrstr);
		} else
		    addr = myaddr;
		item = addrToItem(addr);
		DURING CIresult = ChangeItem(conn,NULL, name,4,item,agent);
		HANDLER {
		  fprintf(stderr,"Can't change address.  Error %d\n",
			 Exception.Code);
		  exit(1);
		} END_HANDLER;
	} else {
		printf("NS address (e.g. 2-273#2-613-688-939-672): ");
		gets(addrstr);
		if (*addrstr==0) {
			addr = myaddr;
		} else
			addr = getXNSaddr(addrstr);
		item = addrToItem(addr);
		DURING
		  AIPresult = AddItemProperty(conn,NULL,name,4,item,agent);
		HANDLER {
		  fprintf(stderr,"Can't add address property.  Error %d\n",
			Exception.Code);
		  exit(1);
		} END_HANDLER;
	}
	/* AuthenticationLevel property */
	if (!isprop(8,LPresult.properties)) {
		item.length = 2;
		item.sequence = (Unspecified *) authseq;
		DURING
		  AIPresult = AddItemProperty(conn,NULL,name,8,item,agent);
		HANDLER {
		  fprintf(stderr,"Can't add AuthenticationLevel property.\n");
		  exit(1);
		} END_HANDLER;
	}
	/* description */
	DURING {
		if (isprop(10005,LPresult.properties)) {
			propnum = 10005; /* 10005<<16; */
			RIresult = RetrieveItem(conn, NULL,
					name,
					propnum, agent);
			propval = ItemToString(RIresult.value);
			printf("Workstation description (Property 10005) has value ``%s''\n",
			       propval );
			clear_RetrieveItemResults(&RIresult);
		}
		propnum = 10024; /* 10024<<16; */
		if (isprop(10024,LPresult.properties)) {
			RIresult = RetrieveItem(conn, NULL,
					name,
					propnum, agent);
			propval = ItemToString(RIresult.value);
			printf("Server description (Property 10024) has value ``%s''\nNew value: ",
			       propval );
			propval = gets(malloc(100));
			item = StringToItem(propval);
			CIresult = ChangeItem(conn, NULL,
					name,
					propnum, item, agent);
		} else {
			printf("Enter new description: ");
			propval = gets(malloc(100));
			item = StringToItem(propval);
			AIPresult = AddItemProperty(conn, NULL,
					name,
					propnum, item, agent);
		}
	} HANDLER {
		fprintf(stderr,
			"Error during Property manipulations, %d (%d)\n",
			Exception.Code,
			CourierErrArgs(Clearinghouse2_CallErrorArgs,problem) );
	} END_HANDLER;
}
