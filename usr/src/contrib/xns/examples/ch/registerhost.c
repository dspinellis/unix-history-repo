/* $Header: registerhost.c,v 2.5 87/04/01 11:48:11 jqj Exp $ */

/*
 * This program enters a Unix host into the Clearinghouse as a server and
 * workstation, hence eligible for gap telnet 
 */
/*
 * $Log:	registerhost.c,v $
 * Revision 2.5  87/04/01  11:48:11  jqj
 * merged Webster changes: added -f switch for registering as file service
 * 
 * Revision 2.4  87/02/19  13:29:59  jqj
 * If hostname() returns a fully qualified Internet domain name, use only
 * the leaf (first) componenent.
 * 
 * Revision 2.3  86/12/15  11:27:06  jqj
 * rework address code to permit multihomed hosts.
 * 
 * Revision 2.2  86/05/07  14:05:27  jqj
 * eliminated use of ns_netof, since it has alignment problems on Gould
 * 
 * Revision 2.1  85/12/17  10:25:34  jqj
 * from Sklower:  default to our address as set by ifconfig
 * 
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
	union {
		u_short y_net[2];
		u_long y_long;
	} netvalue;

	s = addr->x_host.c_host;
	netvalue.y_net[0] = addr->x_net.s_net[0];
	netvalue.y_net[1] = addr->x_net.s_net[1];
	sprintf(buf,"%lx#%x.%x.%x.%x.%x.%x#%x",
		ntohl(netvalue.y_long),
		s[0], s[1], s[2], s[3], s[4], s[5],
		ntohs(addr->x_port));
	return(buf);
}


addNAelem(nalistp, addr)
	NetworkAddressList *nalistp;
	struct ns_addr *addr;
{
	register NetworkAddress *naddrp;
	
	naddrp = nalistp->sequence + nalistp->length;			
	nalistp->length++;
	naddrp->network[0] = htons(addr->x_net.s_net[0]);
	naddrp->network[1] = htons(addr->x_net.s_net[1]);
	naddrp->host[0] = htons(addr->x_host.s_host[0]);
	naddrp->host[1] = htons(addr->x_host.s_host[1]);
	naddrp->host[2] = htons(addr->x_host.s_host[2]);
}

Item
getaddresslist(myaddr)
	struct ns_addr *myaddr;
{
	char addrstr[200];
	struct ns_addr addr;
	extern struct ns_addr ns_addr();
	NetworkAddressList nalist;
	Unspecified buf[501], *bp;
	Item item;
	Cardinal len;
	NetworkAddress naddrs[10];
	
	nalist.length = 0;
	nalist.sequence = naddrs;
	printf("NS address (e.g. 2-273#2-613-688-939-672, - for default, CR to end):\nPrimary NS address: ");
	
	/* get list of addresses */
	gets(addrstr);
	while (*addrstr != 0) {
		if (*addrstr=='-') {
			addNAelem(&nalist, myaddr);
		} else {
			addr = ns_addr(addrstr);
			addNAelem(&nalist, &addr);
		}
		printf("Additional NS address (CR for none): ");
		gets(addrstr);
	}
	
	/* canonicalize socket #s */
	for (len = 0; len < nalist.length; len++)
		nalist.sequence[len].socket = 0;
		
	/* encode */
	bp = buf + sizeof_Cardinal(len);
	len = externalize_NetworkAddressList(&nalist, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Item(&item, buf);
	return(item);
}

main(argc, argv)
	int argc;
	char *argv[];
{
	ObjectName myname, name, defname, fsname;
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
		mystrname[200], *myhostname;
	String ItemToString();
	Item item, StringToItem(), addrToItem();
	Property propnum;
	struct ns_addr *addr;
	static Boolean authseq[2] = {1, 0};	/* simple, not strong */
	Boolean fileservice= 0;
	int opt;
	extern int optind;
	extern char *optarg;

	if (argc < 1 || argc > 5) {
		fprintf(stderr,"Usage: %s [-f] [hostname]\n",argv[0]);
		exit(1);
	}
	while ( (opt= getopt(argc, argv, "f")) != EOF )
		switch (opt) {
			case 'f' :
				fileservice++;
				break;

			default :
				fprintf(stderr,  "Invalid command option -%c\n", opt);
				exit(1);
		}
	CH_NameDefault(&defname);
	if (argc == optind) {
	    char *tmp; extern char *index();
	    gethostname(myhostname=malloc(100), 100);
	    if ((tmp = index(myhostname,'.')) != NULL)
	    	*tmp = '\0';
	    name = CH_StringToName(myhostname, &defname);
	} else
	    name = CH_StringToName(argv[optind], &defname);
	printf("Registering host %s\n",
		CH_NameToString(name) );
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
		myaddr->x_port = 0;
		item = getaddresslist(myaddr);
		DURING CIresult = ChangeItem(conn,NULL, name,4,item,agent);
		HANDLER {
		  fprintf(stderr,"Can't change address.  Error %d\n",
			 Exception.Code);
		  exit(1);
		} END_HANDLER;
	} else {
		item = getaddresslist(myaddr);
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
		propnum = 10005; /* 10005<<16; */
		if (isprop(propnum,LPresult.properties)) {
			RIresult = RetrieveItem(conn, NULL,
					name,
					propnum, agent);
			propval = ItemToString(RIresult.value);
			printf("Workstation description (Property 10005) has value ``%s''\n",
			       propval );
			clear_RetrieveItemResults(&RIresult);
		}
		propnum = 10024; /* 10024<<16; */
		if (isprop(propnum,LPresult.properties)) {
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
			clear_RetrieveItemResults(&RIresult);
		} else {
			printf("Enter new description: ");
			propval = gets(malloc(100));
			item = StringToItem(propval);
			AIPresult = AddItemProperty(conn, NULL,
					name,
					propnum, item, agent);
		}
		if ( fileservice ) {
			propnum= 10000; /* 10000<<16 */
			if (isprop(10000,LPresult.properties)) {
				RIresult = RetrieveItem(conn, NULL,
						name,
						propnum, agent);
				propval = ItemToString(RIresult.value);
				printf("FileService description (Property 10000) has value ``%s''\nNew value: ",
				       propval );
				propval = gets(malloc(100));
				item = StringToItem(propval);
				CIresult = ChangeItem(conn, NULL,
						name,
						propnum, item, agent);
			} else {
				printf("Enter new FileService description: ");
				propval = gets(malloc(100));
				item = StringToItem(propval);
				AIPresult = AddItemProperty(conn, NULL,
						name,
						propnum, item, agent);
			}
		}
	} HANDLER {
		fprintf(stderr,
			"Error during Property manipulations, %d (%d)\n",
			Exception.Code,
			CourierErrArgs(Clearinghouse2_CallErrorArgs,problem) );
	} END_HANDLER;
}

