#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/courier.h>
#include <xnscourier/except.h>
#include <xnscourier/CH.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	ObjectName myname, name, hint;
	struct xn_addr *destaddr, *getXNSaddr();
	Authenticator agent;
	CourierConnection *conn, *ch2conn;
	int i;
	ListPropertiesResults result;
	char *pwd, *getXNSpass();

	if (argc < 2 || argc >4) {
		fprintf(stderr,"Usage: %s name\n",argv[0]);
		exit(1);
	}
	CH_NameDefault(&myname);
	name = CH_StringToName(argv[1],&myname);
	myname.object = pwd = "";
	/* pwd = getXNSpass("Password:"); */
	MakeSimpleCredsAndVerifier(&myname,pwd,
			&agent.credentials, &agent.verifier);
	if (argc > 2)
		name.domain = argv[2];
	if (argc > 3)
		name.organization = argv[3];
	if ((conn = CH_GetFirstCH()) == NULL) {
		fprintf(stderr, "Can't open connection to local CH\n");
		exit(1);
	}
	DURING
		result = ListProperties(conn,NULL,name,agent);
	HANDLER {
		if (Exception.Code != WrongServer) {
			fprintf(stderr,"Oops.  Error\n");
			exit(1);
		}
		hint = CourierErrArgs(WrongServerArgs, hint);
		ch2conn = CH_GetOtherCH(conn, hint);
		CourierClose(conn);
		if (ch2conn == NULL) {
			fprintf(stderr,"Can't get to alternate CH\n");
			exit(1);
			}
		conn = ch2conn;
		DURING
			result = ListProperties(conn,NULL,name,agent);
		HANDLER {
			fprintf(stderr,"Oops.  Error.\n");
			exit(1);
		} END_HANDLER;
	} END_HANDLER;
	CourierClose(conn);
	printf("name: %s:%s:%s\n", result.distinguishedObject.object,
			result.distinguishedObject.domain,
			result.distinguishedObject.organization);
	for (i = 0; i < result.properties.length; i++)
		printf("\t%d:  %d\n",i,result.properties.sequence[i]>>16);
}
