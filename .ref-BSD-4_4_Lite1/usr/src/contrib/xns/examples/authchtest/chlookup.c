#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include "Clearinghouse_defs.h"
#include <xnscourier/except.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	LookupObjectResults result;
	struct ns_addr *destaddr;
	CourierConnection *conn;
	extern struct ns_addr *getXNSaddr();
	ObjectNamePattern name;
	ObjectName myname;
	char *mypwd;
	extern char *getpass();
	Authenticator agent;

	if (argc == 1)
		/* default to Dandelion 2-272, 2-852-151-014 */
		destaddr = getXNSaddr("8E0#00.00.AA.00.7D.E7");
	else if (argc != 2 || (destaddr = getXNSaddr(argv[1]))==NULL) {
		fprintf(stderr,"Usage: %s machine\n",argv[0]);
		exit(1);
	}
	if ((conn = CourierOpen(destaddr)) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",argv[1]);
		exit(1);
	}
	myname.organization = name.organization = "Berkeley.EECS";
	myname.domain = name.domain = "Evans";
	name.object = "bill";
	myname.object = "sklower";
	mypwd = getpass("Password:");
	MakeSimpleCredentialsAndVerifier(&myname, mypwd,
		&agent.credentials,
		&agent.verifier );

	DURING
		result = LookupObject(conn,NULL,name,agent);
	HANDLER {
		switch (Exception.Code) {
		case CallError:
			fprintf(stderr,"Call error, %d\n",
				CourierErrArgs(CallErrorArgs,problem));
			break;
		case ArgumentError:
			fprintf(stderr,"Argument error (5d,%d)\n",
				CourierErrArgs(ArgumentErrorArgs,problem),
				CourierErrArgs(ArgumentErrorArgs,which) );
			break;
		case AuthenticationError:
			fprintf(stderr,"Authentication error, %d\n",
				CourierErrArgs(AuthenticationErrorArgs,problem)
				);
			break;
		case WrongServer:
			fprintf(stderr,"Wrong server.  Try %s:%s:%s\n",
			    CourierErrArgs(WrongServerArgs,hint.object),
			    CourierErrArgs(WrongServerArgs,hint.domain),
			    CourierErrArgs(WrongServerArgs,hint.organization)
				);
			break;
		default:
			fprintf(stderr,"Some random error, code %d\n",
				Exception.Code);
			break;
		}
	exit(1);
	} END_HANDLER;

	printf("returned %s:%s:%s\n",
		result.distinguishedObject.object,
		result.distinguishedObject.domain,
		result.distinguishedObject.organization );
}
