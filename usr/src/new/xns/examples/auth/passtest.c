#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "BasicAuthentication2_defs.h"
#include <xnscourier/except.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	CheckSimpleCredentialsResults result;
	struct xn_addr *destaddr;
	CourierConnection *conn;
	extern struct xn_addr *getXNSaddr();
	Credentials credentials;
	Verifier verifier;
	Clearinghouse_Name name;
	char *pwd;
	char *xnshost;
	extern char *getpass();

	/* defaults */
		/* default to CornellS1 2-273, 2-852-151-014 */
	xnshost = "8E1#00.00.AA.00.5E.E6";
		/* default to our organization/domain */
	name.organization = "Cornell-Univ";
	name.domain = "Computer Science";
		/* default to me */
	name.object = "JQ Johnson";

	switch (argc) {
	case 3: xnshost = argv[2];
	case 2:	name.object = argv[1];
	case 1:	if ((destaddr = getXNSaddr(xnshost)) == NULL)
			fprintf(stderr,"Invalid machine name.\n");
		else
			break;		/* got a valid host name */
	default:
		fprintf(stderr,"Usage: %s user [machine]\n",argv[0]);
		exit(1);
	}
	fprintf(stderr,"User: %s:%s:%s\n",
		name.object, name.domain, name.organization);
	pwd = getpass("Password: ");

	if ((conn = CourierOpen(destaddr)) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",xnshost);
		exit(1);
	}

	MakeSimpleCredentialsAndVerifier(&name,pwd,&credentials,&verifier);

	DURING
		result = CheckSimpleCredentials(conn,NULL,credentials,verifier);
	HANDLER {
		switch (Exception.Code) {
		case CallError:
			fprintf(stderr,"Call error, number %d, argument %d\n",
				CourierErrArgs(CallErrorArgs,problem),
				CourierErrArgs(CallErrorArgs,whichArg));
			break;
		case AuthenticationError:
			fprintf(stderr,"Auth error, problem %d\n",
				CourierErrArgs(AuthenticationErrorArgs,problem) );
			break;
		case REJECT_ERROR:
			fprintf(stderr,"REJECT:  type = %d\n",
				CourierErrArgs(rejectionDetails, designator));
			break;
		default:
			fprintf(stderr,"Some random error, code %d\n",
				Exception.Code);
			break;
		}
	exit(1);
	} END_HANDLER;

	/* CourierClose(conn); */
	printf("Returned %d\n", result.ok);
}
