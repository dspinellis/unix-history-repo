#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/except.h>

extern GetData();

main(argc, argv)
	int argc;
	char *argv[];
{
	ListAliasesOfResults result;
	struct ns_addr *destaddr;
	CourierConnection *conn;
	extern struct ns_addr *getXNSaddr();
	ObjectNamePattern name;
	ObjectName myname;
	extern char *getpass();
	Authenticator agent;

	if (argc != 2) {
		fprintf(stderr,"Usage: %s alias\n",argv[0]);
		exit(1);
	}
		/* default to CornellS1 2-273, 2-852-151-014 */
	destaddr = getXNSaddr("8E0#00.00.AA.00.7D.E7");
	if ((conn = CourierOpen(destaddr)) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",argv[1]);
		exit(1);
	}
	name.organization = "Berkeley.EECS";
	name.domain = "Evans";
	name.object = argv[1];
	/* use a null credentials&verifier */
	myname.organization = myname.domain = myname.object = "";
	MakeSimpleCredentialsAndVerifier(&myname, "",
		&agent.credentials, &agent.verifier );
	printf("Aliases:\n");

	DURING
		result = ListAliasesOf(conn, GetData, name,
			BulkData_immediateSink, agent);
	HANDLER {
		switch (Exception.Code) {
		case CallError:
			fprintf(stderr,"Call error, %d\n",
				CourierErrArgs(CallErrorArgs,problem));
			break;
		case ArgumentError:
			switch (CourierErrArgs(ArgumentErrorArgs,problem)) {
			case illegalOrganizationName:
			case illegalDomainName:
			case illegalObjectName:
				fprintf(stderr,
					"%s:%s:%s has bad format\n",
					name.object, name.domain, 
					name.organization);
				break;
			case noSuchOrganization:
				fprintf(stderr,
					"%s does not exist\n",
					name.organization);
				break;
			case noSuchDomain:
				fprintf(stderr,
					"%s:%s does not exist\n",
					name.domain,
					name.organization);
				break;
			case noSuchObject:
				fprintf(stderr,"No such object as %s:%s:%s\n",
					name.object, name.domain, 
					name.organization);
				break;
			default:
				fprintf(stderr,"Argument error (%d,%d)\n",
				    CourierErrArgs(ArgumentErrorArgs,problem),
				    CourierErrArgs(ArgumentErrorArgs,which) );
				break;
			}
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
		case PropertyError:
			fprintf(stderr,"Property error %d in %s:%s:%s\n",
			    CourierErrArgs(PropertyErrorArgs,problem),
			    CourierErrArgs(PropertyErrorArgs,distinguishedObject.object),
			    CourierErrArgs(PropertyErrorArgs,distinguishedObject.domain),
			    CourierErrArgs(PropertyErrorArgs,distinguishedObject.organization)
				);
			break;
		default:
			fprintf(stderr,"Some random error, code %d\n",
				Exception.Code);
			break;
		}
	exit(1);
	} END_HANDLER;

	printf("Distinguished name:\n\t%s:%s:%s\n",
		result.distinguishedObject.object,
		result.distinguishedObject.domain,
		result.distinguishedObject.organization );
}
