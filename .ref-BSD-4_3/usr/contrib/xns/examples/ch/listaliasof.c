#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include "Clearinghouse2_defs.h"
#include <xnscourier/except.h>


#define MAXPACKS 5

static
ProcessObjectName(obj)
	ObjectName obj;
{
	printf("\t%s:%s:%s\n", obj.object, obj.domain, obj.organization);
}

static
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

main(argc, argv)
	int argc;
	char *argv[];
{
	ListAliasesOfResults result;
	CourierConnection *conn;
	extern CourierConnection *CH_GetFirstCH();
	extern ObjectName CH_StringToName();
	ObjectNamePattern name;
	extern char *getpass();
	Authenticator agent;
	static ObjectName defaults = {"Cornell-Univ","Computer Science",""};

	if (argc != 2) {
		fprintf(stderr,"Usage: %s alias\n",argv[0]);
		exit(1);
	}
	if ((conn = CH_GetFirstCH()) == NULL) {
		fprintf(stderr,"Can't open connection to %s\n",argv[1]);
		exit(1);
	}
	name.object = argv[1];
	name = CH_StringToName(argv[1],&defaults);
	/* use a null credentials&verifier */
	MakeSimpleCredsAndVerifier(&defaults, "",
		&agent.credentials, &agent.verifier );
	printf("Aliases:\n");

	DURING
		result = ListAliasesOf(conn, GetData, name,
			BulkData1_immediateSink, agent);
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
