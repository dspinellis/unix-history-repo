#ifndef lint
static char *rcsid = "$Header: xnscreds.c,v 1.2 87/07/28 08:41:13 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* 
 * $Log:	xnscreds.c,v $
 * Revision 1.2  87/07/28  08:41:13  ed
 * fprintf to stderr not stdin (how did it work?).
 * 
 * Revision 1.1  87/03/17  16:28:14  ed
 * Initial revision
 * 
 * 
 */

#include <stdio.h>
#include <xnscourier/Authentication2.h>

int authenticate= 0;
int verbose= 0;

main(argc, argv)
int argc;
char *argv[];
{
	char name[128];
	char *pwd, *cp;
	char *getXNSpass(), *getenv(), *index();
	Cardinal passwd, buffer;
	int csh, i;
	FILE *tty, *fopen();
	Authentication2_Credentials credentials;
	Authentication2_Verifier verifier;
	Authentication2_ThreePartName defaultobjname, username;
	Authentication2_ThreePartName CH_StringToName();

	static char *options= "av";
	static char *usage= "Usage: %s [-a] [-v]\n";

	extern int optind;
	extern char *optarg;
	int opt;

	if ( !isatty(fileno(stdin)) ) {
		fprintf(stderr, "\nMust be run from terminal\n");
		exit(1);
	}

	if ( argc > 3 ) {
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	while ( (opt= getopt(argc, argv, options)) != EOF ) {
		switch (opt) {
			case 'a' :
				authenticate++;
				break;
			case 'v' :
				verbose++;
				break;

			default :
				fprintf(stderr, "Invalid command option -%c\n", opt);
				exit(1);
		}
	}

	if ( (tty= fopen("/dev/tty", "r+")) == NULL ) {
		tty= stdin;
	} else {
		setbuf(tty, (char *)NULL);
	}
	fprintf(stderr, "Enter XNS name: ");
	fgets(name, sizeof(name), tty);
	if ( (cp= index(name, '\n')) )
		*cp= '\0';

	pwd= getXNSpass("Enter XNS password: ");
	fclose(tty);
	
	CH_NameDefault(&defaultobjname);
	username= CH_StringToName(name, &defaultobjname);
	MakeSimpleCredsAndVerifier(&username, pwd, &credentials, &verifier, 0);

	if ( authenticate ) {
		if ( verbose ) {
			fprintf(stderr, "Validating XNS credentials\n");
		}

		if ( !Auth_CredCheck(credentials, verifier) ) {
			fprintf(stderr, "Invalid XNS credentials\n");
			exit(1);
		}
	}

	passwd= verifier.sequence[0];

	csh= 0;

	if ( (cp= getenv("SHELL")) && (i= strlen(cp)) >= 3 ) {
		csh= (strcmp(&cp[i-3], "csh") ? 0 : 1);
	}

	if ( !csh )
		fprintf(stdout, "export XNSNAME XNSPASSWD\n");
	else
		fprintf(stdout, "set noglob;\n");

	if ( csh ) {
	    	fprintf(stdout, "setenv XNSNAME %s ;\n", name);
		fprintf(stdout, "setenv XNSPASSWD %d ;\n", passwd);
		fprintf(stdout, "unset noglob;\n");
	} else {
		fprintf(stdout, "XNSNAME=%s ;\n", name);
		fprintf(stdout, "XNSPASSWD=%d ;\n", passwd);
	}

}

