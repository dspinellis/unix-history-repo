#include "externs.h"
#include "defines.h"

/*
 * Initialize variables.
 */

void
tninit()
{
    init_terminal();

    init_network();
    
    init_telnet();
}


/*
 * main.  Parse arguments, invoke the protocol or command parser.
 */


void
main(argc, argv)
	int argc;
	char *argv[];
{
    tninit();		/* Clear out things */

    TerminalSaveState();

    prompt = argv[0];
    while ((argc > 1) && (argv[1][0] == '-')) {
	if (!strcmp(argv[1], "-d")) {
	    debug = 1;
	} else if (!strcmp(argv[1], "-n")) {
	    if ((argc > 1) && (argv[2][0] != '-')) {	/* get file name */
		NetTrace = fopen(argv[2], "w");
		argv++;
		argc--;
		if (NetTrace == NULL) {
		    NetTrace = stdout;
		}
	    }
	} else {
#if	defined(TN3270) && defined(unix)
	    if (!strcmp(argv[1], "-t")) {
		if ((argc > 1) && (argv[2][0] != '-')) { /* get file name */
		    transcom = tline;
		    (void) strcpy(transcom, argv[1]);
		    argv++;
		    argc--;
		}
	    } else if (!strcmp(argv[1], "-noasynch")) {
		noasynch = 1;
	    } else
#endif	/* defined(TN3270) && defined(unix) */
	    if (argv[1][1] != '\0') {
		fprintf(stderr, "Unknown option *%s*.\n", argv[1]);
	    }
	}
	argc--;
	argv++;
    }
    if (argc != 1) {
	if (setjmp(toplevel) != 0)
	    Exit(0);
	tn(argc, argv);
    }
    setjmp(toplevel);
    for (;;) {
#if	!defined(TN3270)
	command(1);
#else	/* !defined(TN3270) */
	if (!shell_active) {
	    command(1);
	} else {
#if	defined(TN3270)
	    shell_continue();
#endif	/* defined(TN3270) */
	}
#endif	/* !defined(TN3270) */
    }
}
