/*	cu.c	4.2	81/11/29	*/

#include "tip.h"

int	cleanup();
int	timeout();

/*
 * Botch the interface to look like cu's
 */
cumain(argc, argv)
	char *argv[];
{
	register int i;

	signal(SIGINT, cleanup);
	signal(SIGQUIT, cleanup);
	signal(SIGHUP, cleanup);
	signal(SIGTERM, cleanup);
	setbuf(stdout, NULL);
	loginit();
	setuid(getuid());
	setgid(getgid());
	vinit();
	boolean(value(VERBOSE)) = 0;
	if (argc < 2) {
		printf("usage: cu telno [-t] [-s speed] [-a acu] [-l line]\n");
		exit(8);
	}
	for (; argc > 1; argv++, argc--) {
		if (argv[1][0] != '-')
			PN = argv[1];
		else switch (argv[1][1]) {

		case 't':
			HW = 1, DU = -1;
			--argc;
			continue;

		case 'a':
			CU = argv[2]; ++argv; --argc;
			break;

		case 's':
			if (speed(atoi(argv[2])) == 0) {
				printf("cu: unsupported speed %s\n", argv[2]);
				exit(3);
			}
			BR = atoi(argv[2]); ++argv; --argc;
			break;

		case 'l':
			DV = argv[2]; ++argv; --argc;
			break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			CU[strlen(CU)-1] = argv[1][1];
			break;

		default:
			printf("Bad flag %s", argv[1]);
			break;
		}
	}
	/*
	 * The "cu" host name is used to define the
	 * attributes of the generic dialer.
	 */
	if ((i = hunt("cu")) == 0) {
		printf("all ports busy\n");
		exit(3);
	}
	if (i == -1) {
		printf("link down\n");
		delock(uucplock);
		exit(3);
	}
	if (HW)
		ttysetup(speed(BR));
	if (connect()) {
		printf("Connect failed\n");
		delock(uucplock);
		exit(1);
	}
	if (!HW)
		ttysetup(speed(BR));
}
