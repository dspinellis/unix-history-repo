#ifndef lint
static char sccsid[] = "@(#)hostid.c	4.2 (Berkeley) %G%";
#endif

main(argc, argv)
	int argc;
	char **argv;
{

	if (argc > 1) {
		int hostid;
		sscanf(argv[1], "%x", &hostid);
		if (sethostid(hostid) < 0)
			perror("hostid");
	} else
		printf("%x\n", gethostid());
}
