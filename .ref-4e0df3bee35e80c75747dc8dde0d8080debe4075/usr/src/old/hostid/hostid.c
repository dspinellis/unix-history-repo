/*	hostid.c	4.1	82/11/07	*/

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
