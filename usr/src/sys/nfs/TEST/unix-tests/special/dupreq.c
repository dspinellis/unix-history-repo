/*	@(#)dupreq.c	1.2 90/01/03 NFS Rev 2 Testsuite	*/
/*
 *  check for lost reply on non-idempotent resuests
 */
#include <stdio.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	int count, i;
	int fd;
	int cfail, lfail, u1fail, u2fail;
	char name1[256];
	char name2[256];

	if (argc != 3) {
		fprintf(stderr, "usage: %s count name\n", argv[0]);
		exit(1);
	}
	setbuf(stdout, NULL);
	count = atoi(argv[1]);
	sprintf(name1, "%s1", argv[2]);
	sprintf(name2, "%s2", argv[2]);
	cfail = lfail = u1fail = u2fail = 0;
	for (i=count; i > 0; i--) {
		if ((fd = creat(name1, 0666)) < 0) {
			cfail++;
			fprintf(stderr, "create ");
			perror(name1);
			continue;
		}
		close(fd);
		if (link(name1, name2) < 0) {
			lfail++;
			fprintf(stderr, "link %s %s", name1, name2);
			perror(" ");
		}
		if (unlink(name2) < 0) {
			u1fail++;
			fprintf(stderr, "unlink %s", name2);
			perror(" ");
		}
		if (unlink(name1) < 0) {
			u2fail++;
			fprintf(stderr, "unlink %s", name1);
			perror(" ");
		}
	}
	fprintf(stdout, "%d tries\n", count);
	if (cfail) {
		fprintf(stdout, "%d bad create\n", cfail);
	}
	if (lfail) {
		fprintf(stdout, "%d bad link\n", lfail);
	}
	if (u1fail) {
		fprintf(stdout, "%d bad unlink 1\n", u1fail);
	}
	if (u2fail) {
		fprintf(stdout, "%d bad unlink 2\n", u2fail);
	}
	exit(0);
}
