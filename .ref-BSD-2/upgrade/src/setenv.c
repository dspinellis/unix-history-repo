#include <retrofit.h>
#include <stdio.h>

/*
 * Simulate version 7 setenv in version 6 using htmp.
 */
struct htmp {
	int	uid;
	char	home[28];
	int	ttytype;
} hentry;

main(argc, argv)
	char **argv;
{
	int t;

	argc--, argv++;
	if (argc != 2 || strcmp(argv[0], "TERM") && strcmp(argv[0], "HOME")) {
		fprintf(stderr, "Usage: setenv TERM type\nor:    setenv HOME dir\n");
		exit(1);
	}
	t = ttyn(2);
	if (t == 'x') {
		fprintf(stderr, "Unit 2 not teletype.\n");
		exit(1);
	}
	if (hget(t) < 0) {
		fprintf(stderr, "Can't access data base.\n");
		exit(1);
	}
	if (!strcmp(argv[0], "TERM")) {
		char buf[512];
		if (tgetent(buf, argv[1]) <= 0) {
			fprintf(stderr, "Unknown terminal type.\n");
			exit(1);
		}
		hsettype(buf[0] | (buf[1] << 8));
	} else
		hsethome(argv[1]);
	hsetuid(getuid());
	if (hput(t) < 0) {
		fprintf(stderr, "Can't update data base.\n");
		exit(1);
	}
}
