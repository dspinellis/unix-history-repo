static	char *sccsid = "@(#)mt.c	4.1 (Berkeley) 81/05/11";

/*
 * mt
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>

struct commands {
	char *c_name;
	int c_code;
} com[] = {
	"eof",	MTWEOF,
	"fsf",	MTFSF,
	"bsf",	MTBSF,
	"fsr",	MTFSR,
	"bsr",	MTBSR,
	"rewind",	MTREW,
	"offline",	MTOFFL,
	0,0
};

int mtfd;
struct mtop mt_com;
char *tape;

main(argc, argv)
char **argv;
{
	char line[80], *getenv();
	register char *cp;
	register struct commands *comp;

	if (argc < 2) {
		fprintf(stderr, "usage: mt [ -t tape ] command [ count ]\n");
		exit(1);
	}
	if ((strcmp(argv[1], "-t") == 0) && argc > 2) {
		argc -= 2;
		tape = argv[2];
		argv += 2;
	} else
		if ((tape = getenv("TAPE")) == NULL)
			tape = "/dev/rmt12";
	if ((mtfd = open(tape, 2)) < 0) {
		if ((mtfd = open(tape, 0)) < 0) {
			perror(tape);
			exit(1);
		}
	}

	cp = argv[1];
	for (comp = com; comp->c_name != NULL; comp++)
		if (strncmp(cp, comp->c_name, strlen(cp)) == 0)
			break;
	if (comp->c_name == NULL) {
		fprintf(stderr, "mt: don't grok \"%s\"\n", cp);
		exit(1);
	}
	mt_com.mt_count = (argc > 2 ? atoi(argv[2]) : 1);
	mt_com.mt_op = comp->c_code;
	if (ioctl(mtfd, MTIOCTOP, &mt_com) < 0) {
		fprintf(stderr, "%s %d ", comp->c_name, mt_com.mt_count);
		perror("failed");
	}
}
