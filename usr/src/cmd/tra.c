#include <sys/types.h>
#include <stat.h>

/*
 * transcribe [ - ] [ -delay ] file
 * Bill Joy UCB May 1977
 *
 * watch a file and copy it out
 * as it grows.  default interval for
 * copying is 15 seconds.
 * option - suppresses complete file from being copied out
 * and only gives new stuff.
 */
struct stat stbuf;
char buf[512];
int i;
off_t offset;
int interval 15;
int timeleft	32000;
char *progname;
int whole 1;

main(argc, argv)
	int argc;
	char *argv[];
{
	progname = *argv++;
	argc--;
nxtarg:
	if (argc > 1 && argv[0][0] == '-' && argv[0][1] == 0) {
		argc--;
		argv++;
		whole = 0;
		goto nxtarg;
	}
	if (argc > 1 && argv[0][0] == '+') {
		timeleft = getdel(argv[0] + 1);
		argc--;
		argv++;
		goto nxtarg;
	}
	if (argc > 1 && argv[0][0] == '-') {
		interval = getdel(argv[0] + 1);
		argv++;
		argc--;
		goto nxtarg;
	}
	if (argc != 1) {
		printf("Usage: %s [ - ] [ -interval ] file\n", progname);
		exit(1);
	}
	if (interval <= 0) {
		printf("Unreasonable interval\n");
		exit(1);
	}
	close(0);
	if (open(argv[0], 0) < 0) {
		perror(argv[0]);
		exit(1);
	}
	if (whole == 0) {
		fstat(0, &stbuf);
		offset = stbuf.st_size;
	}
	do {
		fstat(0, &stbuf);
		if (stbuf.st_size > offset) {
			lseek(0, (long) offset, 0);
			while ((i = read(0, buf, sizeof buf)) > 0) {
				offset =+ i;
				write(1, buf, i);
			}
		}
		sleep(interval);
		timeleft =- interval;
	} while (timeleft > 0);
}

getdel(cp)
	char *cp;
{
	register int j;

	j = 0;
	do {
		number(*cp);
		j = j * 10 + *cp++ - '0';
	} while (*cp);
	return (j);
}

number(c)
	char c;
{
	if (c < '0' || c > '9') {
		printf("Bad number for interval\n");
		exit(1);
	}
}
