/*
 * Program to convert history file to dbm file.  The old 3 field
 * history file is still kept there, because we need it for expire
 * and for a human readable copy.  But we keep a dbm hashed copy
 * around by message ID so we can answer the yes/no question "have
 * we already seen this message".  The content is the ftell offset
 * into the real history file when we get the article - you can't
 * really do much with this because the file gets compacted.
 */

#include <stdio.h>

typedef struct {
	char *dptr;
	int dsize;
} datum;

FILE *fd;

char namebuf[BUFSIZ];
char lb[BUFSIZ];

char *index();

main(argc, argv)
char **argv;
{
	register char *p, *q;
	long fpos;
	datum lhs, rhs;
	int rv;

	if (argc != 2 ) {
		fprintf(stderr, "Usage: cvt.hist /usr/lib/news\n");
		exit(1);
	}

	umask(0);
	sprintf(namebuf, "%s/history.dir", argv[1]);
	close(creat(namebuf, 0666));
	sprintf(namebuf, "%s/history.pag", argv[1]);
	close(creat(namebuf, 0666));
	sprintf(namebuf, "%s/history", argv[1]);

	fd = fopen(namebuf, "r");
	if (fd == NULL) {
		perror(namebuf);
		exit(2);
	}

	dbminit(namebuf);
	while (fpos=ftell(fd), fgets(lb, BUFSIZ, fd) != NULL) {
		p = index(lb, '\t');
		if (p)
			*p = 0;
		lhs.dptr = lb;
		lhs.dsize = strlen(lb) + 1;
		rhs.dptr = (char *) &fpos;
		rhs.dsize = sizeof fpos;
		rv = store(lhs, rhs);
		if (rv < 0)
			fprintf(stderr, "store(%s) failed\n", lb);
	}
	exit(0);
}
