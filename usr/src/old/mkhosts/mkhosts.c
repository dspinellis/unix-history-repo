#ifndef lint
static	char *sccsid = "@(#)mkhosts.c	4.3 (Berkeley) 84/05/17";
#endif

#include <sys/file.h>
#include <stdio.h>
#include <netdb.h>
#include <ndbm.h>

char	buf[BUFSIZ];

main(argc, argv)
	char *argv[];
{
	DBM *dp;
	register struct hostent *hp;
	datum key, content;
	register char *cp, *tp, **sp;
	register int naliases, *nap;
	int verbose = 0, entries = 0, maxlen = 0;

	if (argc > 1 && strcmp(argv[1], "-v") == 0) {
		verbose++;
		argv++, argc--;
	}
	if (argc != 2) {
		fprintf(stderr, "usage: mkhosts [ -v ] file\n");
		exit(1);
	}
	if (access(argv[1], R_OK) < 0) {
		perror(argv[1]);
		exit(1);
	}
	umask(0);
	dp = ndbmopen(argv[1], O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (dp == NULL) {
		fprintf(stderr, "dbminit failed: ");
		perror(argv[1]);
		exit(1);
	}
	dp->db_maxbno = 0;
	sethostfile(argv[1]);
	sethostent(1);
	while (hp = gethostent()) {
		cp = buf;
		tp = hp->h_name;
		while (*cp++ = *tp++)
			;
		nap = (int *)cp;
		cp += sizeof (int);
		naliases = 0;
		for (sp = hp->h_aliases; *sp; sp++) {
			tp = *sp;
			while (*cp++ = *tp++)
				;
			naliases++;
		}
		*nap = naliases;
		bcopy((char *)&hp->h_addrtype, cp, sizeof (int));
		cp += sizeof (int);
		bcopy((char *)&hp->h_length, cp, sizeof (int));
		cp += sizeof (int);
		bcopy(hp->h_addr, cp, hp->h_length);
		cp += hp->h_length;
		content.dptr = buf;
		content.dsize = cp - buf;
		if (verbose)
			printf("store %s, %d aliases\n", hp->h_name, naliases);
		key.dptr = hp->h_name;
		key.dsize = strlen(hp->h_name);
		dbmstore(dp, key, content, DB_INSERT);
		for (sp = hp->h_aliases; *sp; sp++) {
			key.dptr = *sp;
			key.dsize = strlen(*sp);
			dbmstore(dp, key, content, DB_INSERT);
		}
		key.dptr = hp->h_addr;
		key.dsize = hp->h_length;
		dbmstore(dp, key, content, DB_INSERT);
		entries++;
		if (cp - buf > maxlen)
			maxlen = cp - buf;
	}
	endhostent();
	printf("%d host entries, maximum length %d\n", entries, maxlen);
	exit(0);
}
