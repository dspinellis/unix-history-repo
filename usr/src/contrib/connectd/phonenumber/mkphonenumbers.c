#include <sys/file.h>
#include <stdio.h>
#include "../phonenumbers.h"
#include <ndbm.h>

char	buf[BUFSIZ];

main(argc, argv)
	char *argv[];
{
	DBM *dp;
	register struct phonenumberent *pn;
	datum key, content;
	register char *cp, *tp, **sp;
	register int *nap;
	int naliases;
	int verbose = 0, entries = 0, maxlen = 0, error = 0;
	char tempname[BUFSIZ], newname[BUFSIZ];

	if (argc > 1 && strcmp(argv[1], "-v") == 0) {
		verbose++;
		argv++, argc--;
	}
	if (argc != 2) {
		fprintf(stderr, "usage: mkphonenumbers [ -v ] file\n");
		exit(1);
	}
	if (access(argv[1], R_OK) < 0) {
		perror(argv[1]);
		exit(1);
	}
	umask(0);

	sprintf(tempname, "%s.new", argv[1]);
	dp = dbm_open(tempname, O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (dp == NULL) {
		fprintf(stderr, "dbm_open failed: ");
		perror(argv[1]);
		exit(1);
	}
	setphonenumberfile(argv[1]);
	setphonenumberent(1);
	while (pn = getphonenumberent()) {
		cp = buf;
		tp = pn->pn_name;
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
		bcopy((char *)&naliases, (char *)nap, sizeof(int));
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
		if (dbm_store(dp, key, content, DBM_INSERT) < 0) {
			perror(hp->h_name);
			goto err;
		}
		for (sp = hp->h_aliases; *sp; sp++) {
			key.dptr = *sp;
			key.dsize = strlen(*sp);
			if (dbm_store(dp, key, content, DBM_INSERT) < 0) {
				perror(*sp);
				goto err;
			}
		}
		key.dptr = hp->h_addr;
		key.dsize = hp->h_length;
		if (dbm_store(dp, key, content, DBM_INSERT) < 0) {
			perror("dbm_store phonenumber address");
			goto err;
		}
		entries++;
		if (cp - buf > maxlen)
			maxlen = cp - buf;
	}
	endphonenumberent();
	dbm_close(dp);

	sprintf(tempname, "%s.new.pag", argv[1]);
	sprintf(newname, "%s.pag", argv[1]);
	if (rename(tempname, newname) < 0) {
		perror("rename .pag");
		exit(1);
	}
	sprintf(tempname, "%s.new.dir", argv[1]);
	sprintf(newname, "%s.dir", argv[1]);
	if (rename(tempname, newname) < 0) {
		perror("rename .dir");
		exit(1);
	}
	printf("%d phonenumber entries, maximum length %d\n", entries, maxlen);
	exit(0);
err:
	sprintf(tempname, "%s.new.pag", argv[1]);
	unlink(tempname);
	sprintf(tempname, "%s.new.dir", argv[1]);
	unlink(tempname);
	exit(1);
}
