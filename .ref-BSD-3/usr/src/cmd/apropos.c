#include <stdio.h>
#include <ctype.h>

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	if (argc == 0) {
usage:
		fprintf(stderr, "apropos word ...\n");
		exit(1);
	}
	if (freopen("/usr/lib/whatis", "r", stdin) == NULL) {
		perror("/usr/lib/whatis");
		exit (1);
	}
	argv[argc] = 0;
	apropos(argv);
	exit(0);
}

apropos(argv)
	char **argv;
{
	char buf[BUFSIZ];
	char *gotit;
	register char **vp;

	gotit = (char *) calloc(1, blklen(argv));
	while (fgets(buf, sizeof buf, stdin) != NULL)
		for (vp = argv; *vp; vp++)
			if (match(buf, *vp)) {
				printf("%s", buf);
				gotit[vp - argv] = 1;
				for (vp++; *vp; vp++)
					if (match(buf, *vp))
						gotit[vp - argv] = 1;
				break;
			}
	for (vp = argv; *vp; vp++)
		if (gotit[vp - argv] == 0)
			printf("%s: nothing apropriate\n", *vp);
}

match(buf, str)
	char *buf, *str;
{
	register char *bp, *cp;

	bp = buf;
	for (;;) {
		if (*bp == 0)
			return (0);
		if (amatch(bp, str))
			return (1);
		bp++;
	}
}

amatch(cp, dp)
	register char *cp, *dp;
{

	while (*cp && *dp && lmatch(*cp, *dp))
		cp++, dp++;
	if (*dp == 0)
		return (1);
	return (0);
}

lmatch(c, d)
	char c, d;
{

	if (c == d)
		return (1);
	if (!isalpha(c) || !isalpha(d))
		return (0);
	if (islower(c))
		c = toupper(c);
	if (islower(d))
		d = toupper(d);
	return (c == d);
}

blklen(ip)
	register int *ip;
{
	register int i = 0;

	while (*ip++)
		i++;
	return (i);
}
