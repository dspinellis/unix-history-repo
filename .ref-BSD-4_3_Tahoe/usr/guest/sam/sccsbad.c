#ifndef lint
static char sccsid[] = "%W% (Lucasfilm) %G%";
#endif

/*
 * Verify source files have sccs keyword lines
 * in correct format for sccspatch.
 */

#include <stdio.h>
#include <strings.h>
#include <sys/param.h>

int	cflag;
int	hflag;

main(argc, argv)
	char *argv[];
{
	FILE *fd;

	argc--, argv++;
	if (argc > 0 && strcmp(*argv, "-c") == 0) {
		cflag++;
		argc--, argv++;
	}
	if (argc > 0 && strcmp(*argv, "-h") == 0) {
		hflag++;
		argc--, argv++;
	}
	while (argc > 0) {
		fd = fopen(*argv, "r");
		if (fd == NULL)
			perror(*argv);
		else
			verify(fd, *argv);
		argc--, argv++;
	}
	exit(0);
}

verify(fd, name)
	FILE *fd;
	char *name;
{
	char buf[BUFSIZ];
	register char *cp;

	if (fgets(buf, sizeof (buf), fd) == NULL) {
		fprintf(stderr, "%s: unexpected EOF\n");
		fclose(fd);
	}
	cp = buf;
	if (*cp == '/') {
		includeverify(fd, name, cp);
		return;
	}
	if (*cp == '#' || strncmp(cp, "static", 6) == 0) {
		ifdefverify(fd, name, cp);
		return;
	}
	fprintf(stderr, "%s: doesn't look like an sccs file\n", name);
	fclose(fd);
}

includeverify(fd, name, cp)
	FILE *fd;
	char *name, *cp;
{
	char module[MAXPATHLEN], temp[MAXPATHLEN];
	char buf[BUFSIZ];
	int rev, rlevel, month, day, year, n;
	FILE *ftemp;

	n = sscanf(cp + 2, " %s %d.%d %d/%d/%d",
	    module, &rev, &rlevel, &year, &month, &day);
	if (strncmp(cp + 3, "%M%", 3) == 0)
		goto bad;
	if (n != 6)
		fprintf(stderr, "%s: malformed sccs include-style line\n",
		    name);
	else if (cflag)
		printf("%s: has include style sccs keyword line\n", name);
bad:
	fclose(fd);
}

ifdefverify(fd, name, cp)
	FILE *fd;
	char *name, *cp;
{
	char buf[BUFSIZ], module[MAXPATHLEN], temp[MAXPATHLEN];
	register char *cp;
	int rev, rlevel, month, day, year, n;
	FILE *ftemp;
	int addendif = 0;

	if (strcmp(cp, "#ifndef lint\n") && strcmp(cp, "#ifndef\tlint\n")) {
		if (strncmp(cp, "static", 6) == 0) {
			strcpy(buf, cp);
			addendif++;
			goto trysccs;
		}
		fprintf(stderr, "%s: malformed sccs #ifdef-line\n", name);
		goto bad;
	}
	if (fgets(buf, sizeof (buf), fd) == NULL) {
		fprintf(stderr, "%s: unexpected EOF after #ifdef\n", name);
		goto bad;
	}
trysccs:
	cp = index(buf, '"');
	if (cp == 0) {
		fprintf(stderr, "%s: missing \" on keyword line\n", name);
		goto bad;
	}
	n = sscanf(cp + 1, "@(#)%s %d.%d (Berkeley) %d/%d/%d",
	    module, &rev, &rlevel, &month, &day, &year);
	if (n == 3)
		n = sscanf(cp + 1, "@(#)%s %d.%d %d/%d/%d",
		    module, &rev, &rlevel, &month, &day, &year);
	if (strncmp(cp + 1, "%W%", 3) == 0)
		goto bad;
	if (n != 6)
		fprintf(stderr, "%s: malformed sccs keyword line\n", name);
	else if (hflag)
		printf("%s: has source style sccs keyword line\n", name);
bad:
	fclose(fd);
}
