#ifndef lint
static char sccsid[] = "%W% (Lucasfilm) %G%";
#endif

/*
 * Massage source files with embedded sccs keywords into a
 * form suitable for creating new sccs s. files.
 */

#define	SITE	"Lucasfilm"

#include <stdio.h>
#include <strings.h>
#include <sys/param.h>

main(argc, argv)
	char *argv[];
{
	FILE *fd;

	argc--, argv++;
	while (argc > 0) {
		fd = fopen(*argv, "r");
		if (fd == NULL)
			perror(*argv);
		else
			patch(fd, *argv);
		argc--, argv++;
	}
	exit(0);
}

patch(fd, name)
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
		includepatch(fd, name, cp);
		return;
	}
	if (*cp == '#' || strncmp(cp, "static", 6) == 0) {
		ifdefpatch(fd, name, cp);
		return;
	}
	fprintf(stderr, "%s: doesn't look like an sccs file\n", name);
	fclose(fd);
}

includepatch(fd, name, cp)
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
	if (n != 6) {
		fprintf(stderr, "%s: malformed sccs include-style line\n",
		    name);
		goto bad;
	}
	sprintf(temp, "%s.patch", name);
	ftemp = fopen(temp, "w");
	if (ftemp == NULL) {
		fprintf(stderr, "%s: can't open temp file\n", temp);
		goto bad;
	}
	fprintf(ftemp, "/*\t%%M%%\t%%I%%\t%%E%%\t*/\n");
	fprintf(ftemp, "%s", cp);
	while (fgets(buf, sizeof (buf), fd))
		fputs(buf, ftemp);
	fclose(ftemp); fclose(fd);
	if (rename(temp, name) < 0)
		perror(name);
	return;
bad:
	fclose(fd);
}

ifdefpatch(fd, name, cp)
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
	if (n != 6) {
		fprintf(stderr, "%s: malformed sccs keyword line\n", name);
		goto bad;
	}
	sprintf(temp, "%s.patch", name);
	ftemp = fopen(temp, "w");
	if (ftemp == NULL) {
		fprintf(stderr, "%s: can't open temp file\n", temp);
		goto bad;
	}
	fprintf(ftemp, "#ifndef lint\n");
	fprintf(ftemp, "static char sccsid[] = ");
	fprintf(ftemp, "\"%%W%% (%s from %d.%d Berkeley) %%G%%\";\n",
	    SITE, rev, rlevel);
	if (addendif)
		fprintf(ftemp, "#endif\n");
	while (fgets(buf, sizeof (buf), fd))
		fputs(buf, ftemp);
	fclose(ftemp); fclose(fd);
	if (rename(temp, name) < 0)
		perror(name);
	return;
bad:
	fclose(fd);
}
