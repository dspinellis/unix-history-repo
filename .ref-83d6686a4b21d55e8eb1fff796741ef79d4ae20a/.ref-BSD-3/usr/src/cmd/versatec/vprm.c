#include <sys/types.h>
#include <dir.h>
#include <stat.h>
#include <stdio.h>

char	line[128];
int	linel;
int	all;
char	vpddir[] = "/usr/vpd";
FILE	*df;
FILE	*dfb;

main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	register char *ap, *cp;
	int cnt;

	if (chdir(vpddir) < 0)
		perror(vpddir), exit(1);
	df = fopen(".", "r");
	if (df == NULL)
		perror(vpddir), exit(1);
	argc--, argv++;
	if (argc == 0) {
		printf("usage: vprm [ id ... ] [ filename ... ] [ user ... ]\n");
		exit(1);
	}
	do {
		if (strcmp(*argv, "-a") == 0)
			all++;
		clobber(*argv++);
	} while (--argc);
}

clobber(cp)
	char *cp;
{
	struct dir dirent;
	int did = 0;

	rewind(df);
	while (fread(&dirent, sizeof dirent, 1, df) == 1) {
		if (dirent.d_ino == 0)
			continue;
		if (dirent.d_name[0] != 'd' || dirent.d_name[1] != 'f')
			continue;
		if (dirent.d_name[7] == 0 || dirent.d_name[8] != 0)
			continue;
		if (chkclob(cp, dirent.d_name)) {
			did++;
			printf("removing %s\n", dirent.d_name+3);
			unlink(dirent.d_name);
			dirent.d_name[0] = 'c'; unlink(dirent.d_name);
			dirent.d_name[2] = 'b'; unlink(dirent.d_name);
			dirent.d_name[2] = 'a';
			dirent.d_name[0] = 'l'; unlink(dirent.d_name);
			dirent.d_name[0] = 't'; unlink(dirent.d_name);
			dirent.d_name[0] = 'd';
		}
	}
	if (did == 0)
		fprintf(stderr, "%s: nothing to remove\n", cp);
}

chkclob(pattern, file)
	char *pattern, *file;
{
	register char *id = pattern;

	/*
	 * Quick check for matching id
	 */
	if (any(id[0], "cd") && id[1] == 'f' && id[2] == 'a')
		id += 3;
	if (strcmp(file+3, id) == 0)
		return (1);
	/*
	 * Now check for matching filename 'B', 'F' or id 'L'
	 */
	dfb = fopen(file, "r");
	if (dfb == NULL)
		return (0);
	while (getline()) switch (line[0]) {

	case 'L':
	case 'B':
	case 'F':
	case 'T':
		if (strcmp(line+1, pattern) == 0) {
			fclose(dfb);
			return (1);
		}
		continue;
	}
	fclose(dfb);
	return (0);
}

any(c, cp)
	char c;
	register char *cp;
{

	while (*cp)
		if (c == *cp++)
			return (1);
	return (0);
}

getline()
{
	register int i, c;

	i = 0;
	while ((c = getc(dfb)) != '\n') {
		if (c <= 0)
			return(0);
		if (i < 100)
			line[i++] = c;
	}
	line[i++] = 0;
	return (1);
}
