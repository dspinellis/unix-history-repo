#include <stdio.h>

/*
 * Diffdir - a (first) directory difference program
 * Bill Joy UCB March 16, 1978
 *
 * This is a difference program which operates on the entire contents of
 * a directory.  It reports common files which are different, running
 * diff if the files are ASCII files.  It also reports files which are
 * unique to one of the two directories.
 *
 * An option "h" (for header) causes diffdir to print each difference
 * on a new page (using an appropriate "pr") and to summarize missing
 * files and differences in binary files on a final page.
 *
 * Option "s" causes files which are the same to be reported also.
 *
 * It would be nice if this were "difftree", and if it knew a few
 * more things, e.g to "size" objects which are different or some
 * such or at least to not say that "directories are different" calling
 * them files, i.e. "Files ex-1.1/temp and ex-1.2/temp are different".
 */

typedef	char bool;

struct entry {
	char	*name;
	int	flags;
} *dir1, *dir2;

#define	ONLY	1
#define	DIFFER	2

bool	eflg;
bool	hflg;
bool	vflg;
bool	sflg;

#define	vprintf	if (vflg) printf
#define	diffopt()	(eflg ? "-e" : "")

main(argc, argv)
	int argc;
	char *argv[];
{
	register struct entry *d1, *d2;
	register char *cp;

	while (argc > 1) {
		cp = argv[1];
		if (*cp++ != '-')
			break;
		while (*cp) switch (*cp++) {

		case 'h':
			hflg++;
			continue;

		case 'v':
			vflg++;
			continue;

		case 's':
			sflg++;
			continue;

		case 'e':
			eflg++;
			printf("#\n");
			continue;

		default:
usage:
			panic("Usage: diffdir [ -h ] dir1 dir2");
		}
		argc--, argv++;
	}
	if (argc != 3)
		goto usage;
	setupdir(argv[1], &dir1);
	setupdir(argv[2], &dir2);
	d1 = dir1; d2 = dir2;
	while (d1->name != 0 || d2->name != 0) {
		if (useless(d1->name)) {
			d1++;
			continue;
		}
		if (useless(d2->name)) {
			d2++;
			continue;
		}
		if (d1->name != 0 && d2->name != 0)
			switch (sgn(strcmp(d1->name, d2->name))) {

			case -1:
onlyin1:
				if (hflg)
					d1->flags =| ONLY;
				else if (!eflg)
					printf("Only in %s: %s\n", argv[1], d1->name);
				d1++;
				continue;

			case 0:
				vprintf("In both: %s\n", d1->name);
				compare(d1, argv[1], argv[2]);
				d1++;
				d2++;
				continue;

			case 1:
onlyin2:			if (hflg)
					d2->flags =| ONLY;
				else if (!eflg)
					printf("Only in %s: %s\n", argv[2], d2->name);
				d2++;
				continue;
			}
		if (d1->name != 0)
			goto onlyin1;
		else
			goto onlyin2;
	}
	if (hflg) {
		int header = 0;

		for (d1 = dir1; d1->name; d1++)
			if (d1->flags & ONLY) {
				if (header == 0) {
					printf("\fOnly in %s\n", argv[1]);
					header = 1;
				}
				printf("\t%s\n", d1->name);
			}
		for (d2 = dir2; d2->name != 0; d2++) {
			if (d2->flags & ONLY) {
				if (header == 0)
					printf("\f");
				if ((header & 2) == 0) {
					printf("Only in %s\n", argv[2]);
					header =| 2;
				}
				printf("\t%s\n", d2->name);
			}
		}
		for (d1 = dir1; d1->name; d1++)
			if (d1->flags & DIFFER) {
				if (header == 0) {
					printf("\f");
					header = 1;
				}
				if ((header & 4) == 0) {
					printf("Non-ascii files which differ:\n");
					header =| 4;
				}
				printf("\t%s\n", d1->name);
			}
	}
	exit(0);
}

int	entcmp();
setupdir(cp, head)
	char *cp;
	struct entry **head;
{
	int count = 1;
	struct dirent0 {
		short ino;
		char fname[14];
	};

	struct dirent1 {
		short ino;
		char fname1[16];
	} dirent;
	register struct entry *hp;

	close(0);
	if (open(cp, 0) < 0) {
		perror(cp);
		exit(1);
	}
	while (read(0, &dirent, sizeof (struct dirent0)) == sizeof (struct dirent0))
	if (dirent.ino)
		count++;
	lseek(0, (long) 0, 0);
	hp = *head = Calloc(count, sizeof **head);
	while (read(0, &dirent, sizeof (struct dirent0)) == sizeof (struct dirent0))
	if (dirent.ino) {
		hp->name = savestr(dirent.fname);
		hp++;
	}
	qsort(*head, count - 1, sizeof **head, entcmp);
}

entcmp(ep, ep2)
	struct entry *ep, *ep2;
{

	return (strcmp(ep->name, ep2->name));
}

Calloc(i, n)
	int i, n;
{
	register unsigned mem;

	mem = calloc(i, n);
	if (mem == 0)
		panic("Ran out of memory");
	return (mem);
}

savestr(cp)
	register char *cp;
{

	return (strcpy(calloc(strlen(cp)+1, sizeof (char)), cp));
}

panic(cp)
	char *cp;
{

	fprintf(stderr, "%s\n", cp);
	exit(1);
}

sgn(i)
	int i;
{
	if (i > 0)
		return (1);
	else if (i < 0)
		return (-1);
	return (0);
}

compare(dp, d1, d2)
	struct entry *dp;
	char *d1, *d2;
{
	register int i;
	char path1[100], path2[100];
	int t1, t2;
	char header[250];
	char *name = dp->name;

	if (max(strlen(d1), strlen(d2)) + strlen(name) + 2 >= 100)
		panic("Path names too long");
	strcat(strcat(strcpy(path1, d1), "/"), name);
	strcat(strcat(strcpy(path2, d2), "/"), name);
	i = callit("/usr/bin/cmp", "cmp", "-s", path1, path2, 0);
	if (i == 0) {
		if (sflg)
			printf("Files %s and %s same\n", path1, path2);
		return;
	}
	if (!ascii(path1) || !ascii(path2)) {
		if (hflg)
			dp->flags =| DIFFER;
		else if (!eflg)
			printf("Files %s and %s differ\n", path1, path2);
		return;
	}
	if (hflg) {
		sprintf(header, "diff %s %s %s", diffopt(), path1, path2);
		prcallit(header, "/usr/bin/diff", "diff", path1, path2, 0);
	} else {
		if (eflg) {
			printf("ed - %s << '-*-END-*-'\n", name);
			callit("/usr/bin/diff", "diff", "-e", path1, path2, 0);
		} else {
			printf("diff %s %s\n", path1, path2);
			callit("/usr/bin/diff", "diff", path1, path2, 0);
		}
		if (eflg)
			printf("w\nq\n'-*-END-*-'\n");
	}
}

prcallit(header, path, av)
	char *header, *path;
{
	int status;
	int pid;
	int pv[2];

	fflush(stdout);
	pipe(pv);
	pid = fork();
	if (pid == -1)
		panic("No more processes");
	if (pid == 0) {
		close(0);
		dup(pv[0]);
		close(pv[0]);
		close(pv[1]);
		execl("/bin/pr", "pr", "-h", header, 0);
		execl("/usr/bin/pr", "pr", "-h", header, 0);
		perror("/usr/bin/pr");
		exit(1);
	}
	pid = fork();
	if (pid == -1)
		panic("No more processes");
	if (pid == 0) {
		close(1);
		dup(pv[1]);
		close(pv[0]);
		close(pv[1]);
		execv(path+4, &av);
		execv(path, &av);
		perror(path);
		exit(1);
	}
	close(pv[0]);
	close(pv[1]);
	while (wait(&status) != -1)
		continue;
}

callit(path, av)
	char *path;
{
	int status;
	int pid;

	fflush(stdout);
	pid = fork();
	if (pid == -1)
		panic("No more processes");
	if (pid == 0) {
		execv(path+4, &av);
		execv(path, &av);
		perror(path);
		exit(1);
	}
	wait(&status);
	return (((status >> 8) & 0377) | (status & 0377));
}

max(a,b)
	int a,b;
{

	return (a > b ? a : b);
}

ascii(cp)
	char *cp;
{
	int f;
	short w;

	f = open(cp, 0);
	if (f < 0)
		return (0);
	if (read(f, &w, sizeof w) != sizeof w) {
		close(f);
		return (1);
	}
	close(f);
	switch (w) {

	case 0405:	/* Overlay executable */
	case 0407:	/* Executable */
	case 0410:	/* Pure executable */
	case 0411:	/* Separate executable */
	case 0413:	/* Demand executable */
	case 0177545:	/* Archive */
	case 0177555:	/* Old archive */
		return (0);
	default:
		if (w & 0100200)
			return (0);
		return (1);
	}
}

useless(cp)
	char *cp;
{

	if (strcmp(cp, ".") == 0 || strcmp(cp, "..") == 0)
		return (1);
	if (cp[0] == '.')
		return (1);		/* For now */
	return (0);
}
