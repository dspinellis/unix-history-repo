static	char sccsid[] = "@(#)diffdir.c 4.4 1/25/81";

#include "diff.h"
/*
 * diff - directory comparison
 */
#define	d_flags	d_ino

#define	ONLY	1		/* Only in this directory */
#define	SAME	2		/* Both places and same */
#define	DIFFER	4		/* Both places and different */
#define	DIRECT	8		/* Directory */

struct	direct *setupdir();
int	header;
char	title[2*BUFSIZ], *etitle;

diffdir(argv)
	char **argv;
{
	register struct direct *d1, *d2;
	struct direct *dir1, *dir2;
	register int i;
	int cmp;

	if (opt == D_IFDEF) {
		fprintf(stderr, "diff: can't specify -I with directories\n");
		done();
	}
	if (opt == D_EDIT && (sflag || lflag))
		fprintf(stderr,
		    "diff: warning: shouldn't give -s or -l with -e\n");
	title[0] = 0;
	strcpy(title, "diff ");
	for (i = 1; diffargv[i+2]; i++) {
		if (!strcmp(diffargv[i], "-"))
			continue;	/* was -S, dont look silly */
		strcat(title, diffargv[i]);
		strcat(title, " ");
	}
	for (etitle = title; *etitle; etitle++)
		;
	setfile(&file1, &efile1, file1);
	setfile(&file2, &efile2, file2);
	argv[0] = file1;
	argv[1] = file2;
	dir1 = setupdir(file1);
	dir2 = setupdir(file2);
	d1 = dir1; d2 = dir2;
	while (d1->d_name[0] != 0 || d2->d_name[0] != 0) {
		if (d1->d_name[0] && useless(d1->d_name)) {
			d1++;
			continue;
		}
		if (d2->d_name[0] && useless(d2->d_name)) {
			d2++;
			continue;
		}
		if (d1->d_name[0] == 0)
			cmp = 1;
		else if (d2->d_name[0] == 0)
			cmp = -1;
		else
			cmp = strncmp(d1->d_name, d2->d_name, DIRSIZ);
		if (cmp < 0) {
			if (lflag)
				d1->d_flags |= ONLY;
			else if (opt == 0 || opt == 2) {
				only(d1, 1);
				printf(": %.*s\n", DIRSIZ, d1->d_name);
			}
			d1++;
		} else if (cmp == 0) {
			compare(d1);
			d1++;
			d2++;
		} else {
			if (lflag)
				d2->d_flags |= ONLY;
			else if (opt == 0 || opt == 2) {
				only(d2, 2);
				printf(": %.*s\n", DIRSIZ, d2->d_name);
			}
			d2++;
		}
	}
	if (lflag) {
		scanpr(dir1, ONLY, "Only in %.*s", file1, efile1);
		scanpr(dir2, ONLY, "Only in %.*s", file2, efile2);
		scanpr(dir1, SAME, "Common identical files", 0, 0);
		scanpr(dir1, DIFFER, "Binary files which differ", 0, 0);
		scanpr(dir1, DIRECT, "Common subdirectories", 0, 0);
	}
	if (rflag) {
		if (header && lflag)
			printf("\f");
		for (d1 = dir1; d1->d_name[0]; d1++)  {
			if ((d1->d_flags & DIRECT) == 0)
				continue;
			strncpy(efile1, d1->d_name, DIRSIZ);
			strncpy(efile2, d1->d_name, DIRSIZ);
/*
			if (opt != D_EDIT) {
				*etitle = 0;
				printf("%s%s %s\n", title, file1, file2);
			}
*/
			calldiff(0);
		}
	}
}

setfile(fpp, epp, file)
	char **fpp, **epp;
	char *file;
{
	register char *cp;

	*fpp = malloc(BUFSIZ);
	if (*fpp == 0) {
		fprintf(stderr, "diff: ran out of memory\n");
		exit(1);
	}
	strcpy(*fpp, file);
	for (cp = *fpp; *cp; cp++)
		continue;
	*cp++ = '/';
	*epp = cp;
}

scanpr(dp, test, title, file, efile)
	register struct direct *dp;
	int test;
	char *title, *file, *efile;
{
	int titled = 0;

	for (; dp->d_name[0]; dp++)
		if (dp->d_flags & test) {
			if (titled == 0) {
				if (header == 0) {
					if (anychange)
						printf("\f");
					header = 1;
				} else
					printf("\n");
				printf(title, efile - file - 1, file);
				printf(":\n");
				titled = 1;
			}
			ptname(dp);
		}
}

only(dp, which)
	struct direct *dp;
	int which;
{
	char *file = which == 1 ? file1 : file2;
	char *efile = which == 1 ? efile1 : efile2;

	printf("Only in %.*s", efile - file - 1, file, DIRSIZ, dp->d_name);
}

ptname(dp)
	struct direct *dp;
{

	printf("\t%.*s\n", DIRSIZ, dp->d_name);
}

int	entcmp();

struct direct *
setupdir(cp)
	char *cp;
{
	struct stat stb;
	register struct direct *dp, *ep;

	close(0);
	if (open(cp, 0) < 0) {
		fprintf(stderr, "diff: ");
		perror(cp);
		done();
	}
	fstat(0, &stb);
	dp = (struct direct *)malloc((unsigned) stb.st_size + sizeof (struct direct));
	if (dp == 0) {
		fprintf(stderr, "diff: ran out of memory\n");
		done();
	}
	if (read(0, (char *)dp, (int)stb.st_size) != (int)stb.st_size) {
		fprintf(stderr, "diff: ");
		perror(cp);
		done();
	}
	qsort(dp, (int) stb.st_size / sizeof (struct direct), 
	    sizeof (struct direct), entcmp);
	ep = &dp[stb.st_size / sizeof (struct direct)];
	ep->d_name[0] = 0;
	while (--ep >= dp && ep->d_ino == 0)
		ep->d_name[0] = 0;
	for (; ep >= dp; ep--)
		ep->d_flags = 0;
	return (dp);
}

entcmp(d1, d2)
	struct direct *d1, *d2;
{

	if (d1->d_ino == 0)
		return (1);
	if (d2->d_ino == 0)
		return (-1);
	return (strncmp(d1->d_name, d2->d_name, DIRSIZ));
}

compare(dp)
	register struct direct *dp;
{
	register int i, j;
	int f1, f2, fmt1, fmt2;
	struct stat stb1, stb2;
	int flag = 0;
	char buf1[BUFSIZ], buf2[BUFSIZ];

	strncpy(efile1, dp->d_name, DIRSIZ);
	strncpy(efile2, dp->d_name, DIRSIZ);
	f1 = open(file1, 0);
	if (f1 < 0) {
		perror(file1);
		return;
	}
	f2 = open(file2, 0);
	if (f2 < 0) {
		perror(file2);
		close(f1);
		return;
	}
	fstat(f1, &stb1); fstat(f2, &stb2);
	fmt1 = stb1.st_mode & S_IFMT;
	fmt2 = stb2.st_mode & S_IFMT;
	if (fmt1 != S_IFREG || fmt2 != S_IFREG) {
		if (fmt1 == fmt2) {
			if (fmt1 != S_IFDIR && stb1.st_rdev == stb2.st_rdev)
				goto same;
			if (fmt1 == S_IFDIR) {
				dp->d_flags = DIRECT;
				if (lflag || opt == D_EDIT)
					goto closem;
				printf("Common subdirectories: %s and %s\n",
				    file1, file2);
				goto closem;
			}
		}
		goto notsame;
	}
	if (stb1.st_size != stb2.st_size)
		goto notsame;
	for (;;) {
		i = read(f1, buf1, BUFSIZ);
		j = read(f2, buf2, BUFSIZ);
		if (i < 0 || j < 0 || i != j)
			goto notsame;
		if (i == 0 && j == 0)
			goto same;
		for (j = 0; j < i; j++)
			if (buf1[j] != buf2[j])
				goto notsame;
	}
same:
	if (sflag == 0)
		goto closem;
	if (lflag)
		dp->d_flags = SAME;
	else
		printf("Files %s and %s are identical\n", file1, file2);
	goto closem;
notsame:
	if (!ascii(f1) || !ascii(f2)) {
		if (lflag)
			dp->d_flags |= DIFFER;
		else if (opt == D_NORMAL || opt == D_CONTEXT)
			printf("Binary files %s and %s differ\n",
			    file1, file2);
		goto closem;
	}
	close(f1); close(f2);
	anychange = 1;
	if (lflag)
		calldiff(title);
	else {
		if (opt == D_EDIT) {
			printf("ed - %.*s << '-*-END-*-'\n",
			    DIRSIZ, dp->d_name);
			calldiff(0);
		} else {
			printf("%s%s %s\n", title, file1, file2);
			calldiff(0);
		}
		if (opt == D_EDIT)
			printf("w\nq\n-*-END-*-\n");
	}
	return;
closem:
	close(f1); close(f2);
}

char	*prargs[] = { "pr", "-h", 0, "-f", 0, 0 };

calldiff(wantpr)
	char *wantpr;
{
	int pid, status, status2, pv[2];

	prargs[2] = wantpr;
	fflush(stdout);
	if (wantpr) {
		sprintf(etitle, "%s %s", file1, file2);
		pipe(pv);
		pid = fork();
		if (pid == -1) {
			fprintf(stderr, "No more processes");
			done();
		}
		if (pid == 0) {
			close(0);
			dup(pv[0]);
			close(pv[0]);
			close(pv[1]);
			execv(pr+4, prargs);
			execv(pr, prargs);
			perror(pr);
			done();
		}
	}
	pid = fork();
	if (pid == -1) {
		fprintf(stderr, "diff: No more processes\n");
		done();
	}
	if (pid == 0) {
		if (wantpr) {
			close(1);
			dup(pv[1]);
			close(pv[0]);
			close(pv[1]);
		}
		execv(diff+4, diffargv);
		execv(diff, diffargv);
		perror(diff);
		done();
	}
	close(pv[0]);
	close(pv[1]);
	while (wait(&status) != pid)
		continue;
	while (wait(&status2) != -1)
		continue;
/*
	if ((status >> 8) >= 2)
		done();
*/
}

#include <a.out.h>

ascii(f)
	int f;
{
	char buf[BUFSIZ];
	register int cnt;
	register char *cp;

	lseek(f, (long)0, 0);
	cnt = read(f, buf, BUFSIZ);
	if (cnt >= sizeof (struct exec)) {
		struct exec hdr;
		hdr = *(struct exec *)buf;
		if (!N_BADMAG(hdr))
			return (0);
	}
	cp = buf;
	while (--cnt >= 0)
		if (*cp++ & 0200)
			return (0);
	return (1);
}

/*
 * THIS IS CRUDE.
 */
useless(cp)
register char *cp;
{

	if (cp[0] == '.')
		return (1);
	if (start && strcmp(start, cp) > 0)
		return (1);
	return (0);
}
