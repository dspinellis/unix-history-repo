/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <stdio.h>

/*
 * Checksum the indicated directory, creating the file "check.sum"
 * with names and int sums stored inside, one per line.  The -c
 * option causes the directory checksums to be verified.
 */

#define	equal(a, b)	(strcmp(a, b) == 0)

int	cflag = 1;			/* Flag to verify */
char	cname[]	= "check.sum";		/* Name of checksum files */
int	errs;				/* Error count */

main(argc, argv)
	char **argv;
{
	register char *cp;

	if (argc == 2 && equal(argv[1], "-c")) {
		cflag = 0;
		argc--, argv++;
	}
	if (argc < 2) {
		checkout(".");
		exit(errs);
	}
	while (--argc) {
		cp = *++argv;
		if (equal(cp, "-c")) {
			cflag++;
			continue;
		}
		checkout(cp);
	}
	exit(errs);
}

/*
 * Checkout a directory.
 * The fork is necessary to preserve the current directory.
 */

checkout(dir)
	char dir[];
{
	int s, pid;
	char ename[DIRSIZ+1], linebuf[BUFSIZ];
	FILE *cf, *df, *ef;
	register int sum, c;
	struct direct dirent;
	register char *cp, *cp2;

	pid = fork();
	if (pid == -1) {
		perror("fork");
		errs++;
		return;
	}
	if (pid > 0) {
		while (wait(&s) != pid)
			;
		if (s != 0)
			errs++;
		return;
	}
	errs = 0;
	fprintf(stderr, "%s:\n", dir);
	if (chdir(dir) < 0) {
		perror(dir);
		exit(1);
	}
	if (cflag) {
		if ((cf = fopen(cname, "r")) == NULL) {
			perror(cname);
			exit(1);
		}
		while (fgets(linebuf, BUFSIZ, cf) != NULL) {
			for (cp = linebuf, cp2 = ename; *cp != ' ';
			    *cp2++ = *cp++)
				;
			*cp2 = '\0';
			if (equal(ename, cname))
				continue;
			if ((ef = fopen(ename, "r")) == NULL) {
				perror(ename);
				errs++;
				continue;
			}
			c = cksum(ef);
			fclose(ef);
			sum = atoi(cp);
			if (sum != c) {
				printf("Checksum error: \"%s\" is %d not %d\n",
					ename, c, sum);
				errs++;
			}
		}
		exit(errs);
	}
	if ((cf = fopen(cname, "w")) == NULL) {
		perror(cname);
		exit(1);
	}
	if ((df = fopen("", "r")) == NULL) {
		perror(dir);
		exit(1);
	}
	while (fread((char *) &dirent, sizeof dirent, 1, df) == 1) {
		struct stat stb;
		if (dirent.d_ino == 0)
			continue;
		for (cp = dirent.d_name, cp2 = ename; *cp &&
		    cp-dirent.d_name < DIRSIZ; *cp2++ = *cp++)
			;
		*cp2 = '\0';
		if (equal(ename, cname))
			continue;
		if ((ef = fopen(ename, "r")) == NULL) {
			perror(ename);
			errs++;
			continue;
		}
		fstat(fileno(ef), &stb);
		if ((stb.st_mode & S_IFMT) != S_IFREG) {
			fclose(ef);
			continue;
		}
		sum = cksum(ef);
		fclose(ef);
		fprintf(cf, "%s %d\n", ename, sum);
	}
	exit(errs);
}

/*
 * Checksum the passed file.  Return the sum of all of its bytes.
 */

cksum(f)
	FILE *f;
{
	register int sum, c;

	sum = 0;
	while ((c = getc(f)) != EOF)
		sum += c;
	if (sum < 0)
		sum = -sum;
	if (sum < 0)
		sum = 0;
	return(sum);
}

/*
 * Convert the passed string to decimal.
 */

atoi(cp)
	register char *cp;
{
	register int sum, sign;

	while (any(*cp, " \t"))
		cp++;
	if (*cp == '-')
		sign = -1;
	else
		sign = 1;
	sum = 0;
	while (any(*cp, "0123456789"))
		sum = sum*10 + *cp++ - '0';
	return(sign*sum);
}

/*
 * Is c any of *cp ?
 */

any(c, cp)
	register int c;
	register char *cp;
{
	while (*cp)
		if (c == *cp++)
			return(1);
	return(0);
}
