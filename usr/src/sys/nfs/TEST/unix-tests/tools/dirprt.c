/*	@(#)dirprt.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.2 Lachman ONC Test Suite source";
 */
#include <sys/param.h>
#ifndef major
#include <sys/types.h>
#endif
#ifdef SVR3
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <stdio.h>
#include <ctype.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	argv++;
	argc--;
	while (argc--) {
		print(*argv++);
	}
}

print(dir)
	char *dir;
{
	DIR *dirp;
#ifdef SVR3
	struct dirent *dp;
#else
	struct direct *dp;
#endif
	int rec = 0;

	dirp = opendir(dir);
	if (dirp == NULL) {
		perror(dir);
		return;
	}
	while ((dp = readdir(dirp)) != NULL) {
#ifdef SVR3
		printf("%5d %5ld %5d %s\n",
		    telldir(dirp),dp->d_ino, dp->d_reclen,
		    dp->d_name);
#else
		printf("%5d %5d %5d %5d %s\n",
		    telldir(dirp),dp->d_fileno, dp->d_reclen,
		    dp->d_namlen, dp->d_name);
#endif
	}
	closedir(dirp);
}

#include <sys/stat.h>

/*
 * open a directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;
	register int fd;
	struct stat sb;
	extern char *malloc();

	if ((fd = open(name, 0)) == -1) {
		printf("open failed\n");
		return (NULL);
	}
	if (fstat(fd, &sb) == -1) {
		printf("stat failed\n");
		return (NULL);
	}
	if ((sb.st_mode & S_IFMT) != S_IFDIR) {
		printf("not a directory\n");
		return (NULL);
	}
	printf("%s mode %o dir %o\n", name, sb.st_mode, S_IFDIR);
	if (((dirp = (DIR *)malloc(sizeof(DIR))) == NULL) ||
#ifdef SVR3
	    ((dirp->dd_buf = malloc(DIRBUF)) == NULL)) {
#else
	    ((dirp->dd_buf = malloc((int)sb.st_blksize)) == NULL)) {
#endif
		if (dirp) {
			if (dirp->dd_buf) {
				free(dirp->dd_buf);
			}
			free(dirp);
		}
		close(fd);
		return (NULL);
	}
#ifndef SVR3
	dirp->dd_bsize = sb.st_blksize;
	dirp->dd_bbase = 0;
	dirp->dd_entno = 0;
#endif
	dirp->dd_fd = fd;
	dirp->dd_loc = 0;
	return (dirp);
}
