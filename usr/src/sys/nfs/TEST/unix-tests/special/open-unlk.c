/*	@(#)open-unlk.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 *  tests operation on open file which has been unlinked.
 *  steps taken:
 *	1.  create file
 *	2.  open for read/write
 *	3.  unlink file
 *	4.  write data
 *	5.  rewind
 *	6.  read data back
 */

#include <stdio.h>
#ifdef SVR3
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#include <errno.h>
extern errno;
#define TBUFSIZ 100
char wbuf[TBUFSIZ], rbuf[TBUFSIZ];
#define TMSG "This is a test message written to the unlinked file\n"

main(argc, argv)
int argc;
char *argv[];
{
	int fd, ret;
	char *tname = "nfstestXXXXXX";
	int errcount = 0;
	long lret;
	extern long lseek();

	setbuf(stdout, NULL);
	mktemp(tname);
#ifdef O_RDWR
	if ((fd = open(tname, O_CREAT|O_TRUNC|O_RDWR, 0777)) < 0) {
		fprintf(stderr, "can't create %s: ", tname);
		xxit("open");
	}
#else
	if ((fd = creat(tname, 0777)) < 0) {
		fprintf(stderr, "can't create %s: ", tname);
		xxit("creat");
	}
	close(fd);
	if ((fd = open(tname, 2)) < 0) {
		fprintf(stderr, "can't reopen %s: ", tname);
		unlink(tname);
		xxit("open");
	}
#endif /* O_RDWR */
	printf("nfsjunk files before unlink:\n  ");
	system("ls -al .nfs*");
	ret = unlink(tname);
	printf("%s open; unlink ret = %d\n", tname, ret);
	if (ret)
		xxit(" unlink");
	printf("nfsjunk files after unlink:\n  ");
	system("ls -al .nfs*");
	strcpy(wbuf, TMSG);
	if ((ret = write(fd, wbuf, TBUFSIZ)) != TBUFSIZ) {
		fprintf(stderr, "write ret %d; expected %d\n", ret, TBUFSIZ);
		if (ret < 0)
			perror(" write");
		exit(1);
	}
	if ((lret = lseek(fd, 0L, 0)) != 0L) {
		fprintf(stderr, "lseek ret %ld; expected 0\n", lret);
		if (lret < 0)
			perror(" lseek");
		exit(1);
	}
	if ((ret = read(fd, rbuf, TBUFSIZ)) != TBUFSIZ) {
		fprintf(stderr, "read ret %d; expected %d\n", ret, TBUFSIZ);
		if (ret < 0)
			perror(" read");
		exit(1);
	}
	if (strcmp(wbuf, rbuf) != NULL) {
		errcount++;
		printf("read data not same as written data\n");
		printf(" written: '%s'\n read:    '%s'\n", wbuf, rbuf);
	} else {
		printf("data compare ok\n");
	}

	if (unlink(tname) == 0) {
		errcount++;
		printf("Error: second unlink succeeded!??\n");
	} else if (errno != ENOENT) {
		errcount++;
		perror("unexpected error on second unlink");
	}

	if (ret = close(fd)) {
		errcount++;
		perror("error on close");
	}

	printf("nfsjunk files after close:\n  ");
	system("ls -al .nfs*");

	if ((ret = close(fd)) == 0) {
		errcount++;
		fprintf(stderr, "second close didn't return error!??\n");
	}

	if (errcount == 0)
		printf("test completed successfully.\n");
	exit(errcount);
}

xxit(s)
char *s;
{
	perror(s);
	exit(1);
}
