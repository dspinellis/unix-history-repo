/*	@(#)open-chmod.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 *  tests operation on open file which has been chmod'd to 0.
 *  steps taken:
 *	1.  create file
 *	2.  open for read/write
 *	3.  chmod 0
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
extern int errno;
#define TBUFSIZ 100
char wbuf[TBUFSIZ], rbuf[TBUFSIZ];
char buf[BUFSIZ];
#define TMSG "This is a test message written to the chmod'd file\n"

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
	printf("testfile before chmod:\n  ");
	sprintf(buf, "ls -l %s", tname);
	system(buf);
	ret = chmod(tname, 0);
	printf("%s open; chmod ret = %d\n", tname, ret);
	if (ret)
		xxit(" chmod");
	printf("testfile after chmod:\n  ");
	system(buf);
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

	printf("testfile after write/read:\n  ");
	system(buf);
	if (unlink(tname) < 0) {
		fprintf(stderr, "can't unlink %s", tname);
		xxit(" ");
	}

	if (close(fd))
		xxit("error on close");

	printf("test completed successfully.\n");
	exit(0);
}

xxit(s)
char *s;
{
	perror(s);
	exit(1);
}
