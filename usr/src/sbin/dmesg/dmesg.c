static char *sccsid = "@(#)dmesg.c	4.1 (Berkeley) 82/03/01";
/*
 *	Suck up system messages
 *	dmesg
 *		print current buffer
 *	dmesg -
 *		print and update incremental history
 */

#include <stdio.h>
#include <sys/param.h>
#include <nlist.h>
#include <signal.h>
#include <sys/vm.h>
#include <sys/msgbuf.h>

struct	msgbuf msgbuf;
char	*msgbufp;
int	sflg;
int	of	= -1;

struct	msgbuf omesg;
struct	nlist nl[2] = {
	{ "_msgbuf" },
	{ 0 }
};

main(argc, argv)
char **argv;
{
	int mem;
	register char *mp, *omp, *mstart;
	int timeout();
	int samef;

	signal(SIGALRM, timeout);
	alarm(30);
	if (argc>1 && argv[1][0] == '-') {
		sflg++;
		argc--;
		argv++;
	}
	if (sflg) {
		of = open("/usr/adm/msgbuf", 2);
		read(of, (char *)&omesg, sizeof(omesg));
		lseek(of, 0L, 0);
	}
	sflg = 0;
	nlist(argc>2? argv[2]:"/vmunix", nl);
	if (nl[0].n_type==0)
		done("No namelist\n");
	if ((mem = open((argc>1? argv[1]: "/dev/kmem"), 0)) < 0)
		done("No mem\n");
	lseek(mem, (long)nl[0].n_value, 0);
	read(mem, &msgbuf, sizeof (msgbuf));
	if (msgbuf.msg_magic != MSG_MAGIC)
		done("Magic number wrong (namelist mismatch?)\n");
	mstart = &msgbuf.msg_bufc[omesg.msg_bufx];
	omp = &omesg.msg_bufc[msgbuf.msg_bufx];
	mp = msgbufp = &msgbuf.msg_bufc[msgbuf.msg_bufx];
	samef = 1;
	do {
		if (*mp++ != *omp++) {
			mstart = msgbufp;
			samef = 0;
			pdate();
			printf("...\n");
			break;
		}
		if (mp == &msgbuf.msg_bufc[MSG_BSIZE])
			mp = msgbuf.msg_bufc;
		if (omp == &omesg.msg_bufc[MSG_BSIZE])
			omp = omesg.msg_bufc;
	} while (mp != mstart);
	if (samef && omesg.msg_bufx == msgbuf.msg_bufx)
		exit(0);
	mp = mstart;
	do {
		pdate();
		if (*mp && (*mp & 0200) == 0)
			putchar(*mp);
		mp++;
		if (mp == &msgbuf.msg_bufc[MSG_BSIZE])
			mp = msgbuf.msg_bufc;
	} while (mp != msgbufp);
	done((char *)NULL);
}

done(s)
char *s;
{
	register char *p, *q;

	if (s && s!=(char *)omesg.msg_magic && sflg==0) {
		pdate();
		printf(s);
	}
	write(of, (char *)&msgbuf, sizeof(msgbuf));
	exit(s!=NULL);
}

pdate()
{
	extern char *ctime();
	static firstime;
	time_t tbuf;

	if (firstime==0) {
		firstime++;
		time(&tbuf);
		printf("\n%.12s\n", ctime(&tbuf)+4);
	}
}

timeout()
{
	done("Buffer file screwed up\n");
}
