/*
 *	Suck up system messages
 *	dmesg
 *		print current buffer
 *	dmesg -
 *		print and update incremental history
 */

#include <stdio.h>
#include <sys/param.h>
#include <a.out.h>
#include <signal.h>

char	msgbuf[MSGBUFS];
char	*msgbufp;
int	sflg;
int	of	= -1;

struct {
	char	*omsgflg;
	int	omindex;
	char	omsgbuf[MSGBUFS];
} omesg;
struct nlist nl[3] = {
	{"_msgbuf"},
	{"_msgbufp"}
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
	if (sflg)
		of = open("/usr/adm/msgbuf", 2);
	read(of, (char *)&omesg, sizeof(omesg));
	lseek(of, 0L, 0);
	sflg = 0;
	nlist(argc>2? argv[2]:"/vmunix", nl);
	if (nl[0].n_type==0)
		done("No namelist\n");
	if ((mem = open((argc>1? argv[1]: "/dev/kmem"), 0)) < 0)
		done("No mem\n");
	lseek(mem, (long)nl[0].n_value, 0);
	read(mem, msgbuf, MSGBUFS);
	lseek(mem, (long)nl[1].n_value, 0);
	read(mem, (char *)&msgbufp, sizeof(msgbufp));
	if (msgbufp < (char *)nl[0].n_value || msgbufp >= (char *)nl[0].n_value+MSGBUFS)
		done("Namelist mismatch\n");
	msgbufp += msgbuf - (char *)nl[0].n_value;
	mstart = &msgbuf[omesg.omindex];
	omp = &omesg.omsgbuf[msgbufp-msgbuf];
	mp = msgbufp;
	samef = 1;
	do {
		if (*mp++ != *omp++) {
			mstart = msgbufp;
			samef = 0;
			pdate();
			printf("...\n");
			break;
		}
		if (mp == &msgbuf[MSGBUFS])
			mp = msgbuf;
		if (omp == &omesg.omsgbuf[MSGBUFS])
			omp = omesg.omsgbuf;
	} while (mp != mstart);
	if (samef && mstart == msgbufp)
		exit(0);
	mp = mstart;
	do {
		pdate();
		if (*mp)
			putchar(*mp);
		mp++;
		if (mp == &msgbuf[MSGBUFS])
			mp = msgbuf;
	} while (mp != msgbufp);
	done((char *)NULL);
}

done(s)
char *s;
{
	register char *p, *q;

	if (s && s!=omesg.omsgflg && sflg==0) {
		pdate();
		printf(s);
	}
	omesg.omsgflg = s;
	q = omesg.omsgbuf;
	for (p = msgbuf; p < &msgbuf[MSGBUFS]; )
		*q++ = *p++;
	omesg.omindex = msgbufp - msgbuf;
	write(of, (char *)&omesg, sizeof(omesg));
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
