#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/mon.c,v 1.1 83/01/29 13:33:02 jkf Exp $";
#endif

/*					-[Sat Jan 29 13:29:17 1983 by jkf]-
 * 	mon.c				$Locker:  $
 * copy of the monitor subroutine
 *
 * (c) copyright 1982, Regents of the University of California
 */

/* 
 * monitor() library function
 * this version was taken from mcrt0.c  where it was put by
 * some misguided C chauvinists who felt that anyone using the monitor()
 * function would of course be running C and would use the C start-up
 * file.
 * Following unix standards for commenting, this file has no comments
 * other than this one, which tells you nothing about what the function
 * does or how it does it.  All those in favor of trashing unix and
 * starting over again, raise your hands.			-jkf
 */
 
struct phdr {
	int *lpc;
	int *hpc;
	int ncnt;
};

int numctrs;
struct cnt {
	int *pc;
	long ncall;
} *countbase;

monitor(lowpc, highpc, buf, bufsiz, cntsiz)
	char *lowpc, *highpc;
	char *buf;
	int bufsiz, cntsiz;
{
	register int o;
	struct phdr *php;
	static int ssiz;
	static char *sbuf;

	if (lowpc == 0) {
		profil(0, 0, 0, 0);
		o = creat("mon.out", 0666);
		write(o, sbuf, ssiz);
		close(o);
		return;
	}
	sbuf = buf;
	ssiz = bufsiz;
	php = (struct phdr *)&buf[0];
	php->lpc = (int *)lowpc;
	php->hpc = (int *)highpc;
	php->ncnt = cntsiz;
	numctrs = cntsiz;
	countbase = (struct cnt *)(buf + sizeof(struct phdr));
	o = sizeof(struct phdr) + cntsiz * sizeof(struct cnt);
	buf += o;
	bufsiz -= o;
	if (bufsiz <= 0)
		return;
	o = (highpc - lowpc);
	if(bufsiz < o)
		o = ((float) bufsiz / o) * 65536;
	else
		o = 65536;
	profil(buf, bufsiz, lowpc, o);
}
