monitor(lowpc, highpc, buf, bufsiz, cntsiz)
char *lowpc, *highpc;
int *buf, bufsiz;
{
	register o;
	static *sbuf, ssiz;
	struct phdr {
		int *lpc;
		int *hpc;
		int ncnt;
	};
	struct cnt {
		int *pc;
		long ncall;
	};

	if (lowpc == 0) {
		profil(0, 0, 0, 0);
		o = creat("mon.out", 0666);
		write(o, sbuf, ssiz);
		close(o);
		return;
	}
	sbuf = buf;
	ssiz = bufsiz;
	buf[0] = (int)lowpc;
	buf[1] = (int)highpc;
	buf[2] = cntsiz;
	o = sizeof(struct phdr) + cntsiz*sizeof(struct cnt);
	buf = (int *) (((int)buf) + o);
	bufsiz -= o;
	if (bufsiz<=0)
		return;
	o = ((highpc - lowpc)>>1);
	if(bufsiz < o)
		o = ((float) bufsiz / o) * 32768;
	else
		o = 0177777;
	profil(buf, bufsiz, lowpc, o);
}
