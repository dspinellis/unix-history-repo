monitor(lowpc, highpc, buf, bufsiz, cntsiz)
char *lowpc, *highpc;
int *buf, bufsiz;
{
	register char *o;
	static *sbuf, ssiz;

	if (lowpc == 0) {
		profil(0, 0, 0, 0);
		o = creat("mon.out", 0666);
		write(o, sbuf, ssiz<<1);
		close(o);
		return;
	}
	if (nargs() <= 4)
		cntsiz = 0;
	ssiz = bufsiz;
	buf[0] = lowpc;
	buf[1] = highpc;
	buf[2] = cntsiz;
	sbuf = buf;
	buf =+ 3*(cntsiz+1);
	bufsiz =- 3*(cntsiz+1);
	if (bufsiz<=0)
		return;
	o = ((highpc - lowpc)>>1) & 077777;
	if(bufsiz < o)
		o = ldiv(bufsiz, 0, o<<1); else
		o = 077777;
	profil(buf, bufsiz<<1, lowpc, o<<1);
}
