/*
 * who
 */

int	fout;
int	buf[256];
char pbuf[100];

main(argc, argv)
char **argv;
{
	char *s, *cbuf;
	int n, fi, i;
	int tty;
	struct {
		char name[8];
		char tty;
		char changed;
		int time[2];
		int uid;
	} *p;

	s = "/etc/utmp";
	if(argc == 2)
		s = argv[1];
	fi = open(s, 0);
	if(fi < 0) {
		write("cannot open wtmp\n", 17);
		exit();
	}
	fout = dup(1);
	close(1);
	if (argc==3)
		tty = ttyn(2);

loop:
	n = read(fi, buf, 512);
	if(n == 0) {
		flush();
		if (argc==3)
			write(fout, "Nobody.\n", 8);
		exit();
	}

	p = &buf;
	for(p = &buf; (n =- 16)>=0; p++) {
		if (argc==3 && tty!=p->tty)
			continue;
		if(p->name[0] == '\0' && argc==1)
			continue;
		for(i=0; i<8; i++) {
			if (p->name[i]<' ' || p->name[i]>'z')
				p->name[i] = ' ';
			putchar(p->name[i]);
		}
		putchar(' ');
		for(i=0; i<3; i++)
			putchar("tty"[i]);
		i = p->tty;
		if (i < 033) {
			putchar('^');
			putchar(i - 1 + 'a');
		} else {
			putchar(i);
			putchar(' ');
		}
		cbuf = ctime(p->time);
		for(i=3; i<16; i++)
			putchar(cbuf[i]);
		if (p->changed && argc != 2)
		{
			if (getpw(p->uid, pbuf))
				goto out;
			s = pbuf;
			putchar(' ');
			for(i=0; i < 8; i++)
				if (*s == ':')
					break;
				else	putchar(*s++);
		}
out:
		putchar('\n');
		if (argc==3) {
			flush();
			exit();
		}
	}
	goto loop;
}
