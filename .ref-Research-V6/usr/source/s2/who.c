/*
 * who
 */

int	fout;
int	buf[256];

main(argc, argv)
char **argv;
{
	char *s, *cbuf;
	int n, fi, i;
	int tty;
	struct {
		char name[8];
		char tty;
		char pad1;
		int time[2];
		char pad2[2];
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
		tty = ttyn(0);

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
			if(p->name[i] == '\0')
				p->name[i] = ' ';
			putchar(p->name[i]);
		}
		for(i=0; i<3; i++)
			putchar("tty"[i]);
		putchar(p->tty);
		cbuf = ctime(p->time);
		for(i=3; i<16; i++)
			putchar(cbuf[i]);
		putchar('\n');
		if (argc==3) {
			flush();
			exit();
		}
	}
	goto loop;
}
