#
/*
 * ttytype - maintain ttytype field in htmp
 *
 * Bill Joy September 24, 1977
 */

char	usagestr[] "usage: ttytype [ -n ] [ [ -d ] type ]\n";
int	mask	0377;

char	htmp[], ttycap[];
char	buf[512], quickie;

main(argc, argv)
	int argc;
	char *argv[];
{
	int uid, tty, type;

	tty = ttyn(2);
	if (tty == 'x') {
		write(1, "Unit 2 not teletype\n", 20);
		exit(1);
	}
	argc--, argv++;
	type = typeof(tty);
	while (argc > 0 && argv[0][0] == '-') {
		switch (argv[0][1]) {
			case 'd':
				if (type != 'du')
					exit(1);
				break;
			case 'n':
				listtypes();
				exit(0);
			case 0:
				quickie++;
				break;
			default:
usage:
				write(2, usagestr, sizeof usagestr);
				exit(1);
		}
		argc--, argv++;
	}
	if (argc > 1)
		goto usage;
	if (argc == 0) {
		if (hget(tty) < 0) {
			perror(htmp);
			exit(1);
		}
		if (!quickie) {
			printf("%s\n", longname(buf, hgettype()));
			exit(0);
		}
		argv--;
		argv[0] = hsgettype();
	}
	switch (tgetent(buf, argv[0])) {
		case -1:
			perror(ttycap);
			exit(1);
		case 0:
			printf("%s: Unknown terminal type\n", argv[0]);
			exit(1);
	}
	if (quickie) {
		printf("%s\n", buf);
		exit(0);
	}
	if (hget(tty) < 0) {
		perror(htmp);
		exit(1);
	}
	hsettype(buf[0] | (buf[1] << 8));
	if (hput(tty) < 0) {
		perror(htmp);
		exit(1);
	}
	exit (0);
}

longname(bp, type)
	register char *bp;
	int type;
{
	register char *cp;
	static char tyname[3];

	tyname[0] = type & 0377;
	tyname[1] = (type >> 8) & 0377;
	tyname[2] = 0;
	switch (tgetent(bp, tyname)) {
		case -1:
			perror(ttycap);
			exit(1);
		case 0:
			return (tyname);
	}
	while (*bp && *bp != ':' && *bp != '|')
		bp++;
	if (*bp == '|') {
		bp++;
		cp = bp;
		while (*cp && *cp != ':' && *cp != '|')
			cp++;
		*cp = 0;
		return (bp);
	}
	return (tyname);
}

listtypes()
{
	register int unit;
	register int c;
	int ibuf[259];

	unit = fopen(ttycap, ibuf);
	if (unit < 0) {
		perror(ttycap);
		exit(1);
	}
	c = 0;
	for (;;) {
		c = getc(ibuf);
		if (c == -1)
			exit(0);
		c = getc(ibuf);
		c = getc(ibuf);
		c = getc(ibuf);
		while (c != -1 && c != '\n' && c != ':') {
			if (c == '|')
				c = ' ';
			putchar(c);
			c = getc(ibuf);
		}
		putchar('\n');
		while (c != '\n' && c != -1)
			c = getc(ibuf);
	}
}

char	utmp[]	"/etc/utmp";

loggedin(tty)
	int tty;
{
	int utio;
	struct utmp {
		char	name[8];
		char	tty;
		char	fill;
		long	timeon;
		char	uid;
		char	gid;
	} uentry;
	struct stb {
		char	minor;
		char	major;
		int	inumber;
		int	flags;
		char	nlinks;
		char	suid;
		char	sgid;
		char	size0;
		int	size1;
		int	addr[8];
		int	actime[2];
		int	modtime[2];
	} stbuf;
	register char *tp;
	int uid;

#ifdef INGRES
	return;
#endif
	uid = getuid() & mask;
	if (uid == 0)
		return;
	tp = "/dev/ttyx";
	tp[8] = tty;
	if (stat(tp, &stbuf)) {
		perror(tp);
		exit(1);
	}
#ifdef INGRES
	if (stbuf.suid == (uid & 0377))
#endif
#ifndef INGRES
	if (stbuf.suid == (uid & 0377) && stbuf.sgid == (uid >> 8))
#endif
		return;
	utio = open(utmp, 0);
	if (utio < 0) {
uerr:
		perror(utmp);
		exit(1);
	}
	seek(utio, tty * sizeof uentry, 0);
	if (read(utio, &uentry, sizeof uentry) != sizeof uentry) {
		close(utio);
		goto uerr;
	}
	close(utio);
	if (uentry.uid == (uid & 0377) && uentry.gid == (uid >> 8))
		return;
	printf("NOT super-user\n");
	exit(1);
}
