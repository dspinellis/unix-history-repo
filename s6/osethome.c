/*
 * sethome - show/change the home directory entry in htmp
 *
 * Bill Joy UCB May/June/September 1977
 */
struct htmp {
	int	uid;
	char	home[28];
	int	ttytype;
} hentry;

char	buf[100];
int	uid, tty;

char	htmp[];

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *bufp, *cp;
	register int i;

	uid = getuid();
	if (uid == 0)
		exit(0);
	tty = ttyn(2);
	if (tty == 'x') {
		printf("Unit 2 not teletype\n");
		exit(1);
	}
	if (argc > 2) {
		printf("Usage: sethome [ homedir ]\nor: sethome -\n");
		exit (1);
	}
	if (argc == 2 && argv[1][0] == '-') {
		if (hget(tty) < 0) {
			printf("Error reading data base\n");
			exit(1);
		}
		printf("%s\n", hgethome());
		exit(0);
	}
	loggedon(tty);
	if (argc == 1) {
		if (getpw(uid, buf)) {
			printf("Uid %d not in password file\n", uid);
			exit(1);
		}
		bufp = buf;
		for (i = 1; i < 6; i++) {
			while (*bufp != ':')
				if (*bufp++ == 0) {
					printf("Uid %d bad passwd entry\n", uid);
					exit(1);
				}
			bufp++;
		}
		for (cp = bufp; *cp && *cp != ':'; cp++)
			continue;
		*cp = 0;
	} else
		bufp = argv[1];
	if (hget(tty) < 0) {
		perror(htmp);
		exit(1);
	}
	hsethome(bufp);
	hsetuid(uid);
	if (hput(tty) < 0) {
		perror(htmp);
		exit(1);
	}
	exit(0);
}

char	utmp[]	"/etc/utmp";

loggedon(tty)
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

	if (uid == 0)
		return;
	tp = "/dev/ttyx";
	tp[8] = tty;
	if (stat(tp, &stbuf)) {
		perror(tp);
		exit(1);
	}
	if (stbuf.suid == (uid & 0377) && stbuf.sgid == (uid >> 8))
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
