#

/*
 * mail command usage
 *	mail [-yn]
 *		prints your mail
 *	mail people
 *		sends standard input to people
 *
 *	mail -r machine user people
 *		sends mail from the network
 */

#define	SIGINT	2
#define DIRECT	040000
#define RMAILCMD "/usr/net/bin/sendmail"

struct inode {
	char minor;
	char major;
	int inumber;
	int flags;
	char nlinks;
	char uid;
	char gid;
	char size0;
	int size1;
	int addr[8];
	int actime[2];
	int modtime[2];
} inode;

char	lettmp[] "/tmp/maXXXXX";
char	preptmp[] "/tmp/mbXXXXX";
int	pwfil;
int	chew;
int	errs;

main(argc, argv)
char **argv;
{
	register int me;
	extern int fout;
	int uf, delexit();
	char namebuf[20];

	mktemp(lettmp);
	mktemp(preptmp);
	unlink(lettmp);
	unlink(preptmp);
	me = getuid();
	if (getname(me, namebuf) < 0) {
		printf("Who are you?\n");
		delexit(1);
	}
	if (argc < 2)
		goto hitit;
	for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++)
	switch(argv[0][1]) {
		register char *cp, *np;

		case 'y':
		case 'n':
			argc++, argv--;
hitit:
			printmail(argc, argv, namebuf);
			delexit(0);

		case 'r':
			if (argc < 2)
				continue;
			if (!equal("network", namebuf) && me != 0) {
				printf("Nice try!\n");
				delexit(1);
			}
			chew++;
			np = namebuf;
			for (cp = argv[1]; *cp; cp++)
				*np++ = *cp;
			argc--, argv++;
			*np++ = ':';
			for (cp = argv[1]; *cp; cp++)
				*np++ = *cp;
			*np++ = 0;
			argc--, argv++;
			continue;
	}
	if ((signal(SIGINT, 01) & 01) == 0)
		signal(SIGINT, delexit);
	unlink(lettmp);
	fout = creat(lettmp, 0600);
	if (fout < 0) {
		fout = 1;
		perror(lettmp);
		delexit(1);
	}
	argc++, argv--;
	bulkmail(argc, argv, namebuf);
	delexit(0);
}

printmail(argc, argv, name)
char **argv;
char *name;
{
	extern int fin, fout;
	register n, c, f;
	char *mname;

	mname = cat("/usr/mail/", name);
	if (stat(mname, &inode)>=0 && inode.nlinks==1 &&
		fopen(mname, &fin)>=0 && (c = getchar())) {
		putchar(c);
		getput();
		close(fin);
		c = 'y';
		if (argc<2) {
			if (ttyn(0)!='x') {
				printf("Save?");
				fin = 0;
				c = getchar();
			}
		} else
			c = argv[1][1];
		if (!any(c, "yn"))
			delexit(0);
		if (c == 'y') {
			if (accesss("mbox")) {
				printf("Saved mail in 'mbox'\n");
				prepend(mname, "mbox", getuid());
				unlink(mname);
			} else
				printf("In wrong directory\n");
		} else
			unlink(mname);
	} else
		printf("No mail.\n");
}

bulkmail(argc, argv, from)
char **argv, *from;
{
	extern int fin, fout;
	register int c;
	int tbuf[2], ttyn1;

	fin = 0;
	(&fin)[1] = 0;
	time(tbuf);
	ttyn1 = ttyn(1);
	if (ttyn1 < 033) {
		ttyn1 =+ 'a' - 1;
		ttyn1 =<< 8;
		ttyn1 =| '^';
	}
	printf("From %s  tty%c  %s", from, ttyn1, ctime(tbuf));
	if (chew)
		do {
			c = getchar();
		} while (c != '\n' && c != 0);
	getput();
	putchar('\n');
	flush();
	close(fout);
	while (--argc > 0)
		sendto(*++argv);
	delexit(errs);
}

sendto(person)
char *person;
{
	static int saved;
	extern int fout, fin;
	register char *filep;
	register int him;

	if ((person[0] == 'y' || person[0] == 'Y') && person[1] == ':')
		person += 2;
	if ((person[0] | ' ') == 'c' && (person[1] | ' ') == 'o' &&
	    (person[2] | ' ') == 'r' && (person[3] | ' ') == 'y' &&
	    person[4] == ':')
		person += 5;
	if (any(':', person) || person[0] == 'm' && person[1] == 's' &&
	    person[2] == 'g' && person[3] == 's' && person[4] == 0) {
		int i = fork();
		int s;

		if (i < 0) {
			perror("fork");
			goto assback;
		}
		if (i == 0) {
			close(0);
			open(lettmp, 0);
			if (any(':', person)) {
				execl(RMAILCMD,"sendmail", person, 0);
				execl("/usr/bin/sendmail", "sendmail", person, 0);
				execl("/bin/sendmail", "sendmail", person, 0);
				perror("sendmail");
			} else {
				execl("/usr/new/msgs", "msgs", "-s", 0);
				execl("/usr/bin/msgs", "msgs", "-s", 0);
			}
			exit(12);
		}
		for (;;) {
			register int j = wait(&s);
			if (j == -1)
				goto assback;
			if (j == i)
				break;
		}
		if ((s & 0377) != 0 || (s >> 8) == 12)
			goto assback;
		return;
	}

	if ((him = getuserid(person)) == -1) {
assback:
		fout = 1;
		flush();
		printf("Can't send to %s.\n", person);
		errs++;
		if (ttyn(0)!='x' && saved==0) {
			saved++;
			if (accesss("dead.letter")) {
				printf("Letter saved in 'dead.letter'\n");
				prepend(lettmp, "dead.letter", getuid());
			} else
				printf("In wrong directory\n");
		}
		return;
	}
	prepend(lettmp, filep=cat("/usr/mail/", person), him);
}

prepend(from, to, own)
char *from, *to;
{
	extern int fin, fout;
	register int sig;

	unlink(preptmp);
	if (fcreat(preptmp, &fout) < 0) {
		fout = 1;
		perror("mail");
		delexit(1);
	}
	chmod(preptmp, 0600);
	if (fopen(from, &fin) < 0) {
		close(fout);
		fout = 1;
		perror("mail");
		unlink(preptmp);
		return(0);
	}
	getput();
	close(fin);
	fopen(to, &fin);
	getput();
	close(fin);
	flush();
	close(fout);
	sig = signal(SIGINT, 01);
	unlink(to);
	if (fcreat(to, &fout) < 0) {
		unlink(preptmp);
		fout = 1;
		signal(SIGINT, sig);
		return(0);
	}
	chmod(to, 0600);
	chown(to, own);
	if(stat(to, &inode) < 0 || inode.nlinks != 1) {
			close(fout);
			fout = 1;
			unlink(preptmp);
			signal(SIGINT, sig);
			return(0);
	}
	if (fopen(preptmp, &fin) < 0) {
		fout = 1;
		perror("mail");
		signal(SIGINT, sig);
		errs++;
		return(0);
	}
	getput();
	flush();
	close(fout);
	close(fin);
	fout = 1;
	signal(SIGINT, sig);
	return(1);
}

delexit(ex)
{
	unlink(lettmp);
	unlink(preptmp);
	exit(ex);
}

equal(as1, as2)
{
	register char *s1, *s2;

	s1 = as1;
	s2 = as2;
	while (*s1++ == *s2)
		if (*s2++ == 0)
			return(1);
	return(0);
}

cat(ap1, ap2)
char *ap1, *ap2;
{
	register char *p1, *p2;
	static char fn[32];

	p1 = ap1;
	p2 = fn;
	while (*p2++ = *p1++);
	p2--;
	p1 = ap2;
	while (*p2++ = *p1++);
	return(fn);
}

getput()
{
	extern int errno;
	register c;

	while(c = getchar()) {
		errno = 0;
		putchar(c);
		if(errno) {
			perror("mail");
			delexit(1);
		}
	}
}

accesss(s1)
{
	if (access(".", 2) != -1 && (stat(s1, &inode)<0 || access(s1, 2)==0))
		return(1);
	return(0);
}

any(c, str)
	char *str;
{
	register char *f;

	f = str;
	while (*f)
		if (c == *f++)
			return(1);
	return(0);
}
