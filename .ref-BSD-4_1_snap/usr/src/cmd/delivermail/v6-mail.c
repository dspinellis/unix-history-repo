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
 *
 * if NOTROOT is defined, don't run as root.
 */

#define	SIGINT		2
#define DIRECT		040000
#define RMAILCMD	"/usr/net/bin/sendmail"
#define GETUID()	(getuid() & 0377)
#define	SPOOLDIR	"/usr/spool/mail/"
#define NOTROOT		$

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
char	*strcat(), *strcpy();

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
	me = GETUID();
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
		case 'f':
			if (argc < 1)
				continue;
			if (!equal("network", namebuf) && me != 0) {
				printf("Nice try!\n");
				delexit(1);
			}
			chew++;
			np = namebuf;
			for (cp = argv[1]; *cp; cp++)
				*np++ = *cp;
			if (argv[0][1] == 'r')
			{
				argc--, argv++;
				*np++ = ':';
				for (cp = argv[1]; *cp; cp++)
					*np++ = *cp;
			}
			*np++ = 0;
			argc--, argv++;
			continue;
	}
	if ((signal(SIGINT, 01) & 01) == 0)
		signal(SIGINT, delexit);
	unlink(lettmp);
# ifdef NOTROOT
	fout = creat(lettmp, 0666);
# else
	fout = creat(lettmp, 0600);
# endif
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

	mname = cat(SPOOLDIR, name);
	if (stat(mname, &inode)>=0 && inode.nlinks==1 &&
		fopen(mname, &fin)>=0 && (c = getchar())) {
		putchar(c);
		getput();
		close(fin);
		c = 'x';
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
				prepend(mname, "mbox", GETUID());
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
	register char *cp;
	char linebuf[128];
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

	/*
	 * If delivering mail from the network via mail -r,
	 * Strip the leading line and throw it away, as long
	 * as it begins with "From ..."
	 */

	if (chew) {
		cp = linebuf;
		do {
			c = getchar();
			if (cp - linebuf < 120)
				*cp++ = c;
		} while (c != '\n' && c != 0);
		*cp = '\0';
		if (linebuf[0] != 'F' || linebuf[1] != 'r' ||
		    linebuf[2] != 'o' || linebuf[3] != 'm')
			printf("%s", linebuf);
	}
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
	int i;

	if ((person[0] == 'i' || person[0] == 'I') && person[1] == ':')
		person += 2;
	for (i = 0; person[i] != '\0'; i++)
	{
		if (person[i] == ':')
		{
			person[i] = '\0';
			if (equal(person, "ing70") || equal(person, "ingres"))
				person += i + 1;
			else
				person[i] = ':';
			break;
		}
	}
	if (person[i] == ':' || equal(person, "msgs"))
	{
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
				execl(RMAILCMD, "sendmail", person, 0);
				execl("/usr/bin/sendmail", "sendmail", person, 0);
				execl("/bin/sendmail", "sendmail", person, 0);
				perror("sendmail");
			} else {
				execl("/usr/new/msgs", "msgs", "-s", 0);
				execl("/usr/ucb/msgs", "msgs", "-s", 0);
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
				prepend(lettmp, "dead.letter", GETUID());
			} else
				printf("In wrong directory\n");
		}
		return;
	}
	filep = cat(SPOOLDIR, person);
	lock(filep);
	prepend(lettmp, filep, him);
	unlock();
}

prepend(from, to, own)
char *from, *to;
{
	extern int fin, fout;
	register int sig;
	int statb[18];

	if (stat(to, statb) >= 0 && (statb[2] & 060000) != 0) {
		write(2, "Exotic destination\n", 19);
		delexit(1);
	}
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
# ifdef NOTROOT
	chmod(to, 0666);
# else
	chmod(to, 0600);
	chown(to, own);
# endif
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
	unlock();
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

char	*maillock	= ".lock";		/* Lock suffix for mailname */
char	*lockname	= "/usr/spool/mail/tmXXXXXX";
char	locktmp[30];				/* Usable lock temporary */
char	curlock[50];				/* Last used name of lock */
int	locked;					/* To note that we locked it */

/*
 * Lock the specified mail file by setting the file mailfile.lock.
 * We must, of course, be careful to unlink the lock file by a call
 * to unlock before we stop.  The algorithm used here is to see if
 * the lock exists, and if it does, to check its modify time.  If it
 * is older than 30 seconds, we assume error and set our own file.
 * Otherwise, we wait for 5 seconds and try again.
 */

lock(file)
char *file;
{
	register int f;
	long age;
	struct inode sbuf;
	long curtime;

	if (file == (char *) 0) {
		printf("Locked = %d\n", locked);
		return(0);
	}
	if (locked)
		return(0);
	strcpy(curlock, file);
	strcat(curlock, maillock);
	strcpy(locktmp, lockname);
	mktemp(locktmp);
	unlink(locktmp);
	for (;;) {
		f = lock1(locktmp, curlock);
		if (f == 0) {
			locked = 1;
			return(0);
		}
		if (stat(curlock, &sbuf) < 0)
			return(0);
		time(&curtime);
		age = * ((long *) sbuf.modtime);
		if (curtime < age + 30) {
			sleep(5);
			continue;
		}
		unlink(curlock);
	}
}

/*
 * Remove the mail lock, and note that we no longer
 * have it locked.
 */

unlock()
{

	if (locked)
		unlink(curlock);
	locked = 0;
}

/*
 * Attempt to set the lock by creating the temporary file,
 * then doing a link/unlink.  If it fails, return -1 else 0
 */

lock1(tempfile, name)
	char tempfile[], name[];
{
	register int fd;

	fd = creat(tempfile, 0);
	if (fd < 0)
		return(-1);
	close(fd);
	if (link(tempfile, name) < 0) {
		unlink(tempfile);
		return(-1);
	}
	unlink(tempfile);
	return(0);
}

/*
 * Concatenate s2 on the end of s1.  S1's space must be large enough.
 * Return s1.
 */

char *
strcat(s1, s2)
register char *s1, *s2;
{
	register os1;

	os1 = s1;
	while (*s1++)
		;
	*--s1;
	while (*s1++ = *s2++)
		;
	return(os1);
}

/*
 * Copy string s2 to s1.  s1 must be large enough.
 * return s1
 */

char *
strcpy(s1, s2)
register char *s1, *s2;
{
	register os1;

	os1 = s1;
	while (*s1++ = *s2++)
		;
	return(os1);
}
