#
/* mail command usage
	mail
		prints your mail
	mail people
		sends standard input to people
 */

#define	SIGINT	2

struct	utmp {
	char	name[8];
	char	tty;
	char	pad1;
	int	ltime[2];
	int	pad2;
};

struct	passwd {
	char	*pw_name;
	char	*pw_passwd;
	int	pw_uid;
	int	pw_gid;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
};

char	lettmp[] "/tmp/maxxxxx";
char	preptmp[] "/tmp/mbxxxxx";
int	pwfil;

main(argc, argv)
char **argv;
{
	register me;
	extern fout;
	register struct passwd *p;
	register char *cp;
	static struct utmp ubuf;
	int uf;

	maketemp();
	if (argc==1 || argc==2 && argv[1][0]=='-') {
		printmail(argc, argv);
		delexit();
	}
	signal(SIGINT, delexit);
	fout = creat(lettmp, 0600);
	if (((me=ttyn(1))!='x' || (me=ttyn(2))!='x')
	 && (uf = open("/etc/utmp", 0)) > 0) {
		while (read(uf, &ubuf, sizeof ubuf) == sizeof ubuf)
			if (ubuf.tty == me) {
				ubuf.name[8] = ' ';
				close(uf);
				for (cp=ubuf.name; *cp++!=' ';);
				*--cp = 0;
				bulkmail(argc, argv, ubuf.name);
			}
	}
	me = getuid() & 0377;
	setpw();
	for (;;)
		if ((p = getpwent()) && p->pw_uid == me)
			bulkmail(argc, argv, p->pw_name);
	fout = 1;
	printf("Who are you?\n");
	delexit();
}

printmail(argc, argv)
char **argv;
{
	extern fin, fout;
	register n, c, f;

	if (fopen(".mail", &fin)>=0 && (c = getchar())) {
		do {
			putchar(c);
		} while (c = getchar());
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
		if (c=='y') {
			prepend(".mail", "mbox");
			printf("Saved mail in 'mbox'\n");
		}
		close(creat(".mail"));
	} else
		printf("No mail.\n");
}

bulkmail(argc, argv, from)
char **argv, *from;
{
	extern fin, fout;
	int tbuf[2];
	register c;

	fin = 0;
	(&fin)[1] = 0;
	time(tbuf);
	printf("From %s %s", from, ctime(tbuf));
	while (c = getchar())
		putchar(c);
	putchar('\n');
	flush();
	close(fout);
	while (--argc > 0)
		sendto(*++argv);
	delexit();
}

sendto(person)
char *person;
{
	static saved;
	extern fout;
	extern fin;
	register struct passwd *p;

	setpw();
	while (p = getpwent()) {
		if (equal(p->pw_name, person)) {
			if (prepend(lettmp, cat(p->pw_dir, "/.mail"))==0)
				break;
			return;
		}
	}
	fout = 1;
	flush();
	printf("Can't send to %s.\n", person);
	if (ttyn(0)!='x' && saved==0) {
		unlink("dead.letter");
		saved++;
		printf("Letter saved in 'dead.letter'\n");
		prepend(lettmp, "dead.letter");
	}
}

prepend(from, to)
char *from, *to;
{
	extern int fin, fout;

	fcreat(preptmp, &fout);
	fopen(from, &fin);
	while (putchar(getchar()));
	close(fin);
	fopen(to, &fin);
	while (putchar(getchar()));
	close(fin);
	flush();
	close(fout);
	if (fcreat(to, &fout) < 0) {
		fout = 1;
		return(0);
	}
	fopen(preptmp, &fin);
	while(putchar(getchar()));
	flush();
	close(fout);
	close(fin);
	fout = 1;
	return(1);
}

setpw()
{
	extern fin;

	if (pwfil == 0) {
		fopen("/etc/passwd", &fin);
		pwfil = fin;
	} else
		fin = pwfil;
	(&fin)[1] = 0;
	seek(fin, 0, 0);
}

getpwent()
{
	register char *p;
	register c;
	static struct passwd passwd;
	static char line[100];
	extern fin;

	p = line;
	while((c=getchar()) != '\n') {
		if(c <= 0)
			return(0);
		if(p < line+98)
			*p++ = c;
	}
	*p = 0;
	p = line;
	passwd.pw_name = p;
	p = pwskip(p);
	passwd.pw_passwd = p;
	p = pwskip(p);
	passwd.pw_uid = atoi(p);
	p = pwskip(p);
	passwd.pw_gid = atoi(p);
	p = pwskip(p);
	passwd.pw_gecos = p;
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
	passwd.pw_shell = p;
	return(&passwd);
}

pwskip(ap)
char *ap;
{
	register char *p;

	p = ap;
	while(*p != ':') {
		if(*p == 0)
			return(p);
		p++;
	}
	*p++ = 0;
	return(p);
}

delexit()
{
	unlink(lettmp);
	unlink(preptmp);
	exit(0);
}

maketemp()
{
	int i, pid, d;

	pid = getpid();
	for (i=11; i>=7; --i) {
		d = (pid&07) + '0';
		lettmp[i] = d;
		preptmp[i] = d;
		pid =>> 3;
	}
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
