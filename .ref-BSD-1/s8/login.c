#
/*
 * login [ name ]
 */

struct {
	char	name[8];
	char	tty;
	char	ifill;
	int	time[2];
	int	uuid;
} utmp, xtmp;

struct {
	int	speeds;
	char	erase, kill;
	int	tflags;
} ttyb;

struct {
	int	junk[5];
	int	size;
	int	more[12];
} statb;

extern	htmp;
struct {
	int	l_tty;
	int	l_time[2];
} last;

char mailfile[] "/usr/mail/xxxxxxxx";
char kickoff[]	"/etc/kickoff";
char lastlog[]	"/usr/adm/lastlog/XXXX";

char	*ttyx;

#define	ECHO	010

main(argc, argv)
char **argv;
{
	char pbuf[128];
	register char *namep, *np, *mp;
	int quiet;
	char pwbuf[9], *cp, *ll;
	int t, sflags, f, c, uid, gid, badname;

	alarm(300); /* 5 minutes to log in */
	signal(3, 1);
	signal(2, 1);
	nice(-2);
	ttyx = "/dev/ttyx";
	if ((utmp.tty=ttyn(0)) == 'x') {
		write(1, "Sorry.\n", 7);
		exit();
	}
	ttyx[8] = utmp.tty;
    loop:
	badname = 0;
	namep = utmp.name;
	mp = &mailfile[10];
	if (argc>1) {
		np = argv[1];
		while (namep<utmp.name+8 && *np) {
			*mp++ = *np;
			*namep++ = *np++;
		}
		argc = 0;
	} else {
		write(1, "Name: ", 7);
		while ((c = getchar()) != '\n') {
			if (c <= 0)
				exit();
			if (namep < utmp.name+8) {
				*mp++ = c;
				*namep++ = c;
			}
		}
		if(namep == utmp.name)
			goto loop;
	}
	while (namep < utmp.name+8)
		*namep++ = ' ';
	*mp = '\0';
	if (getpwentry(utmp.name, pbuf, "/etc/passwdf")) {
		badname++;
		goto askpw;
	}
	mp = np = colon(pbuf);
	if (*np!=':') {
	   askpw:
		gtty(0, &ttyb);
		sflags = ttyb.tflags;
		ttyb.tflags =& ~ ECHO;
		stty(0, &ttyb);
		write(1, "Password: ", 10);
		namep = pwbuf;
		while ((c=getchar()) != '\n') {
			if (c <= 0)
				exit();
			if (namep<pwbuf+8)
				*namep++ = c;
		}
		*namep++ = '\0';
		ttyb.tflags = sflags;
		stty(0, &ttyb);
		write(1, "\n", 1);
		namep = crypt(pwbuf);
		while (*namep++ == *np++);
		if (*--namep== '\0' && *--np ==':')
			goto good;
		namep = crypt(pwbuf);
		np = mp;
		while (*namep++ == *np++);
		if (*--namep!= '\0' || *--np !=':' || badname)
			goto bad;
good:	;
	}
	if (getpwentry(utmp.name, pbuf, "/etc/passwd"))
		goto bad;
	np = colon(pbuf);
	np = colon(np);
	uid = 0;
	while (*np != ':')
		uid = uid*10 + *np++ - '0';
	np++;
	ll = &lastlog[17];
	gid = 0;
	while (*np != ':') {
		*ll++ = *np;
		gid = gid*10 + *np++ - '0';
	}
	*ll = 0;
	np++;
	np = colon(np);
	namep = np;
	np = colon(np);
	time(utmp.time);
	utmp.uuid = (gid<<8)|uid;
	if ((f = open("/etc/utmp", 1)) >= 0) {
		t = utmp.tty;
		seek(f, (t&0177)*16, 0);
		write(f, &utmp, 16);
		close(f);
	}
	if ((f = open("/usr/adm/wtmp", 1)) >= 0) {
		seek(f, 0, 2);
		write(f, &utmp, 16);
		close(f);
	}
	f = gid<<8 | uid;
	if (f == 0)
		if (utmp.tty!='5' && utmp.tty!='8' && utmp.tty!='t')
			goto bad;
	if ((f=open(lastlog, 2)) < 0) {
		if ((f=creat(lastlog, 0600)) < 0)
			goto cont;
		close(f);
		if ((f=open(lastlog, 2)) < 0)
			goto cont;
	}
	seek(f, uid*6, 0);
	if (read(f, &last.l_tty, 6) == 6) {
		if (last.l_time[0]) {
			ll = ctime(last.l_time);
			c = ' ';
			if (last.l_tty < 033) {
				c = last.l_tty - 1 + 'a';
				last.l_tty = '^';
			}
		printf("Last login: %.16s on tty%c%c\n", ll, last.l_tty, c);
		}
	}
	seek(f, uid*6, 0);
	write(f, &utmp.tty, 6);
	close(f);
    cont:
	quiet = 0;
	if(utmp.name[0] >= 'A' && utmp.name[0] <= 'Z')
		quiet++;
	if(!quiet)
	if ((f = open("/etc/motd", 0)) >= 0) {
		while ((t=read(f, &xtmp, sizeof xtmp)) > 0)
			write(1, &xtmp, t);
		close(f);
	}
	if(!quiet)
	if(stat(mailfile, &statb) >= 0 && statb.size)
		write(1, "You have mail.\n", 15);
	chown(ttyx, gid<<8|uid);
	hsetuid(gid << 8 | uid);
	hsethome(namep);
	hsettype(typeof(utmp.tty));
	hput(utmp.tty);
	setuid(gid<<8 | uid);
	f = 0;
	if (fork() == 0) {
		execl(kickoff, "kickoff", 0);
		exit();
	} else {
		if (gid || uid) {
			wait(&f);
			if (f>>8 != 0) {
				printf("priority too low to log in\n");
				exit();
			}
		}
	}
	for (;;) {
		if (*namep == '\0')
			break;
		cp = namep++;
		for (; *namep != '/' && *namep != '\0'; namep++);
		if (*namep == '/')
			*namep++ = '\0';
		if (chdir(cp)<0) {
			write(1, "No directory\n", 13);
			exit();
		}
		alarm(0);
		if(!quiet)
		if ((f = open(".reminder", 0)) >= 0) {
			while ((t=read(f, &utmp, sizeof utmp)) > 0)
				write(1, &utmp, t);
			close(f);
		}
	}
	nice(0);
	if (*np == '\0')
		np = "/bin/sh";
	if (stat(".start_up", &statb) >= 0 && statb.size)
		if (fork() == 0) {
			execl("/bin/sh", "-", ".start_up", 0);
			exit();
		} else
			wait(&f);
	execl(np, "-", 0);
	write(1, "No shell.\n", 9);
	exit();
bad:
	write(1, "Login incorrect.\n", 17);
	goto loop;
}

getpwentry(name, buf, fname)
char *name, *buf;
{
	extern fin;
	int fi, r, c;
	register char *gnp, *rnp;

	fi = fin;
	r = 1;
	if((fin = open(fname, 0)) < 0)
		goto ret;
loop:
	gnp = name;
	rnp = buf;
	while((c=getchar()) != '\n') {
		if(c <= 0)
			goto ret;
		*rnp++ = c;
	}
	*rnp++ = '\0';
	rnp = buf;
	while (*gnp++ == *rnp++);
	if ((*--gnp!=' ' && gnp<name+8) || *--rnp!=':')
		goto loop;
	r = 0;
ret:
	close(fin);
	fin = 0;
	(&fin)[1] = 0;
	(&fin)[2] = 0;
	return(r);
}

colon(p)
char *p;
{
	register char *rp;

	rp = p;
	while (*rp != ':') {
		if (*rp++ == '\0') {
			write(1, "Bad /etc/passwd\n", 16);
			exit();
		}
	}
	*rp++ = '\0';
	return(rp);
}
