#
/*
 * login [ name ]
 */

struct {
	char	name[8];
	char	tty;
	char	ifill;
	int	time[2];
	int	ufill;
} utmp;

struct {
	int	regs[2];
	int	tflags;
} ttyb;

char	*ttyx;

#define	ECHO	010

main(argc, argv)
char **argv;
{
	char pbuf[128];
	register char *namep, *np;
	char pwbuf[9];
	int t, sflags, f, c, uid, gid;

	signal(3, 1);
	signal(2, 1);
	ttyx = "/dev/ttyx";
	if ((utmp.tty=ttyn(0)) == 'x') {
		write(1, "Sorry.\n", 7);
		exit();
	}
	ttyx[8] = utmp.tty;
    loop:
	namep = utmp.name;
	if (argc>1) {
		np = argv[1];
		while (namep<utmp.name+8 && *np)
			*namep++ = *np++;
		argc = 0;
	} else {
		write(1, "Name: ", 7);
		while ((c = getchar()) != '\n') {
			if (c <= 0)
				exit();
			if (namep < utmp.name+8)
				*namep++ = c;
		}
	}
	while (namep < utmp.name+8)
		*namep++ = ' ';
	if (getpwentry(utmp.name, pbuf))
		goto bad;
	np = colon(pbuf);
	if (*np!=':') {
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
		if (*--namep!='\0' || *--np!=':')
			goto bad;
	}
	np = colon(np);
	uid = 0;
	while (*np != ':')
		uid = uid*10 + *np++ - '0';
	np++;
	gid = 0;
	while (*np != ':')
		gid = gid*10 + *np++ - '0';
	np++;
	np = colon(np);
	namep = np;
	np = colon(np);
	if (chdir(namep)<0) {
		write(1, "No directory\n", 13);
		goto loop;
	}
	time(utmp.time);
	if ((f = open("/etc/utmp", 1)) >= 0) {
		t = utmp.tty;
		if (t>='a')
			t =- 'a' - (10+'0');
		seek(f, (t-'0')*16, 0);
		write(f, &utmp, 16);
		close(f);
	}
	if ((f = open("/usr/adm/wtmp", 1)) >= 0) {
		seek(f, 0, 2);
		write(f, &utmp, 16);
		close(f);
	}
	if ((f = open("/etc/motd", 0)) >= 0) {
		while(read(f, &t, 1) > 0)
			write(1, &t, 1);
		close(f);
	}
	if ((f = open("mailbox", 0)) >= 0) {
		write(1, "You have mail.\n", 15);
		close(f);
	}
	chown(ttyx, uid);
	setgid(gid);
	setuid(uid);
	if (*np == '\0')
		np = "/bin/sh";
	execl(np, "-", 0);
	write(1, "No shell.\n", 9);
	exit();
bad:
	write(1, "Login incorrect.\n", 17);
	goto loop;
}

getpwentry(name, buf)
char *name, *buf;
{
	extern fin;
	int fi, r, c;
	register char *gnp, *rnp;

	fi = fin;
	r = 1;
	if((fin = open("/etc/passwd", 0)) < 0)
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
