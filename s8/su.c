/* su -- set user id */

int	chngflg;
char	password[100];
char	pwbuf[100];
int	ttybuf[3];
int uid;
char	Home[40];
int	Uid, attyn;
extern	htmp;

main(argc,argv)
char **argv;
{
	register char *p, *q;
	int i;
	char *nptr;
	int badsw;
	extern fin;

	badsw = 0;
	if (argc>1) 
	{
		if (getuid() != 0)
		{
			printf("sorry: not super-user\n");
			exit(1);
		}
		nptr = *++argv;
	}
	else
		nptr = "root";
	if ((uid = getpwid(nptr, pwbuf, "/etc/passwdf")) == -1)
		badsw++;
	(&fin)[1] = 0;
	p = pwbuf;
	while(*p != ':')
		if(*p++ == '\0')
			goto badpw;
	if(*++p == ':')
		goto ok;
	if(getuid() == 0)
	{
		while (*p != ':')
			if (*p++ == '\0')
				goto badpw;
		goto ok;
	}
	gtty(0, ttybuf);
	ttybuf[2] =& ~010;
	stty(0, ttybuf);
	printf("Password: ");
	q = password;
	while((*q = getchar()) != '\n') {
		if (q >= &password[80]) {
			badsw++;
			break;
		}
		if(*q++ == '\0')
			return;
	}
	*q = '\0';
	ttybuf[2] =| 010;
	stty(0, ttybuf);
	printf("\n");
	if (badsw)
		goto error;
	q = crypt(password);
	while(*q++ == *p++);
	if(*--q == '\0' && *--p == ':')
		goto ok;
	goto error;

badpw:
	printf("bad password file\n");
	return;
ok:
	if (getpwid(nptr, pwbuf, "/etc/passwd") == -1)
		goto badpw;
	p = pwbuf;
	for (i = 0; i < 5; i++)
	{
		p++;
		while (*p != ':')
			if (*p++ == '\0')
				goto badpw;
	}
	q = ++p;
	while (*p != ':')
		if (*p++ == '\0')
			goto badpw;
	*p++ = '\0';
	attyn = ttyn(2);
	hget(attyn);
	strcpy(Home, hgethome());
	Uid = hgetuid();
	hsethome(q);
	hsetuid(uid);
	hput(attyn);
	if (uid != 0 && chdir(q) < 0)
	{
		printf("no directory!\n");
		terminate();
	}
	if (*p != '\0')
	{
		update(1);
		if (fork() == 0)
		{
			setuid(uid);
			execl(p, "-setuser", 0);
			printf("no shell!\n");
			exit(1);
		}
		terminate();
	}
	trace("ok ");
	update(1);
	if (fork() == 0)
	{
		setuid(uid);
		execl("/bin/sh", "-setuser", 0);
		printf("cannot execute shell\n");
		exit(1);
	}
	terminate();
error:
	printf("sorry\n");
	trace("bad");
}

trace(ptr)
char *ptr;
{
	register char *p,*q;
	register int num;
	char passbuf[200];
	int fp;

	if (uid != 0)
		return;
	if ((fp=open("/usr/adm/su",2))<0)
		return;
	seek (fp,0,2);
	q = passbuf;
	p = ptr;
	while (*p != '\0')
		*q++ = *p++;
	for (num=0; num<4; num++)
		*q++ = ' ';
	time(ttybuf);
	p = ctime(ttybuf);
	while (*p != '\n')
		*q++ = *p++;
	for (num=0; num<4; num++)
		*q++ = ' ';
	num = getuid();
	if (getpw(num,pwbuf)==0) {
		p = pwbuf;
		while (*p && *p != ':')
			*q++ = *p++;
	}
	if (ptr[0] == 'b')
	{
		p = "\tpw = ";
		while (*q++ = *p++);
		--q;
		p = password;
		while (*q++ = *p++);
		--q;
		*q++ = '\t';
		p = "tty = ";
		while (*q++ = *p++);
		--q;
		*q++ = ttyn(0);
	}
	*q++ = '\n';
	write(fp,passbuf,q-passbuf);
	close(fp);
}

/*
	getpwid -- return user id from user name
*/

getpwid(name, buf, fname)
char *name;
char buf[];
{
	static pbuf[259];
	register char c;
	register char *bp, *aptr;
	int n, m;

	if (fopen(fname, pbuf) < 0)
		return(-1);
	m = 0;
	n = -1;

	for (;;) {
		bp = buf;
		while((c=getc(pbuf)) != '\n') {
			if(c <= '\0')
				goto out;
			*bp++ = c;
		}
		*bp++ = '\0';
		bp = buf;
		aptr = name;
		while ((c = *bp++) != ':')
			if (c != *aptr++)
				goto next;
		if (*aptr!='\0' && *aptr!=' ')
			goto next;
		while ((c = *bp++) != ':')
			if (c == '\0')
				goto out;
		n = 0;
		while ((c = *bp++) != ':')
			n = n*10 + c - '0';
		while ((c = *bp++) != ':')
			m = m*10 + c - '0';
out:
		close(pbuf[0]);
		pbuf[0] = 0;
		pbuf[1] = 0;
		pbuf[2] = 0;
		return(m<<8 | n);
next:
	continue;
	}
}

update(flag)
{
	register i, tty;

	i = open("/etc/utmp", 2);
	tty = ttyn(0);
	if (tty == 'x')
	{
		printf("standard input not a tty!\n");
		exit(1);
	}
	seek(i, (tty & 0177) * 16 + 9, 0);
	read(i, &chngflg, 1);
	seek(i, -1, 1);
	write(i, &flag, 1);
	seek(i, 4, 1);
	write(i, &uid, 2);
}

terminate()
{
	int i;

	signal(1, 1);
	signal(2,1);
	signal(3,1);
	while (wait(&i) != -1);
	uid = getuid();
	update(chngflg);
	strcpy(hgethome(), Home);
	hsetuid(Uid);
	hput(attyn);
	exit(0);
}

strcpy(to, from)
	char *to, *from;
{
	while (*to++ = *from++)
		continue;
}
