/*
 * enter a password in the password file
 * this program should be suid with owner
 * with an owner with write permission on /etc/passwd
 */
char	*tfile	{ "/usr/adm/ptmp" };
char	*pfile	{ "/etc/passwd" };
char	*pw;
char	pwbuf[10];
int	ttybuf[3];
int	tbuf[259];
int	pbuf[259];

main(argc, argv)
char *argv[];
{
	register u, c;
	register char *p;
	int exitt();
	int v;
	char x;

	if(argc < 2) {
		write(2, "Usage: passwd user [ password ]\n", 32);
		goto bex;
	}
	if(argc == 2) {
		signal(1, &exitt);
		signal(2, &exitt);
		signal(3, &exitt);
		gtty(0, ttybuf);
		ttybuf[2] =& ~010;
		stty(0, ttybuf);
again:
		write(2, "Password: ", 10);
		p = pwbuf;
		for(;;) {
			if(read(0, p, 1) != 1)
				break;
			if(*p == '\n')
				break;
			if(p < pwbuf+9)
				p++;
		}
		*p = 0;
		write(2, "\nAgain: ", 8);
		p = pwbuf;
		for (;;) {
			if (read(0, &x, 1) != 1 || x == '\n')
				break;
			if (*p++ != x) {
xxx:
				while (read(0, &x, 1) == 1 && x != '\n')
					continue;
				write(2, "\nThose weren't the same\n", 24);
				goto again;
			}
		}
		if (*p != 0)
			goto xxx;
		ttybuf[2] =| 010;
		stty(0, ttybuf);
		write(2, "\n", 1);
		pw = pwbuf;
	} else
		pw = argv[2];
	signal(1, 1);
	signal(2, 1);
	signal(3, 1);

	if(stat(tfile, tbuf+20) >= 0) {
		write(2, "Temporary file busy -- try again\n", 33);
		goto bex;
	}
	tbuf[0] = creat(tfile, 0600);
	if(tbuf[0] < 0) {
		write(2, "Cannot create temporary file\n", 29);
		goto bex;
	}
	pbuf[0] = open(pfile, 0);
	if(pbuf[0] < 0) {
		write(2, "Cannot open /etc/passwd\n", 25);
		goto out;
	}
	goto l1;

/*
 * skip to beginning of next line
 */

skip:
	while(c != '\n') {
		if(c < 0)
			goto ill;
		c = getc(pbuf);
		putc(c, tbuf);
	}

/*
 * compare user names
 */

l1:
	c = getc(pbuf);
	putc(c, tbuf);
	if(c < 0) {
		write(2, "User name not found in password file\n", 37);
		goto out;
	}
	p = argv[1];
	while(c != ':') {
		if(*p++ != c)
			goto skip;
		c = getc(pbuf);
		putc(c, tbuf);
	}
	if(*p)
		goto skip;
/*
 * skip old password
 */
	do {
		c = getc(pbuf);
		if(c < 0)
			goto ill;
	} while(c != ':');

/*
 * copy in new password
 */
	p = pw;
	for(c=0; c<9; c++)
		if(*p++ == 0)
			break;
	*--p = 0;
	if(p != pw)
		p = crypt(pw);
	while(*p)
		putc(*p++, tbuf);
	putc(':', tbuf);

/*
 * validate uid and gid
 */

	u = 0;
	do {
		c = getc(pbuf);
		putc(c, tbuf);
		if(c >= '0' && c <= '9')
			u = u*10 + c-'0';
		if(c < 0)
			goto ill;
	} while(c != ':');
	v = 0;
	do {
		c = getc(pbuf);
		putc(c, tbuf);
		if(c >= '0' && c <= '9')
			v = v*10 + c-'0';
		if(c < 0)
			goto ill;
	} while(c != ':');
	c = getuid();
	if(c == 0)
		goto ok;
	if(c == ((v<<8) | u))
		goto ok;
	if((c&0377) == 0 && ((c>>8)&0377) == v)
		goto ok;
	write(2, "Permission denied\n", 18);
	goto out;
ok:

/*
 * copy out and back
 */

	for(;;) {
		c = getc(pbuf);
		if(c < 0) {
			fflush(tbuf);
			close(pbuf[0]);
			close(tbuf[0]);
			tbuf[0] = open(tfile, 0);
			if(tbuf[0] < 0) {
				write(2, "Urk\n", 4);
				goto out;
			}
			pbuf[0] = creat(pfile, 0644);
			if(pbuf[0] < 0) {
				write(2, "Cannot create /etc/passwd\n", 27);
				goto out;
			}
			while((c = read(tbuf[0], tbuf+1, 512)) > 0)
				write(pbuf[0], tbuf+1, c);
			unlink(tfile);
			exit(0);
		}
		putc(c, tbuf);
	}

ill:
	write(2, "Password file illformed\n", 24);

out:
	unlink(tfile);

bex:
	exit(1);
}

exitt()
{
	ttybuf[2] =| 010;
	stty(0, ttybuf);
	write(2, "\n", 1);
	exit(1);
}
