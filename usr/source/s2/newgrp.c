int	gbuf[259];
int	pbuf[259];
int	ttyb[3];
char	name[10];
char	passwd[10];
char	space[1000];
int	peek;
int	pwdflg;

main(argc, argv)
char *argv[];
{
	register id;
	register char *p, *q;

	if(argc < 2) {
		printf("usage: newgrp groupname\n");
		done();
	}
	if(fopen("/etc/group", gbuf) < 0) {
		printf("cannot open group file\n");
		done();
	}
	do {
		field(name, gbuf);
		p = name;
		q = argv[1];
		while(*p == *q++)
			if(*p++ == 0 || p >= name+8)
				goto l1;
	} while(skip(gbuf));
	printf("%s: not a valid group name\n", argv[1]);
	done();

l1:
	if(fopen("/etc/passwd", pbuf) < 0) {
		printf("cannot open password file\n");
		done();
	}
	p = space;
	id = getuid() & 0377;
	do {
		field(p, pbuf);
		field(name, pbuf);
		if(value(pbuf) == id) {
			if(name[0] == 0)
				pwdflg++;
			while(*p++) ;
		}
	} while(skip(pbuf));
	*p = 0;

	field(passwd, gbuf);
	id = value(gbuf);
	if(id == 1) {
		pwdflg = 0;
		goto l2;
	}
	do {
		p = space;
		field(name, gbuf);
		while(*p) {
			q = name;
			while(*p == *q++) {
				if(*p++ == 0)
					goto l2;
			}
			while(*p++) ;
		}
	} while(peek == ',');
	goto no;

l2:
	if(pwdflg && passwd[0]) {
		printf("password: ");
		gtty(0, pbuf);
		pbuf[3] = pbuf[2];
		pbuf[2] =& ~010;
		stty(0, pbuf);
		read(0, gbuf, 512);
		pbuf[2] = pbuf[3];
		stty(0, pbuf);
		printf("\n");
		p = name;
		q = gbuf;
		while(p < name+8) {
			if(*q == '\n')
				break;
			*p++ = *q++;
		}
		*p = 0;
		p = crypt(name);
		q = passwd;
		while(q < passwd+8)
			if(*p++ != *q++)
				goto no;
	}
	if(setgid(id) < 0) {
		perror("setgid");
		goto no;
	}
	done();

no:
	printf("Sorry\n");
	done();
}

skip(buf)
{

	while(peek > 0) {
		if(peek == '\n') {
			peek = 1;
			return(1);
		}
		peek = getc(buf);
	}
	return(0);
}

field(cp, buf)
{
	register c;
	register char *p, *q;

	p = cp;
	q = p+8;
	while((c = getc(buf)) != '\n') {
		if(c == ',' || c == ':' || c <= 0)
			break;
		if(p < q)
			*p++ = c;
	}
	*p = 0;
	peek = c;
}

value(buf)
{
	register n, c;

	n = 0;
	while((c = getc(buf)) >= '0' && c <= '9')
		n = n*10 + c-'0';
	peek = c;
	return(n);
}

done()
{

	setuid(getuid());
	execl("/bin/sh", "-", 0);
	exit();
}
