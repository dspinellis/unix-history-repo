getpw(uid, buf)
int uid;
char buf[];
{
	auto pbuf[259];
	static pwf;
	register n, c;
	register char *bp;

	if(pwf == 0) {
		fopen("/etc/passwd", pbuf);
		pwf = pbuf[0];
	} else
		pbuf[0] = pwf;
	if(pwf < 0)
		return(1);
	seek(pwf, 0, 0);
	pbuf[1] = 0;
	pbuf[2] = 0;

	for (;;) {
		bp = buf;
		while((c=getc(pbuf)) != '\n') {
			if(c == '\0')
				return(1);
			*bp++ = c;
		}
		*bp++ = '\0';
		bp = buf;
		n = 3;
		while(--n)
		while((c = *bp++) != ':')
			if(c == '\n')
				return(1);
		while((c = *bp++) != ':') {
			if(c<'0' || c>'9')
				continue;
			n = n*10+c-'0';
		}
		if(n == uid)
			return(0);
	}
	return(1);
}
