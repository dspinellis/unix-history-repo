/* mail command usage
	mail
	prints your mail
	mail file people
	sends file to people by login name
	mail person
	sends following tty input to him
 */
char iobuf[518];
char ubuf[16];

main(argc, argv)
char *argv[];
{
	char *buf;
	static char myname[20], junk[40], number[20];
	static char hisname[40], hisdir[40];
	auto j, j0, k, n, mail, file, mynumber;

	if(argc==1 || (argc==2 && argv[1][0]=='-'))
		goto printm;
	if(fopen("/etc/passwd", iobuf) < 0) {
		printf("Can't open password file\n");
		exit();
	}
	if(mynumber = ttyn(0))	{
		if((file = open("/tmp/utmp",0)) < 0)
			goto otherway;
		while(read(file,ubuf,16))	{
			if(mynumber == ubuf[8])	{
				close(file);
				file = 0;
				while(file < 8 && ubuf[file] != ' ')	{
					myname[file] = ubuf[file];
					file++;
					}
				myname[file] = 0;
				goto namefound;
				}
			}
		}
otherway:
	mynumber = getuid();
	for(;;) {
		if(!(getfield(myname) &&
		     getfield(junk) &&
		     getfield(number) &&
		     getfield(junk) &&
		     getfield(junk) &&
		     getfield(junk) &&
		     getfield(junk))) {
			printf("Who are you?\n");
			exit();
		}
		if(atoi(number) == mynumber)
			break;
	}

namefound:
	close(iobuf[0]);
	if(argc > 2)
		file = argv[1];
	else
		file = 0;
	j0 = argc>2? 1: 0;
	fopen("/etc/passwd", iobuf);
	k = 2;
	for(;;) {
		if(!(getfield(hisname) &&
		     getfield(junk) &&
		     getfield(junk) &&
		     getfield(junk) &&
		     getfield(junk) &&
		     getfield(hisdir) &&
		     getfield(junk)))
			break;
		j = j0;
		while(++j < argc)
		if(comp(argv[j], hisname)) {
			send(file, myname, hisname, hisdir);
			argv[j][0] = 0;
			if(++k >= argc)
				exit();
		}
	}

	j = j0;
	while(++j < argc)
	if(argv[j][0] != 0) {
		junk[0] = 0;
		append(argv[j], junk);
		send(file, myname, argv[j], junk);
	}
	exit();

printm:
	if((mail=open("mailbox", 0)) < 0) {
		printf("No mail\n");
		exit();
	}
	while(n = read(mail, iobuf, 512))
		write(1, iobuf, n);
	if(argc != 2) {
		printf("Save? ");
		read(0, buf=iobuf, 512);
	} else
		buf = &argv[1][1];
	if(buf[0] == 'y')  {
		if(concat("mailbox","mbox",0))
			printf("Old mail in `mbox'\n");
	}
	unlink("mailbox");
	exit();
}

getfield(buf)
char buf[];
{
	int j;
	char c;

	j = 0;
	while((c = buf[j] = getc(iobuf)) >= 0)
	if(c==':' || c=='\n') {
		buf[j] =0;
		return(1);
	} else
		j++;
	return(0);
}

/* send overwrites hisdir */

send(file, myname, hisname, hisdir)
char hisname[], myname[], hisdir[];
{
	static char buf[512];
	int	n;

	for(n = 0 ; hisdir[n++];);
	append("/mailbox", hisdir);
	buf[0] = 0;
	append("\nFrom ",buf);
	append(myname,buf);
	append(" ",buf);
	time(buf+256);
	append(ctime(buf+256),buf);
	if(!concat(file,hisdir,1,buf))
		printf("Can't send to `%s'\n",hisname);
	hisdir[n] = 0;
}

comp(n1, n2)
char n1[], n2[];
{
	int i;
	char c;

	i = 0;
	while((c=n1[i]) == n2[i])
	if(c == 0)
		return(1); else
		i++;
	return(0);
}

atoi(a)
char a[];
{
	int i, j;

	i = 0;
	j = 0;
	while(a[j])
		i = i*10 + a[j++] - '0';
	return(i);
}

append(tail, head)
char head[], tail[];
{
	int i, j;

	i = 0;
	while(head[i])
		i++;
	j = 0;
	while(head[i++] = tail[j++]);
}

concat(fn1,fn2,flg,header)
char	*fn1,*fn2;
int	flg;
char	*header;
{
	char	*flname;
	int	fn;
	int	tmp,n;
	static	buf[512];

	flname = "/tmp/mtm\0\0";
	for(flname[8] = 'a' ; flname[8] <= 'z' ; flname[8]++)
		if(stat(flname,buf) < 0)
			goto found;

notmp:
	printf("can't creat tmp file\n");
	exit();

found:
	if((tmp = creat(flname,0400)) < 0)
		goto notmp;
	if(!fn1)
		/*standard input file*/
		fn = 0;
	else
		if((fn = open(fn1,0)) < 0)	{
			close(tmp);
			unlink(flname);
			printf("can't open `%s'\n", fn1);
			exit();
			}
	if(flg)	{
		for(n = 0 ; header[n++] ;);
		write(tmp,header,n-1);
		}
	while(n = read(fn,buf,512))
		write(tmp,buf,n);
	if(flg)
		write(tmp,"\n",1);
	if(fn)
		close(fn);
	if(stat(fn2,buf) >= 0)	{
		if((fn = open(fn2,0)) < 0)	{
			close(tmp);
			unlink(flname);
			return(0);
			}
		while(n = read(fn,buf,512))
			write(tmp,buf,n);
		close(fn);
		}
	close(tmp);
	if((tmp = open(flname,0)) < 0)	{
		unlink(flname);
		goto notmp;
		}
	if((fn = creat(fn2,0666)) < 0)	{
		close(tmp);
		unlink(flname);
		return(0);
		}
	while(n = read(tmp,buf,512))
		write(fn,buf,n);
	close(fn);
	close(tmp);
	unlink(flname);
	return(1);
	}
