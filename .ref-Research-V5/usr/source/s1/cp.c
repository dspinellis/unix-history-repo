/*
 * cp oldfile newfile
 */

main(argc,argv)
char **argv;
{
	int buf[256];
	int fold, fnew, n, ct, tell;
	char *p1, *p2, *bp;
	int mode;

	tell = 0;
	if(argc == 4 && argv[1][0] == '-' && argv[1][1] == 't') {
		argc--;
		argv++;
		tell = 1;
	}
	if(argc != 3) {
		write(1, "Usage: cp oldfile newfile\n", 26);
		exit(1);
	}
	if((fold = open(argv[1], 0)) < 0) {
		write(1, "Cannot open old file.\n", 22);
		exit(1);
	}
	fstat(fold, buf);
	mode = buf[2];
	if((fnew = creat(argv[2], mode)) < 0){
		stat(argv[2],  buf);
		if((buf[2] & 060000) == 040000) {
			p1 = argv[1];
			p2 = argv[2];
			bp = buf;
			while(*bp++ = *p2++);
			bp[-1] = '/';
			p2 = bp;
			while(*bp = *p1++)
				if(*bp++ == '/')
					bp = p2;
			if((fnew = creat(buf, mode)) < 0) {
				write(1, "Cannot creat new file.\n", 23);
				exit(1);
			}
		} else {
			write(1, "Cannot creat new file.\n", 23);
			exit(1);
		}
	}
	while(n = read(fold,  buf,  512)) {
	if(n < 0) {
		write(1, "Read error\n", 11);
		exit(1);
	} else
		if(write(fnew, buf, n) != n){
			write(1, "Write error.\n", 13);
			exit(1);
		}
		ct++;
	}
	if(tell) {
		conf(ct, 6, buf);
		buf[3] = '\n';
		write(1, buf, 7);
	}
	exit(0);
}

conf(n,width,buf) 
	char	*buf;
{
	auto	i,a;

	i = width;
	while(i--)	buf[i] = ' ';

	buf[(a = n/10)?conf(a,--width,buf):--width] = n%10 + '0';

	return(++width);
}


