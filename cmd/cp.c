main(argc,argv)
char **argv;
{
char buf[512];
int fold, fnew, n;
char *p1, *p2, *bp;
int mode;
	if(argc != 3) {
		write(1,"Usage: cp oldfile newfile\n",26);
		exit();
	}
	if((fold = open(argv[1],0)) < 0){
		write(1,"Cannot open old file.\n",22);
		exit();
	}
	fstat(fold,buf);
	mode = buf[2] & 037;
	if((fnew = creat(argv[2],mode)) < 0){
		stat(argv[2], buf);
		if((buf[3] & 0100) != 0){
			p1 = argv[1] - 1;
			p2 = argv[2] - 1;
			bp = buf - 1;
			while(*++bp = *++p2);
			*bp = '/';
			p2 = bp;
			while(*++bp = *++p1)
				if(*bp == '/')
					bp = p2;
			if((fnew = creat(buf,mode)) < 0){
				write(1,"Cannot creat new file.\n",23);
				exit();
			}
		}else{
		write(1,"Cannot creat new file.\n",23);
		exit();
		}
	}
	while(n = read(fold, buf, 512))
	if(n < 0){
		write(1,"Read error\n",11);
		exit();
	}else
		if(write(fnew,buf,n) != n){
			write(1,"Write error.\n",13);
			exit();
		}
	fstat(fnew,buf);
	exit();
}

			p2 = argv[2] - 1;
			bp = buf - 1;
			while(*++bp = *++p2);
			*bp = '/';
			p2 = bp;
			while(*++bp