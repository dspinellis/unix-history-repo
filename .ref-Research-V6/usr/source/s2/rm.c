main(argc, argv)
char *argv[];
{
	char *arg;
	int fflg, rflg;

	fflg = 0;
	rflg = 0;
	while(--argc > 0) {

		arg = *++argv;
		if(arg[0] == '-') {
			if(arg[1] == 'f') {
				fflg++;
				continue;
			}
			if(arg[1] == 'r') {
				rflg++;
				continue;
			}
		}

		rm(arg, fflg, rflg);
	}
}

struct stbuf	{
	int dev;
	int inum;
	int mode;
	char nlink;
	char uid;
	char gid;
	char siz0;
	char siz1;
	int addr[8];
	int adate[2];
	int mdate[2];
	};

rm(arg, fflg, rflg)
char arg[];
{
	char *p;
	int buf[20];
	int i, b;

	if(stat(arg, buf)) {
		printf("%s: non existent\n", arg);
		return;
	}
	if((buf->mode & 060000) == 040000)	{
		if(rflg) {
			i = fork();
			if(i == -1) {
				printf("%s: try again\n", arg);
				return;
			}
			if(i) {
				while(wait() != i);
				return;
			}
			if(chdir(arg)) {
				printf("%s: cannot chdir\n", arg);
				exit();
			}
			p = 0;
			execl("/etc/glob", "glob", "rm", "-r",
				fflg? "-f": "*", fflg? "*": p, 0);
			printf("%s: no glob\n", arg);
			exit();
		}
		printf("%s: directory\n", arg);
		return;
	}

	if(!fflg) {

		if((getuid()&0377) == buf->uid)
			b = 0200; else
			b = 2;
		if((buf->mode & b) == 0 && ttyn(0) != 'x') {
			printf("%s: %o mode ", arg, buf->mode);
			i = b = getchar();
			i = b;
			while(b != '\n' && b != '\0')
				b = getchar();
			if(i != 'y')
				return;
		}
	}
	if(unlink(arg))
		printf("%s: not removed\n", arg);
}

putchar(c)
{
	write(1, &c, 1);
}

getchar()
{
	char c;

	if(read(0, &c, 1) != 1) return(0);
	return(c);
}
