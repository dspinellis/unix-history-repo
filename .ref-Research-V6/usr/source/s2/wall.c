char	mesg[3000];
int	msize;
struct
{
	char	name[8];
	char	tty[2];
	int	time[2];
	int	junk;
} utmp[50];

main(argc, argv)
char *argv[];
{
	register i, *p;
	int f;

	f = open("/etc/utmp", 0);
	if(f < 0) {
		printf("utmp?\n");
		exit();
	}
	read(f, utmp, sizeof utmp);
	close(f);
	f = 0;
	if(argc >= 2) {
		f = open(argv[1], 0);
		if(f < 0) {
			printf("%s?\n", argv[1]);
			exit();
		}
	}
	while((i = read(f, &mesg[msize], sizeof mesg - msize)) > 0)
		msize =+ i;
	close(f);
	for(i=0; i<sizeof utmp/sizeof utmp[0]; i++) {
		p = &utmp[i];
		if(p->tty[0] == 0)
			continue;
		sleep(1);
		sendmes(p->tty[0]);
	}
}

sendmes(tty)
{
	register i;
	register char *s;

	i = fork();
	if(i == -1) {
		printf("try again\n");
		return;
	}
	if(i)
		return;
	s = "/dev/ttyx";
	s[8] = tty;
	i = open(s, 1);
	if(i < 0) {
		printf("cannot open tty%c\n", tty);
		exit();
	}
	close(1);
	dup(i);
	printf("Broadcast Message ...\n\n");
	write(1, mesg, msize);
	exit();
}
