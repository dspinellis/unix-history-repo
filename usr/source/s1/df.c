char	*dargv[]
{
	0,
	"/dev/rk2",
	"/dev/rp0",
	0
};
struct
{
	char	*s_isize;
	char	*s_fsize;
	int	s_nfree;
	int	s_free[100];
	int	s_ninode;
	int	s_inode[100];
	char	s_flock;
	char	s_ilock;
	char	s_fmod;
	int	time[2];
	int	pad[50];
} sblock;

int	fi;

main(argc, argv)
char **argv;
{
	int i;

	if(argc <= 1) {
		for(argc = 1; dargv[argc]; argc++);
		argv = dargv;
	}

	for(i=1; i<argc; i++) {
		if(argc > 1)
			printf("%s ", argv[i]);
		dfree(argv[i]);
	}
}

dfree(file)
char *file;
{
	int i;

	fi = open(file, 0);
	if(fi < 0) {
		printf("cannot open %s\n", file);
		return;
	}
	sync();
	bread(1, &sblock);
	i = 0;
	while(alloc())
		i++;
	printf("%l\n", i);
	close(fi);
}

alloc()
{
	int b, i, buf[256];

	i = --sblock.s_nfree;
	if(i<0 || i>=100) {
		printf("bad free count\n");
		return(0);
	}
	b = sblock.s_free[i];
	if(b == 0)
		return(0);
	if(b<sblock.s_isize+2 || b>=sblock.s_fsize) {
		printf("bad free block (%l)\n", b);
		return(0);
	}
	if(sblock.s_nfree <= 0) {
		bread(b, buf);
		sblock.s_nfree = buf[0];
		for(i=0; i<100; i++)
			sblock.s_free[i] = buf[i+1];
	}
	return(b);
}

bread(bno, buf)
{
	int n;
	extern errno;

	seek(fi, bno, 3);
	if((n=read(fi, buf, 512)) != 512) {
		printf("read error %d\n", bno);
		printf("count = %d; errno = %d\n", n, errno);
		exit();
	}
}
