char	buf[100];

main(argc, argv)
char **argv;
{
	register i;
	register char *c1, *c2;

	if(argc < 3) {
		write(2, "arg count\n", 10);
		exit();
	}
	argc--;
	c1 = buf;
	c2 = argv[argc];
	while(*c1++ = *c2++);
	c1[-1] = '/';
	*c1++ = '.';
	*c1 = '\0';
	for(i=1; i<argc; i++) {
		if(fork()==0) {
			execl("/bin/cp", "cp", argv[i], buf);
			exit();
		}
		wait();
	}
}
