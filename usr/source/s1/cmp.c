int	ibuf1[259];
int	ibuf2[259];
int	eflg;
int	lflg	1;
int	line[2]	{0, 1};
int	chr[2] { -1, -1};

main(argc, argv)
char **argv;
{
	register c1, c2;
	char *arg;

	if(argc < 3)
		goto narg;
	arg = argv[1];
	if(arg[0] == '-' && arg[1] == 's') {
		lflg--;
		argv++;
		argc--;
	}
	arg = argv[1];
	if(arg[0] == '-' && arg[1] == 'l') {
		lflg++;
		argv++;
		argc--;
	}
	if(argc < 3)
		goto narg;
	arg = argv[1];
	if( arg[0]=='-' && arg[1]==0 )
		ibuf1[0] = dup(0);
	else if(fopen(arg, ibuf1) < 0)
		goto barg;
	arg = argv[2];
	if(fopen(arg, ibuf2) < 0)
		goto barg;

loop:
	if (++chr[1]==0)
		chr[0]++;
	c1 = getc(ibuf1);
	c2 = getc(ibuf2);
	if(c1 == c2) {
		if (c1 == '\n')
			if (++line[1]==0)
				line[0]++;
		if(c1 == -1) {
			if(eflg)
				exit(1);
			exit(0);
		}
		goto loop;
	}
	if(lflg == 0)
		exit(1);
	if(c1 == -1) {
		arg = argv[1];
		goto earg;
	}
	if(c2 == -1)
		goto earg;
	if(lflg == 1) {
		printf("%s %s differ: char %s, line ", argv[1], arg,
			locv(chr[0], chr[1]));
		printf("%s\n", locv(line[0], line[1]));
		exit(1);
	}
	eflg = 1;
	printf("%5s %3o %3o\n", locv(chr[0], chr[1]), c1, c2);
	goto loop;

narg:
	printf("arg count\n");
	exit(2);

barg:
	printf("cannot open %s\n", arg);
	exit(2);

earg:
	printf("EOF on %s\n", arg);
	exit(1);
}

putchar(c)
{

	write(1, &c, 1);
}
