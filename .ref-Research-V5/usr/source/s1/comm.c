#define LB 250
int	one;
int	two;
int	three;

char	*ldr[3];

char	ib1[518];
char	ib2[518];
main(argc,argv)
	char	*argv[];
{
	extern	fout;
	int	l;
	char	lb1[LB],lb2[80];

	ldr[0] = "";
	ldr[1] = "\t";
	ldr[2] = "\t\t";
	if(argc > 1)  {
		if(*argv[1] == '-' && argv[1][1] != 0) {
			l = 1;
			while(*++argv[1]) {
				switch(*argv[1]) {
				case'1':
					if(!one) {
						one = 1;
						ldr[1][0] =
						ldr[2][l--] = '\0';
					}
					break;
				case '2':
					if(!two) {
						two = 1;
						ldr[2][l--] = '\0';
					}
					break;
				case '3':
					three = 1;
					break;
				default:
				printf("Illegal flag: %c\n",*argv[1]);
				exit();
				}
			}
			argv++;
			argc--;
		}
	}

	if(argc < 3) {
		printf("Argc = %d\n",argc);
		exit();
	}

	openfil(argv[1],ib1);
	openfil(argv[2],ib2);
	fout = dup(1);


	if(rd(ib1,lb1) < 0) {
		if(rd(ib2,lb2) < 0)	exit();
		copy(ib2,lb2,2);
	}
	if(rd(ib2,lb2) < 0)	copy(ib1,lb1,1);

	while(1) {

		switch(compare(lb1,lb2)) {

			case 0:
				wr(lb1,3);
				if(rd(ib1,lb1) < 0) {
					if(rd(ib2,lb2) < 0)	fexit();
					copy(ib2,lb2,2);
				}
				if(rd(ib2,lb2) < 0)	copy(ib1,lb1,1);
				continue;

			case 1:
				wr(lb1,1);
				if(rd(ib1,lb1) < 0)	copy(ib2,lb2,2);
				continue;

			case 2:
				wr(lb2,2);
				if(rd(ib2,lb2) < 0)	copy(ib1,lb1,1);
				continue;
		}
	}
}

rd(file,buf)
	char	*buf;
{

	register int i;
	i = 0;
	while((*buf = getc(file)) >=0) {
		if(*buf == '\n' || i > LB-2) {
			*buf = '\0';
			return(0);
		}
		i++;
		buf++;
	}
	return(-1);
}

wr(str,n)
	char	*str;
{

	switch(n) {

		case 1:
			if(one)	return;
			break;

		case 2:
			if(two)	return;
			break;

		case 3:
			if(three)	return;
	}
	printf("%s%s\n",ldr[n-1],str);
}

copy(ibuf,lbuf,n)
{
	do {
		wr(lbuf,n);
	} while(rd(ibuf,lbuf) >= 0);

	flush();
	exit();
}

compare(a,b)
	char	*a,*b;
{
	register char *ra,*rb;

	ra = --a;
	rb = --b;
	while(*++ra == *++rb)
		if(*ra == '\0')	return(0);
	if(*ra < *rb)	return(1);
	return(2);
}
fexit()
{
	flush();
	exit();
}

openfil(s,b)
char *s;
int *b;
{
	if(s[0]=='-' && s[1]==0)
		b[0] = 0;
	else if(fopen(s, b) == -1) {
		printf("Can't open %s\n", s);
		exit(1);
	}
}
