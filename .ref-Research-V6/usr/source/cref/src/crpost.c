/*int	mbuf[1024];	/*INSTR*/
int	ig	100;
int	esw;
int	initf	1;
char	ssp[270];
char	*sym[9]	{ssp,
		ssp + 30,
		ssp + 60,
		ssp + 90,
		ssp + 120,
		ssp + 150,
		ssp + 180,
		ssp + 210,
		ssp + 240,
		ssp + 270
		};

main(argc, argv)
	char	*argv[];
{
	auto	f,fct,file,i;

	if(argc < 2) {
		printf("Usage: crpost [-d] file1 file2 ...\n");
		exit();
	}

	if(*argv[1] == '-') {
		if(argv[1][1] == 'E') {
			fct = 3;
			esw = 1;
			goto on;
		}
		fct = argv[1][1] - '0';
		if((fct < 1) || (fct > 9)) {
			printf("-d: 0 < d < 10\n");
			exit();
		}
		if(argv[1][2] == 'x') {
			ig = fct-1;
		}
on:
		argv++;
		argc--;
	} else {
		fct = 3;
	}

	f = 0;
	while(++f < argc) {
		file = open(argv[f], 0);

		edf(file, fct);

		close(file);
	}
	flsh(0);

	exit();
}

int	lno	1;

edf(file, fct)
{
	auto	i,j,l;
	extern	lno,etext;
	char	brk,*buf,bufsp[150],line[150];
	char extern	*sym[];

/*	monitor(&main,&etext,&mbuf,1024);	/*INSTR*/
	for(i = 0; i < fct; i++)
		*sym[i] = 0200;
fields:
	l = -1;
	buf = &bufsp;

	for(i = 0; i < fct; ++i) {
		buf--;

swt:

		switch(*++buf = get(file)) {

			default:
				if(esw && *buf >= 'A' && *buf <= 'Z'
					&& i == 0)
					*buf =| 040;
				goto swt;

			case -1:
				*buf = ' ';
			case '\t':
				if(i == ig)	continue;
				brk = *buf;
				*buf = '\0';
				buf = &bufsp;
				if(comp(buf, sym[i])) {
					if(esw && i == 0) {
						line[0] = line[1] = '\t';
						l = 1;
						goto rest;
					}
					line[++l] = '\t';
					continue;
				} else {
					copy(buf, sym[i]);
					l =+ copy(buf, &line[++l]);
					line[--l] = brk;
					if(l < 8 && esw && i == 0)
						line[++l] = '\t';
					j = i;
					while(++j < fct)
						*sym[j] = 0200;
					continue;
				}

			case '\n':
				lno++;
				brk = *buf;
				*buf = '\0';
				buf = &bufsp;
				if(comp(buf, sym[i])) {
					goto fields;
				} else {
					copy(buf, sym[i]);
					l =+ copy(buf, &line[++l]);
					line[--l] = '\n';
					j = i;
					while(++j < fct)
						*sym[j] = 0;
					goto out;
				}

			case '\0':
				goto fexit;
		}
	}

rest:
	while((line[++l] = get(file)) != '\n')
		if(line[l] == '\0')	goto fexit;

	lno++;
out:
	if(*line != '\t') {
		put(0,"\n",1);
		lno++;
	}

	put(0,line,++l);

	goto fields;

fexit:
/*	monitor(0);	/*INSTR*/
	return(0);

}


copy(a, b)
	char	*a,*b;
{
	char	*c;

	b--;
	c = --a;
	while(*++b = *++a);
	return(a - c);
}

comp(a, b)
	char	*a, *b;
{
/*	printf("comp: %s %s\n",a,b); /*DEBUG*/
	a--;
	b--;
	while(*++a == *++b) {
		if(*a == '\0')	return(1);
	}
	return(0);
}


char	buf[512];
int	nread	1;

get(ifile) int ifile;
{

	char static *ibuf;

	if(--nread){
		return(*ibuf++);
	}

	if(nread = read(ifile,buf,512)){
		if(nread < 0)goto err;

		ibuf = buf;
		return(*ibuf++);
	}

	nread = 1;
	return(0);

err:
	nread = 1;
	printf("read error\n");
	return(0);

}


int	tp[1]	1;
int	optr[4];
char	bsp[512];

char	*obuf[1]	bsp;

int	nflush;

put(fil,string,n)
	char	*string;
{
	int	i;
	char	*o;

/*printf("%d %c %d\n",fil,*string,n);/*DEBUG*/

	string--;

	if((i = optr[fil] + n - 512) >= 0) {
		n =- i;
		o = &obuf[fil][optr[fil]] -1;
		while(--n >= 0)
			*++o = *++string;
		optr[fil] = 512;
		flsh(fil);
		n = i;
	}

	o = &obuf[fil][optr[fil]] - 1;
	optr[fil] =+ n;

	while(--n >= 0) {
		*++o = *++string;
	}
	return(0);
}

flsh(fil)
{
	extern	tp[];

	if(optr[fil] <= 0)	return(optr[fil]);

	nflush++;
	if(write(tp[fil],obuf[fil],optr[fil]) != optr[fil])
		return(-1);
	optr[fil] = 0;
	return(0);
}

