#
# include "econs.h"
# include "ecmn.h"
/*int	mbuf[1024];	/*INSTR*/
/*int	tbuf[12];	/*INSTR*/

int	(*acts[])()	{0,
			&coll,
			&save,
			&out,
			&error,
			&hyphen,
			&pno
			};

char	*tmp[2]	{"/tmp/crt0a",
		"/tmp/crt1a"
		};

char	*ignonl	"/usr/lib/eign";
char	*gtab	"/usr/lib/etab";

main(argc,argv)
	char	*argv[];
{
	auto	i,j,tm1,tm2,tm3;
	char	*fn,*av[8];
/*	extern	etext;	/*INSTR*/

/*	monitor(&main,&etext,&mbuf,1024);	/*INSTR*/

	if(*argv[1] == '-') {
		j = flags(argv);
		argv =+ j;
		argc =- j;
	}

	init();

	i = 0;
	if(argc == 1) {
		*ibuf1 = 0;
		curfl = 2;
		curf[0] = '_';
		curf[1] = '\t';
		goto pipe;
	}
	while(++i < argc) {
		curs[4] = '\t';

		if(fopen(argv[i],ibuf1) < 0) {
			printf("Can't open %s\n",argv[i]);
			dexit();
		}

		curfl = 0;
		while((curf[curfl] = *argv[i]++) != 0 && curfl <=8)
			if(curf[curfl++] == '/')	curfl = 0;
		curf[curfl++] = '\t';
		if(curfl == 8) curf[8] = -1;

pipe:
		ibuf = ibuf1;
		lno = 1;

		driver();

		close(file);
	}
	flsh(0);
	close(tp[0]);


/*	monitor(0);	/*INSTR*/
/*	dexit();	/*INSTR*/

/*	times(tbuf);	/*INSTR*/
/*	tm1 = tbuf[0]/6;	/*INSTR*/
/*	tm2 = tbuf[1]/6;	/*INSTR*/
/*	printf("Prep:  %d  %d\n", tm1, tm2);	/*INSTR*/

/*	exit();	/*DEBUG*/
	fn = "/bin/sort";
	av[0] = "sort";
	av[1] = tmp[0];
	av[2] = "-o";
	av[3] = tmp[0];
	av[4] = 0;

	callsys(fn, av);

/*	times(tbuf);	/*INSTR*/
/*	tm1 = tbuf[3]/6;	/*INSTR*/
/*	tm2 = tbuf[5]/6;	/*INSTR*/
/*	printf("Sort:  %d  %d\n", tm1, tm2);	/*INSTR*/

	if(usw) {
		fn = "/usr/bin/upost";
		av[0] = "upost";
		i = 0;
	} else if(count) {
		fn = "count";
		av[0] = "count";
		i = 0;
	} else {
		fn = "/usr/bin/crpost";
		av[0] = "crpost";
		av[1] = "-E";
		i = 1;
	}
	av[++i] = tmp[0];
	av[++i] = 0;

	callsys(fn,av);

/*	times(tbuf);	/*INSTR*/
/*	tm1 = tbuf[3]/6 - tm1;	/*INSTR*/
/*	tm2 = tbuf[5]/6 - tm2;	/*INSTR*/
/*	printf("Post:  %d  %d\n", tm1, tm2);	/*INSTR*/

	dexit();
}

driver()
{
	auto	p;

top:
	l = -1;
	while((c = line[++l] = getc(ibuf)) != -1) {
/*	printf("driver: c = %o l = %d\n",c,l); /*DEBUG*/
		if(l >= 299) {
			printf("Line too long: %d.\n",lno);
			dexit();
		}

		if(c & 0200) {
			printf("Illegal character: %o line %d\n",c,lno);
			dexit();
		}

		if(fl) {
			if((*flag[fl])())
				continue;
		}

/*printf("cs = %d cc = %c ca = %d\n",cs,c,tab[cs].cl[c]);	/*DEBUG*/

		if(p = tab[cs].cl[c])
			(*acts[p])();
		continue;
	}
	if(ibuf == ibuf1)	return;
	ibuf = ibuf1;
	goto top;

}

init()
{
	int	b[3];
	auto	fi,i;
	extern	coll(),save(),out(),asym(),asw(),csym(),csw();
	extern	incl(),decl(),sk(),sk2();
	extern	dexit();

	ibuf1 = &ib1;



	if((fi = open(gtab,0)) < 0) {
		printf("Cannot open grammar table; see lem\n");
		dexit();
	}

	i = -1;
	while(++i < NUMS)
		if(read(fi,tab[i].cl,256) < 256) {
			printf("Bad grammar table; see lem\n");
			dexit();
		}

	close(fi);



	if(signal(1,1) != 1)	signal(1,&dexit);
	if(signal(2,1) != 1)	signal(2,&dexit);
	if(signal(3,1) != 1)	signal(3,&dexit);
	while((tp[1] = creat(tmp[1],0)) < 0)
		tmp[1][9]++;
	close(tp[1]);
	tmp[0][9] = tmp[1][9];
	tp[0] = creat(tmp[0],CREATC);

	if(count)	return;

	itab.hptr = &ipsp;
	itab.symt = &issp;
	itab.hsiz = PTRI;
	itab.ssiz = CHARI;
	itab.nsym = 0;
	itab.curb = 1;

	if((fi = open(ignonl,0)) < 0) {
		printf("Cannot open ignore/only file.\n");
		dexit();
	}
	if((read(fi,b,6) == 6) && (b[0] == 0100200)) {
		if(read(fi,itab.hptr,b[1]) < b[1]) {
			printf("Cannot read ignore/only file.\n");
			dexit();
		}
		if(read(fi,itab.symt,b[2]) < b[2]) {
			printf("Cannot read ignor/only file.\n");
			dexit();
		}
		close(fi);
	} else {
		close(fi);
		compile();
	}
	return;
}

error(a)
{
	printf("Error %d\n",a);
	dexit();
}

dexit()
{
	extern	nflush;

/*	printf("nflush = %d\n",nflush);	/*DEBUG*/
	if(tp[0] > 0 && utmp == 0) {
		unlink(tmp[0]);
		unlink(tmp[1]);
	}
	exit();
}

callsys(f,v)
	char	f[],*v[];
{
	int	t,status,i;

	if((t = fork()) == 0) {
		for(i = 1; i <= 12; i++)	signal(i,0);
		execv(f,v);
		printf("Can't find %s\n",f);
		exit(1);
	} else {
		if(t == -1) {
			printf("Try again\n");
			return(1);
		}
	}

	while(t != wait(&status));
/*	printf("Status = %o, %s\n",status,f);	/*DEBUG*/
	if((t = (status & 0377)) != 0) {
		if(t != 2) {
			printf("Fatal error in %s\n",f);
			printf("t = %d\n",t);
		}
		dexit();
	}
	return((status>>8) & 0377);
}

flags(argv)
	char	*argv[];
{
	int	j,xx;
	char	*ap;

	j = 1;
	ap = argv[1];
	while(*++ap != '\0') {
		switch(*ap) {

			default:
				printf("Unrecognized flag: %c\n",*ap);
				dexit();

			case 'c':
				count = 1;
				continue;

			case 'i':	/* Ignore file */
				if(!xx) {
					xx = 1;
					only = 0;
					ignonl = argv[++j];
				}
				continue;

			case 'o':	/*only file*/
				if(!xx) {
					xx = 1;
					only = 1;
					ignonl = argv[++j];
				}
				continue;

			case 'p':
				page = 1;
				continue;

			case 't':
				utmp = argv[++j];
				tmp[0] = argv[j];
				continue;

			case 'u':	/* Unique symbols only */
				usw = 1;
				continue;

			case 'w':	/* Word list only */
				word = 1;
				continue;

		}
	}
	return(j);
}


compile()
{
	char	buf[40],*b;
	int	i,v;

	fopen(ignonl,ibuf1);

	b = buf - 1;
	while((*++b = getc(ibuf1)) != -1) {
		if(*b == '\n') {
			*b = '\0';
			search(buf,b - buf,&itab,1);
			b = buf - 1;
		} else {
			if(*b == '\t') {
				v = 0;
				while((i = getc(ibuf1)) != -1) {
					if(i == '\n')	break;
					v = v*10 + (i - '0');
				}
				search(buf,b - buf,&itab,v);
				b = buf - 1;
			} else {
				if((b - buf) > 39) {
					printf("Ignore/only symbol too long.\n");
					dexit();
				}
			}
		}
	}
	close(ibuf1);
	return;
}
