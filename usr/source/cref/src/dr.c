#
# include "../mcons.h"
# include "../ccmn.h"
/*int	mbuf[1024];	/*INSTR*/
/*int	tbuf[36];	/*INSTR*/

int	(*acts[])()	{0,
			&coll,
			&save,
			&out,
			&asym,
			&asw,
			&csym,
			&csw,
			&incl,
			&decl,
			&sk2,
			&sk,
			&tabs,
			&semi
			};

char	*tmp[5]	{"/tmp/crt0a",
		"/tmp/crt1a",
		"/tmp/crt2a",
		"/tmp/crt3a",
		"/tmp/crt4a"
		};

char	*ignonl	"/usr/lib/aign";
char	*gtab	"/usr/lib/atab";

main(argc,argv)
	char	*argv[];
{
	auto	i,j,tm1,tm2,tm3;
	char	*fn,*av[8];
/*	extern	etext;	/*INSTR*/

/*	monitor(&main,&etext,&mbuf,1024);	/*INSTR*/
	if(argc < 2) {
		printf("Usage: cref [-aceilosux] file1 ...\n");
		exit();
	}

	lbuf[4] = '\t';
	if(*argv[1] == '-') {
		j = flags(argv);
		argv =+ j;
		argc =- j;
	}

	init();

	i = 0;
	while(++i < argc) {
		curs[0] = '_';
		curs[1] = '\t';
		curs[4] = '\t';
		cursl = 2;

		if(fopen(argv[i],ibuf1) < 0) {
			printf("Can't open %s\n",argv[i]);
			dexit();
		}
		ibuf = ibuf1;

		curf[0] = '\t';
		curfl = 1;
		while((curf[curfl] = *argv[i]++) != 0 && curfl <= 8)
			if(curf[curfl++] == '/')	curfl = 1;

		curf[curfl++] = '\t';
		if(curfl == 10)	curf[9] = -1;
/*		printf("%s %d\n",curf,curfl);/*DEBUG*/

		lno = 1;

		driver();

		close(file);
	}
	for(j = 0; j < 4;) {
		flsh(j,0);
		close(tp[j++]);
	}

/*	monitor(0);	/*INSTR*/
/*	dexit();	/*INSTR*/

/*	times(tbuf);	/*INSTR*/
/*	tm1 = tbuf[15]/6;	/*INSTR*/
/*	tm2 = tbuf[19]/6;	/*INSTR*/
/*	tm3 = tbuf[23]/6;	/*INSTR*/
/*	printf("Prep: %d  %d  %d\n",tm1,tm2,tm3);	/*INSTR*/

	if(utmp)	exit();
	fn = "/bin/sort";
	av[0] = "sort";
	av[1] = "-a";
	av[3] = "-o";
	av[5] = 0;
	for(i = 0; i < 4; i++) {
		av[4] = av[2] = tmp[i];
		callsys(fn,av);
		if(utmp)	break;
	}

/*	times(tbuf);	/*INSTR*/
/*	tm1 = tbuf[27]/6;	/*INSTR*/
/*	tm2 = tbuf[31]/6;	/*INSTR*/
/*	tm3 = tbuf[35]/6;	/*INSTR*/
/*	printf("Sort: %d  %d  %d\n",tm1,tm2,tm3);	/*INSTR*/

	if(usw) {
		fn = "/usr/bin/upost";
		av[0] = "upost";
		i = 0;
	} else {
		fn = "/usr/bin/crpost";
		av[0] = "crpost";
		av[1] = cross? "-4x": "-3";
		i = 1;
	}
	j = -1;
	while(++j < 4) {
		av[++i] = tmp[j];
		if(utmp)	break;
	}
	av[++i] = 0;

	callsys(fn,av);

/*	times(tbuf);	/*INSTR*/
/*	tm1 = tbuf[27]/6 - tm1;	/*INSTR*/
/*	tm2 = tbuf[31]/6 - tm2;	/*INSTR*/
/*	tm3 = tbuf[35]/6 - tm3;	/*INSTR*/
/*	printf("Post: %d  %d  %d\n",tm1,tm2,tm3);	/*INSTR*/

	dexit();
}

driver()
{
	auto	p;

top:
	l = -1;
	while((c = line[++l] = getc(ibuf)) != -1) {
		if(l >= 131) {
			printf("Line too long: %d %s\n",lno,curf);
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
	ibuf2 = &ib2;

	xtab.hptr = &xpsp;
	xtab.symt = &xssp;
	xtab.hsiz = PTRX;
	xtab.ssiz = CHARX;
	xtab.nsym = 0;
	xtab.curb = 1;

	itab.hptr = &ipsp;
	itab.symt = &issp;
	itab.hsiz = PTRI;
	itab.ssiz = CHARI;
	itab.nsym = 0;
	itab.curb = 1;

	if((fi = open(gtab,0)) < 0) {
		printf("Cannot open grammar table; see lem\n");
		dexit();
	}

	i = -1;
	while(++i < NUMS)
		if(read(fi,tab[i].cl,256) < 256) {
			printf("Bad grammar table.\n");
			dexit();
		}

	close(fi);


	if((fi = open(ignonl,0)) < 0) {
		printf("Cannot open ignore/only file: %s\n",ignonl);
		dexit();
	}
	if((read(fi,b,6) == 6) && (b[0] == 0100200)) {
		if(read(fi,itab.hptr,b[1]) < b[1]) {
			printf("Cannot read ignore/only file: %s\n",ignonl);
			dexit();
		}
		if(read(fi,itab.symt,b[2]) < b[2]) {
			printf("Cannot read ignore/only file: %s\n",ignonl);
			dexit();
		}
		close(fi);
	} else {
		close(fi);
		compile();
	}

	if((signal(1,1) & 1) == 0)	signal(1,&dexit);
	if((signal(2,1) & 1) == 0)	signal(2,&dexit);
	if((signal(3,1) & 1) == 0)	signal(3,&dexit);
/*	signal(4,&dexit);
	signal(5,&dexit);
	signal(6,&dexit);
	signal(7,&dexit);
	signal(8,&dexit);
	signal(10,&dexit);
	signal(11,&dexit);
	signal(12,&dexit);
*/
	if(utmp == 0) {
		while((tp[4] = creat(tmp[4],0)) < 0)
			tmp[4][9]++;
		close(tp[4]);
		tmp[0][9] = tmp[4][9];
		tmp[1][9] = tmp[4][9];
		tmp[2][9] = tmp[4][9];
		tmp[3][9] = tmp[4][9];
		tp[0] = creat(tmp[0],CREATC);
		tp[1] = creat(tmp[1],CREATC);
		tp[2] = creat(tmp[2],CREATC);
		tp[3] = creat(tmp[3],CREATC);
	} else {
		if((tp[0] = creat(utmp,CREATC)) < 0) {
		printf("Can't create user's temp file.\n");
			exit();
		}
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
		unlink(tmp[2]);
		unlink(tmp[3]);
		unlink(tmp[4]);
	}
	exit();
}

callsys(f,v)
	char	f[],*v[];
{
	int	t,status,i;

	if((t = fork()) == 0) {
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

			case '1':	/* Symbol first */
				order = 1;
				continue;

			case '2':	/* Current file first */
				order = 2;
				continue;

			case '3':	/* Current symbol first */
				order = 3;
				continue;

			case 'a':	/* Assembler */
				cflag = 0;
				continue;

			case 'c':	/* C */
				gtab = "/usr/lib/ctab";
				if(!xx)
					ignonl = "/usr/lib/cign";
				cflag = 1;
				continue;

			case 'e':	/* English */
				gtab = "/usr/lib/etab";
				if(!xx)
					ignonl = "/usr/lib/eign";
				continue;

			case 'i':	/* Ignore file */
				if(!xx) {
					xx = 1;
					only = 0;
					ignonl = argv[++j];
				}
				continue;

			case 'l':	/* Line numbers in col. 3 */
				cross = 0;
				continue;

			case 'o':	/* Only file */
				if(!xx) {
					xx = 1;
					only = 1;
					ignonl = argv[++j];
				}
				continue;

			case 's':	/* Symbols in col. 3 */
				cross = 1;
				continue;

			case 't':
				utmp = argv[++j];
				tmp[0] = argv[j];
				continue;

			case 'u':	/* Unique symbols only */
				usw = 1;
				continue;

			case 'x':	/* C externals */
				xsw = 1;
				gtab = "/usr/lib/ctab";
				if(!xx)
					ignonl = "/usr/lib/cign";
				cflag = 1;
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
					printf("Ignore/only symbol too long");
					dexit();
				}
			}
		}
	}
	close(ibuf1);
	return;
}
