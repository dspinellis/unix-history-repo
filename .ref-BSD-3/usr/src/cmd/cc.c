#
# include <stdio.h>
# include <ctype.h>
# include <signal.h>
/* C command */

# define SBSIZE 10000
# define MAXINC 10
# define MAXFIL 100
# define MAXLIB 100
# define MAXOPT 100
char	tmp0[30];
char	*tmp1;
char	*tmp2;
char	*tmp3;
char	*tmp4;
char	*tmp5;
char	*outfile;
char *copy(),*setsuf();
# define CHSPACE 1000
char	ts[CHSPACE+50];
char	*tsa = ts;
char	*tsp = ts;
char	*av[50];
char	*clist[MAXFIL];
char	*llist[MAXLIB];
char	*alist[20];
int Wflag;
int dflag;
int	pflag;
int	sflag;
int	cflag;
int	eflag;
int	gflag;
int	exflag;
int	oflag;
int	proflag;
int	noflflag;
int	exfail;
char	*chpass ;
char	*npassname ;
char	pass0[40] = "/lib/ccom";
char	pass2[20] = "/lib/c2";
char	passp[20] = "/lib/cpp";
char	*pref = "/lib/crt0.o";

main(argc, argv)
char *argv[]; {
	char *t;
	char *savetsp;
	char *assource;
	char **pv, *ptemp[MAXOPT], **pvt;
	int nc, nl, i, j, c, f20, nxo, na;
	int idexit();

	i = nc = nl = f20 = nxo = 0;
	pv = ptemp;
	while(++i < argc) {
		if(*argv[i] == '-') switch (argv[i][1]) {
		default:
			goto passa;
		case 'S':
			sflag++;
			cflag++;
			break;
		case 'o':
			if (++i < argc) {
				char t;
				outfile = argv[i];
				if ((t=getsuf(outfile))=='c'||t=='o') {
					error("Would overwrite %s", outfile);
					exit(8);
				}
			}
			break;
		case 'O':
			oflag++;
			break;
		case 'p':
			proflag++;
			break;
		case 'g':
			gflag++;
			break;
		case 'W':	/* deprecated */
		case 'w':
			Wflag++;
			break;
		case 'E':
			exflag++;
		case 'P':
			pflag++;
			if (argv[i][1]=='P')
			fprintf(stderr, "(Warning): -P option obsolete\n");
			*pv++ = argv[i];
		case 'c':
			cflag++;
			break;

		case 'f':
			noflflag++;
			if (npassname || chpass)
				error("-f overwrites earlier option",0);
			npassname = "/lib/f";
			chpass = "12";
			break;

		case '2':
			if(argv[i][2] == '\0')
				pref = "/lib/crt2.o";
			else {
				pref = "/lib/crt20.o";
				f20 = 1;
			}
			break;
		case 'D':
		case 'I':
		case 'U':
		case 'C':
			*pv++ = argv[i];
			if (pv >= ptemp+MAXOPT)
				{
				error("Too many DIUC options", 0);
				--pv;
				}
			break;
		case 't':
			if (chpass)
				error("-t overwrites earlier option",0);
			chpass = argv[i]+2;
			if (chpass[0]==0)
				chpass = "012p";
			break;

		case 'B':
			if (npassname)
				error("-B overwrites earlier option", 0);
			npassname = argv[i]+2;
			if (npassname[0]==0)
				npassname = "/usr/c/o";
			break;

		case 'd':
			dflag++;
			strcpyn(alist, argv[i], 19);
			break;
		} else {
		passa:
			t = argv[i];
			if((c=getsuf(t))=='c' || c=='s'|| exflag) {
				clist[nc++] = t;
				if (nc>=MAXFIL)
					{
					error("Too many source files",0);
					exit(1);
					}
				t = setsuf(t, 'o');
			}
			if (nodup(llist, t)) {
				llist[nl++] = t;
				if (nl >= MAXLIB)
					{
					error("Too many object/library files",0);
					exit(1);
					}
				if (getsuf(t)=='o')
					nxo++;
			}
		}
	}
	if (gflag) oflag = 0;
	if (npassname && chpass ==0)
		chpass = "012p";
	if (chpass && npassname==0)
		npassname = "/usr/c/";
	if (chpass)
	for (t=chpass; *t; t++)
		{
		switch (*t)
			{
			case '0':
				strcpy (pass0, npassname);
				strcat (pass0, "ccom");
				continue;
			case '2':
				strcpy (pass2, npassname);
				strcat (pass2, "c2");
				continue;
			case 'p':
				strcpy (passp, npassname);
				strcat (passp, "cpp");
				continue;
			}
		}
	if (noflflag)
		pref = proflag ? "/lib/fmcrt0.o" : "/lib/fcrt0.o";
	else if (proflag)
		pref = "/lib/mcrt0.o";
	if(nc==0)
		goto nocom;
	if (pflag==0) {
		FILE *c;
		sprintf(tmp0,"/tmp/ctm%05.5da",getpid());
		while((c=fopen(tmp0, "r")) != NULL) {
			fclose(c);
			tmp0[9]++;
		}
		while((creat(tmp0, 0400))<0)
			tmp0[9]++;
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)	/* interrupt */
		signal(SIGINT, idexit);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)	/* terminate */
		signal(SIGTERM, idexit);
	(tmp1 = copy(tmp0))[13] = '1';
	(tmp2 = copy(tmp0))[13] = '2';
	(tmp3 = copy(tmp0))[13] = '3';
	if (oflag)
		(tmp5 = copy(tmp0))[13] = '5';
	if (pflag==0)
		(tmp4 = copy(tmp0))[13] = '4';
	pvt = pv;
	for (i=0; i<nc; i++) {
		if (nc>1) {
			printf("%s:\n", clist[i]);
			fflush(stdout);
		}
		if (getsuf(clist[i])=='s') {
			assource = clist[i];
			goto assemble;
		} else
			assource = tmp3;
		if (pflag)
			tmp4 = setsuf(clist[i], 'i');
		savetsp = tsp;
		av[0] = "cpp";
		av[1] = clist[i];
		av[2] = exflag ? "-" : tmp4;
		na = 3;
		for(pv=ptemp; pv <pvt; pv++)
			av[na++] = *pv;
		av[na++]=0;
		if (callsys(passp, av))
			{exfail++; eflag++;}
		av[1] =tmp4;
		tsp = savetsp;
		av[0]= "ccom";
		if (pflag || exfail)
			{
			cflag++;
			continue;
			}
		if(sflag)
			assource = tmp3 = setsuf(clist[i], 's');
		av[2] = tmp3;
		if(oflag)
			av[2] = tmp5;
		if (proflag) {
			av[3] = "-XP";
			av[4] = 0;
		} else
			av[3] = 0;
		if (gflag) {
			int i;

			i = av[3] ? 4 : 3;
			av[i++] = "-Xg";
			av[i] = 0;
		}
		if (Wflag) {
			int i;

			for (i = 3; i < 10 && av[i] != 0; i++)
				;
			av[i] = "-W";
			av[++i] = 0;
		}
				
		if (callsys(pass0, av)) {
			cflag++;
			eflag++;
			continue;
		}
		if (oflag) {
			av[0] = "c2";
			av[1] = tmp5;
			av[2] = tmp3;
			av[3] = 0;
			if (callsys(pass2, av)) {
				unlink(tmp3);
				tmp3 = assource = tmp5;
			} else
				unlink(tmp5);
		}
		if (sflag)
			continue;
	assemble:
		av[0] = "as";
		av[1] = "-o";
		av[2] = setsuf(clist[i], 'o');
		av[3] = assource;
		if (dflag) {
			av[4] = alist;
			av[5] = 0;
		} else
			av[4] = 0;
		cunlink(tmp1);
		cunlink(tmp2);
		cunlink(tmp4);
		if (callsys("/bin/as", av) > 1) {
			cflag++;
			eflag++;
			continue;
		}
	}
nocom:
	if (cflag==0 && nl!=0) {
		i = 0;
		av[0] = "ld";
		av[1] = "-X";
		av[2] = pref;
		j = 3;
		if (outfile) {
			av[j++] = "-o";
			av[j++] = outfile;
		}
		while(i<nl)
			av[j++] = llist[i++];
		if (gflag)
			av[j++] = "-lg";
		if(f20)
			av[j++] = "-l2";
		else {
			av[j++] = "/lib/libc.a";
			av[j++] = "-l";
		}
		av[j++] = 0;
		eflag |= callsys("/bin/ld", av);
		if (nc==1 && nxo==1 && eflag==0)
			cunlink(setsuf(clist[0], 'o'));
	}
	dexit();
}

idexit()
{
	eflag = 100;
	dexit();
}

dexit()
{
	if (!pflag) {
		cunlink(tmp1);
		cunlink(tmp2);
		if (sflag==0)
			cunlink(tmp3);
		cunlink(tmp4);
		cunlink(tmp5);
		cunlink(tmp0); 
	}
	exit(eflag);
}

error(s, x)
{
	fprintf(exflag?stderr:stdout , s, x);
	putc('\n', exflag? stderr : stdout);
	exfail++;
	cflag++;
	eflag++;
}




getsuf(as)
char as[];
{
	register int c;
	register char *s;
	register int t;

	s = as;
	c = 0;
	while(t = *s++)
		if (t=='/')
			c = 0;
		else
			c++;
	s -= 3;
	if (c<=14 && c>2 && *s++=='.')
		return(*s);
	return(0);
}

char *
setsuf(as, ch)
char as[];
{
	register char *s, *s1;

	s = s1 = copy(as);
	while(*s)
		if (*s++ == '/')
			s1 = s;
	s[-1] = ch;
	return(s1);
}

callsys(f, v)
char f[], *v[]; {
	int t, status;

	if ((t=vfork())==0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		fflush(stdout);
		_exit(100);
	} else
		if (t == -1) {
			printf("Try again\n");
			return(100);
		}
	while(t!=wait(&status));
	if ((t=(status&0377)) != 0 && t!=14) {
		if (t!=2)		/* interrupt */
			{
			printf("Fatal error in %s\n", f);
			eflag = 8;
			}
		dexit();
	}
	return((status>>8) & 0377);
}

char *
copy(as)
char as[];
{
	register char *otsp, *s;
	int i;

	otsp = tsp;
	s = as;
	while(*tsp++ = *s++);
	if (tsp >tsa+CHSPACE)
		{
		tsp = tsa = i = calloc(CHSPACE+50,1);
		if (i== -1){
			error("no space for file names");
			dexit(8);
			}
		}
	return(otsp);
}

nodup(l, os)
char **l, *os;
{
	register char *t, *s;
	register int c;

	s = os;
	if (getsuf(s) != 'o')
		return(1);
	while(t = *l++) {
		while(c = *s++)
			if (c != *t++)
				break;
		if (*t=='\0' && c=='\0')
			return(0);
		s = os;
	}
	return(1);
}

cunlink(f)
char *f;
{
	if (f==0)
		return(0);
	return(unlink(f));
}
