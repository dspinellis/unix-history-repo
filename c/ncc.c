/* C command */

main(argc, argv)
char argv[][]; {
	extern callsys, printf, unlink, link, nodup;
	extern getsuf, setsuf, copy;
	extern tsp;
	extern tmp0, tmp1, tmp2, tmp3;
	char tmp0[], tmp1[], tmp2[], tmp3[];
	char glotch[100][], clist[50][], llist[50][], ts[500];
	char tsp[], av[50][], t[];
	auto nc, nl, cflag, i, j, c;

	tmp0 = tmp1 = tmp2 = tmp3 = "//";
	tsp = ts;
	i = nc = nl = cflag = 0;
	while(++i < argc) {
		if(*argv[i] == '-' & argv[i][1]=='c')
			cflag++;
		else {
			t = copy(argv[i]);
			if((c=getsuf(t))=='c') {
				clist[nc++] = t;
				llist[nl++] = setsuf(copy(t));
			} else {
			if (nodup(llist, t))
				llist[nl++] = t;
			}
		}
	}
	if(nc==0)
		goto nocom;
	tmp0 = copy("/tmp/ctm0a");
	while((c=open(tmp0, 0))>=0) {
		close(c);
		tmp0[9]++;
	}
	while((creat(tmp0, 012))<0)
		tmp0[9]++;
	intr(delfil);
	(tmp1 = copy(tmp0))[8] = '1';
	(tmp2 = copy(tmp0))[8] = '2';
	(tmp3 = copy(tmp0))[8] = '3';
	i = 0;
	while(i<nc) {
		if (nc>1)
			printf("%s:\n", clist[i]);
		av[0] = "c0";
		av[1] = clist[i];
		av[2] = tmp1;
		av[3] = tmp2;
		av[4] = 0;
		if (callsys("/usr/lib/c0", av)) {
			cflag++;
			goto loop;
		}
		av[0] = "c1";
		av[1] = tmp1;
		av[2] = tmp2;
		av[3] = tmp3;
		av[4] = 0;
		if(callsys("/usr/lib/c1", av)) {
			cflag++;
			goto loop;
		}
		av[0] = "as";
		av[1] = "-";
		av[2] = tmp3;
		av[3] = 0;
		callsys("/bin/as", av);
		t = setsuf(clist[i]);
		unlink(t);
		if(link("a.out", t) | unlink("a.out")) {
			printf("move failed: %s\n", t);
			cflag++;
		}
loop:;
		i++;
	}
nocom:
	if (cflag==0 & nl!=0) {
		i = 0;
		av[0] = "ld";
		av[1] = "/usr/lib/crt0.o";
		j = 2;
		while(i<nl)
			av[j++] = llist[i++];
		av[j++] = "-lc";
		av[j++] = "-l";
		av[j++] = 0;
		callsys("/bin/ld", av);
	}
delfil:
	dexit();
}
dexit()
{
	extern tmp0, tmp1, tmp2, tmp3;

	unlink(tmp1);
	unlink(tmp2);
	unlink(tmp3);
	unlink(tmp0);
	exit();
}

getsuf(s)
char s[];
{
	extern exit, printf;
	auto c;
	char t, os[];

	c = 0;
	os = s;
	while(t = *s++)
		if (t=='/')
			c = 0;
		else
			c++;
	s =- 3;
	if (c<=8 & c>2 & *s++=='.' & *s=='c')
		return('c');
	return(0);
}

setsuf(s)
char s[];
{
	char os[];

	os = s;
	while(*s++);
	s[-2] = 'o';
	return(os);
}

callsys(f, v)
char f[], v[][]; {

	extern fork, execv, wait, printf;
	auto t, status;

	if ((t=fork())==0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		exit(1);
	} else
		if (t == -1) {
			printf("Try again\n");
			return(1);
		}
	while(t!=wait(&status));
	if ((t=(status&0377)) != 0) {
		if (t!=9)		/* interrupt */
			printf("Fatal error in %s\n", f);
		dexit();
	}
	return((status>>8) & 0377);
}

copy(s)
char s[]; {
	extern tsp;
	char tsp[], otsp[];

	otsp = tsp;
	while(*tsp++ = *s++);
	return(otsp);
}

nodup(l, s)
char l[][], s[]; {

	char t[], os[], c;

	os = s;
	while(t = *l++) {
		s = os;
		while(c = *s++)
			if (c != *t++) goto ll;
		if (*t++ == '\0') return (0);
ll:;
	}
	return(1);
}

tsp;
tmp0;
tmp1;
tmp2;
tmp3;

