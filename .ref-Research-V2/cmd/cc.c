/* C command */

char *tmp0 "//";
char *tmp1 "//";
char *tmp2 "//";
char *tmp3 "//";
char *tmp4 "//";
char ts[500];
char *tsp ts;
char *av[50];
char *clist[50];
char *llist[50];

main(argc, argv)
char *argv[]; {
	char *t, *pref;
	int nc, nl, cflag, i, j, c, f20;

	i = nc = nl = cflag = f20 = 0;
	pref = "/lib/crt0.o";
	while(++i < argc) {
		if(*argv[i] == '-')
			switch (argv[i][1]) {
				default:
					goto passa;
				case 'c':
					cflag++;
					break;
				case '2':
					pref = "/lib/crt20.o";
					f20 = 1;
			}
		else {
		passa:
			t = copy(argv[i]);
			if(getsuf(t)=='c') {
				clist[nc++] = t;
				t = setsuf(copy(t));
				if (dup(llist, t))
					continue;
			}
			llist[nl++] = t;
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
	(tmp4 = copy(tmp0))[8] = '4';
	for (i=0; i<nc; i++) {
		if (nc>1)
			printf("%s:\n", clist[i]);
		av[0] = "c0";
		av[1] = expand(clist[i]);
		if (av[1] == 0) {
			cflag++;
			continue;
		}
		av[2] = tmp1;
		av[3] = tmp2;
		av[4] = 0;
		if (callsys("/lib/c0", av)) {
			cflag++;
			continue;
		}
		av[0] = "c1";
		av[1] = tmp1;
		av[2] = tmp2;
		av[3] = tmp3;
		av[4] = 0;
		if(callsys("/lib/c1", av)) {
			cflag++;
			continue;
		}
		av[0] = "as";
		av[1] = "-";
		if(f20) {
			av[2] = "/lib/20.s";
			av[3] = tmp3;
			av[4] = 0;
		} else {
			av[2] = tmp3;
			av[3] = 0;
		}
		callsys("/bin/as", av);
		t = setsuf(clist[i]);
		unlink(t);
		if(link("a.out", t) | unlink("a.out")) {
			printf("move failed: %s\n", t);
			cflag++;
		}
	}
nocom:
	if (cflag==0 & nl!=0) {
		i = 0;
		av[0] = "ld";
		av[1] = pref;
		j = 2;
		while(i<nl)
			av[j++] = llist[i++];
		if(f20) {
			av[j++] = "-l2";
			av[j++] = 0;
		} else {
			av[j++] = "/lib/libc.a";
			av[j++] = "-l";
			av[j++] = 0;
		}
		if(f20) {
			callsys("/usr/lib/ld20", av);
		} else {
			callsys("/bin/ld", av);
		}
		if (nc==1 & nl==1)
			unlink(llist[0]);
	}
delfil:
	dexit();
}

dexit()
{
	unlink(tmp1);
	unlink(tmp2);
	unlink(tmp3);
	unlink(tmp4);
	unlink(tmp0);
	exit();
}

expand(file)
char *file;
{
	int ibuf1[259], ibuf2[259], obuf[259];
	char *hdrp, hdr[64];
	int nlflg, c;

	if (fopen(file, ibuf1)<0)
		return(file);
	if (getc(ibuf1) != '%') {
		close(ibuf1[0]);
		return(file);
	}
	ibuf1[1]++;		/* back up over % */
	ibuf1[2]--;
	if (fcreat(tmp4, obuf) < 0) {
		printf("Can't creat %s\n", tmp4);
		dexit();
	}
	nlflg = 1;
	while (c = getc(ibuf1)) {
		if (c == '%' & nlflg) {
			hdrp = hdr;
			while ((c = getc(ibuf1)) != '\n')
				if (c!=' ')
					*hdrp++ = c;
			*hdrp = '\0';
			if (hdr[0] == '\0')
				goto nohdr;
			if (fopen(hdr, ibuf2) < 0) {
				printf("Missing file %s\n", hdr);
				close(ibuf1[0]);
				return(0);
			}
			while (c = getc(ibuf2)) {
				if (nlflg)
					putc(1, obuf);  /* SOH */
				nlflg = c=='\n';
				putc(c, obuf);
			}
			close(ibuf2[0]);
			c = '\n';
		}
	nohdr:
		nlflg = c=='\n';
		putc(c, obuf);
	}
	fflush(obuf);
	close(obuf[0]);
	close(ibuf1[0]);
	return(tmp4);
}

getsuf(s)
char s[];
{
	int c;
	char t, *os;

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
	char *os;

	os = s;
	while(*s++);
	s[-2] = 'o';
	return(os);
}

callsys(f, v)
char f[], *v[]; {
	int t, status;

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
	if ((t=(status&0377)) != 0 & t!=14) {
		if (t!=12)		/* interrupt */
			printf("Fatal error in %s\n", f);
		dexit();
	}
	return((status>>8) & 0377);
}

copy(s)
char s[]; {
	char *otsp;

	otsp = tsp;
	while(*tsp++ = *s++);
	return(otsp);
}

dup(l, s)
char **l, s[]; {

	char *t, *os, c;

	os = s;
	while(t = *l++) {
		s = os;
		while(c = *s++)
			if (c != *t++)
				break;
		if (*t++ == '\0') return (1);
	}
	return(0);
}
lse
		if (t == -1) {
			printf("Try again\