/* fortran command */

char	ts[500];
char	*tsp ts;
char	*av[50];
char	*llist[50];

main(argc, argv)
char *argv[]; {
	char *t;
	int nl, cflag, i, j;

	llist[0] = 0;
	i = nl = cflag = 0;
	while(++i < argc) {
		if (getsuf(t=copy(argv[i])) == 'f') {
			printf("%s:\n", t);
			av[0] = "fc";
			av[1] = t;
			av[2] = 0;
			if(callsys("/usr/fort/fc1", av) == 0) {
				av[0] = "as";
				av[1] = "-";
				av[2] = "f.tmp1";
				av[3] = 0;
				callsys("/bin/as", av);
				setsuf(t);
				unlink(t);
				if(link("a.out", t) | unlink("a.out")) {
					printf("move failed: %s\n", t);
					return;
				}
			} else {
				cflag++;
				goto loop;
			}
		} else if (*t == '-' & t[1] =='c') {
			cflag++;
			goto loop;
		}
		if (nodup(llist, t)) {
			llist[nl++] = t;
			llist[nl] = 0;
		}
loop:;
	}
	unlink("f.tmp1");
	if (cflag | nl==0) return;
	i = 0;
	av[0] = "ld";
	av[1] = "/lib/fr0.o";
	j = 2;
	while(i<nl)
		av[j++] = llist[i++];
	av[j++] = "-lf";
	av[j++] = "/lib/filib.a";
	av[j++] = "-l";
	av[j++] = 0;
	callsys("/bin/ld", av);
}

getsuf(s)
char s[]; {
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
	if (c<=8 & c>2 & *s++=='.' & *s=='f')
		return('f');
	return(0);
}

setsuf(s)
char s[]; {
	while(*s++);
	s[-2] = 'o';
}

callsys(f, v)
char f[], *v[]; {
	int t, status;

	if ((t=fork())==0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		exit(-1);
	} else
		if (t == -1) {
			printf("Try again\n");
			exit(-1);
	}
	while(t!=wait(&status));
	if ((status&0377) != 0)
		exit(-1);
	return((status>>8) & 0377);
}

copy(s)
char s[]; {
	char *otsp;

	otsp = tsp;
	while(*tsp++ = *s++);
	return(otsp);
}

nodup(l, s)
char *l[], s[]; {
	char *t, *os, c;

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
(t=fork())==0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		exit(-1);
	} else
		if (t == -1) {
			printf("Try again\n");
			exit(-1);
	}
	while(t!=wait(&status));
	if ((status&0377) != 0)
		exit(-1);
	retur