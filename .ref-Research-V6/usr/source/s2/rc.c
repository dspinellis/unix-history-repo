/* Ratfor-Fortran command */

extern int fin, fout;
char	ts[1000];
char	*tsp	ts;
char	*av[50];
char	*rlist[50];
int	nr	0;
char	*llist[50];
int	nl	0;
int	nxo	0;
int	bdcount	0;	/* count block data files generated */
int	rflag;
int	dflag	0;
int	vflag	1;
int	fflag;
int	cflag;
char	*complr "/usr/fort/fc1";
char	*ratfor "/usr/lib/ratfor";

main(argc, argv)
char *argv[]; {
	char *t;
	int i, j, c;
	int dexit();

	for(i=0; ++i < argc; ) {
		if(*argv[i] == '-')
			switch (argv[i][1]) {
				default:
					goto passa;
				case 'd':
					dflag = 1;
					break;
				case 'v':
					vflag = 0;
					break;
				case 'r':
					rflag = fflag = cflag = 1;
					break;
				case 'f':
					fflag = 1;
					break;
				case 'c':
					cflag = 1;
					break;
				case '2':
					complr = "/usr/fort/fc2";
					break;
			}
		else {
	   passa:
			t = argv[i];
			if( (c=getsuf(t))=='r' )
				ratcomp(t);
			else if( c=='f')  {
				fortcomp(t);
				llenter(setsuf(copy(t),'o'));
			}
			else
				llenter(copy(t));
		}
	}
	if(rflag)
		dexit();
	if ((signal(2, 1) & 01) == 0)
		signal(2, &dexit);
	if(dflag)
		printf("cflag=%d, nl=%d\n", cflag, nl);
	if (cflag==0 && nl!=0) {
		i = 0;
		av[0] = "ld";
		av[1] = "-x";
		av[2] = "/lib/fr0.o";
		j = 3;
		while(i<nl)
			av[j++] = llist[i++];
		av[j++] = "-lf";
		av[j++] = "/lib/filib.a";
		av[j++] = "-l";
		av[j++] = 0;
		callsys("/bin/ld", av);
	}
	dexit();
}

dexit()
{
	int i;
	cunlink("ratjunk");
	cunlink("f.tmp1");
	exit(0);
}


ratcomp(s) char *s; {
	int i,j,t,nerr,status;
	nr = 0;
	if(vflag)
		printf("%s:\n",s);
	av[0] = ratfor;
	av[1] = s;
	av[2] = 0;
	if( (t=fork())==0 ){
		close(1);
		fout = creat("ratjunk", 0666);
		execv(ratfor, av);
		fout = 2;
		error("can't ratfor\n");
		exit(1);
	}
	while( t!=wait(&status) );
	if( (t=(status&0377)) != 0 && t!=14 )
		dexit(1);
	t = (status>>8) & 0377;
	if( t )
		return(++cflag);
	splitup();
	nerr=0;
	for(i=0; i<nr; i++){
		if( vflag ) printf("   ");
		if( fortcomp(rlist[i]) )
			nerr++;
	}
	if( nerr )
		return(1);
	av[0] = "ld";
	av[1] = "-r";
	av[2] = "-x";
	j = 3;
	for(i=0; i<nr; i++)
		av[j++] = rlist[i];
	av[j] = 0;
	callsys("/bin/ld", av);
	t = setsuf(copy(s),'o');
	if( move("a.out", t) )
		cflag++;
	llenter(t);
	for(i=0; i<nr; i++) {
		if( nodup(llist,rlist[i]) )
			cunlink(rlist[i]);
		if( fflag==0 )
			cunlink(setsuf(rlist[i],'f'));
	}
}

fortcomp(s) char *s; {
	int t;
	if( vflag ) printf("%s:\n", s);
	av[0] = complr;
	av[1] = s;
	av[2] = 0;
	if( callsys(complr, av) )
		return(++cflag);
	av[0] = "as";
	av[1] = "-";
	av[2] = "f.tmp1";
	av[3] = 0;
	callsys("/bin/as", av);
	t = setsuf(s, 'o');
	if( move("a.out", t) )
		return(++cflag);
	return(0);
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
	if (c<=14 && c>2 && *s++=='.')
		return(*s);
	return(0);
}

setsuf(s, ch)
char s[];
{
	char *os;

	os = s;
	while(*s++);
	s[-2] = ch;
	return(os);
}

move(s,t) char *s, *t; {
	cunlink(t);
	if(link(s, t) || cunlink(s)) {
		printf("move failed: %s\n", t);
		return(1);
	}
	return(0);
}

callsys(f, v)
char f[], *v[]; {
	int i, t, status;

	if(dflag){
		for(i=0; v[i]; i++)
			printf("%s ", v[i]);
		putchar('\n');
	}
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
	if ((t=(status&0377)) != 0 && t!=14) {
		if (t!=2)		/* interrupt */
			printf("Fatal error in %s\n", f);
		dexit();
	}
	t = (status>>8) & 0377;
	if(dflag && status != 0)
		printf("status = %d\n", t);
	return(t);
}

copy(s)
char s[]; {
	char *otsp;

	otsp = tsp;
	while(*tsp++ = *s++);
	return(otsp);
}

nodup(l, s)
char **l, s[]; {
	char *t, *os, c;

	if (getsuf(s) != 'o')
		return(1);
	os = s;
	while(t = *l++) {
		s = os;
		while(c = *s++)
			if (c != *t++)
				break;
		if (*t++ == '\0')
			return(0);
	}
	return(1);
}

llenter(t) char *t; {
	if (nodup(llist, t)) {
		llist[nl++] = t;
		if (getsuf(t)=='o')
			nxo++;
	}
}

cunlink(f)
char *f;
{
	if( dflag )
		printf("unlink %s\n", f);
	if (f==0)
		return(0);
	return(unlink(f));
}

splitup(){
	char in[200], fname[20];
	int buf[259];
	int i,fd,c;
	if( (fin=open("ratjunk", 0)) < 0)
		error("can't open ratjunk\n");
	while( gets(in) ){
		getname(in, fname);
		savename(fname);
		if( (fd = fcreat(fname, buf)) < 0)
			error("can't open %s", fname);
		puts(in,buf);
		while( ! endcard(in) ){
			gets(in);
			puts(in,buf);
		}
		fflush(buf);
		close(fd);
	}
	close(fin);
}

gets(s) char *s; {
	int c;
	while( (*s++=c=getchar()) != '\n' && c != '\0' );
	*s = '\0';
	return(c);
}

puts(s,b) char *s; int *b; {
	while( *s )
		putc(*s++, b);
}

savename(s) char *s; {
	rlist[nr++] = copy(s);
}

getname(s,f) char *s,*f; {
	int i,j,c;
   loop:
	while( *s == ' ' || *s == '\t' )
		s++;
	if( compar(s,"subroutine") ){ s =+ 10; goto bot; }
	else if( compar( s,"function") ){ s =+ 8; goto bot; }
	else if( compar(s,"real") ){ s =+ 4; goto loop; }
	else if( compar(s,"integer") ){ s =+ 7; goto loop; }
	else if( compar(s,"logical") ){ s =+ 7; goto loop; }
	else if( compar(s,"double") ){ s =+ 6; goto loop; }
	else if( compar(s,"precision") ){ s =+ 9; goto loop; }
	else if( compar(s,"complex") ){ s =+ 7; goto loop; }
	else if( compar(s,"block") ){
		s = "blockdata ";
		s[9] = (bdcount++) + '0';
		goto bot;
	}
	else {
		for(i=0; f[i]="MAIN.f"[i]; i++);
		return;
	}
   bot:
	while( *s == ' ' || *s == '\t' )
		s++;
	for(i=0; alphanum(s[i]); i++)
		f[i] = s[i];
	f[i++] = '.';
	f[i++] = 'f';
	f[i++] = '\0';
}

compar(s,t) char *s,*t; {
	while( *t )
		if( *s++ != *t++ )
			return(0);
	return(1);
}

alphanum(c) int c; {
	return( (c>='a' && c<='z')
		|| (c>='A' && c<='Z')
		|| (c>='0' && c<='9') );
}

endcard(s) char *s; {
	if( *s==0 )
		return(1);
	while( *s==' ' || *s=='\t' )
		s++;
	if( *s!='e' || *(s+1)!='n' || *(s+2)!='d' || *(s+3)!='\n' )
		return(0);
	return(1);
}

error(s1, s2){
	fout = 1;
	printf(s1,s2);
	putchar('\n');
	flush(1);
	cflag++;
}
