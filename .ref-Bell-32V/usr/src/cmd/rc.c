#include <signal.h>
/* EFL-Ratfor-Fortran command */

char *setsuf(),*copy();
extern int fin, fout;
char	ts[4000];
char	*tsp	= ts;
char	*av[500];
char	*rlist[500];
int	nr	= 0;
char	*llist[500];
int	nl	= 0;
int	nxo	= 0;
int	bdcount	= 0;	/* count block data files generated */
int	rflag	= 0;	/* Ratfor or EFL ony, no compile */
int	dflag	= 0;	/* Compile EFL DEBUG statements if set */
int	tflag	= 0;	/* Trace operation of command if set */
int	vflag	= 1;	/* Verify files compiled if set */
int	mflag	= 0;	/* Ratfor macro pre-pass if set */
int	fflag	= 0;	/* Save Fortran intermediate files if set */
int	cflag	= 0;	/* Compile only if set */
int	Uflag	= 0;	/* Add IMPLICIT UNDEFINED to generated fortran */
int	Cflag	= 0;	/* Copy Ratfor comments if set */
int	errcnt;
char	*arg0;
char	*complr = "/usr/fort/fc1";
char	*ratfor = "/usr/bin/ratfor";
char	*ratout = "ratjunk";
char	*rattmp = "ratjunk.r";
char	*ratopt	= "-1&";
char	*efl    = "/usr/bin/efl";
char	*eflout = "efljunk";
char	*eflopt = "-u         ";
char	*macro	= "/usr/bin/m4";
char	*undecl	= "implicit undefined /a-z,A-Z/\n";

# define BADOPEN 127
main(argc, argv)
char *argv[]; {
	char *t;
	int i, j, c;
	int dexit();

	arg0 = argv[0];
	for(i=0; ++i < argc; ) {
		if(*argv[i] == '-')
			for(j=1; argv[i][j]; j++) {
				switch (argv[i][j]) {
				default:
					if(j == 1) goto passa;
					else continue;
				case 'm':
					mflag = 1;
					break;
				case 't':
					tflag = 1;
					break;
				case 'v':
					vflag = 0;
					break;
				case 'd':
					eflopt[7] = 'd';
					break;
				case 'g':
					eflopt[2] = 'g';
					eflopt[3] = argv[i][j+1];
					rflag = cflag = fflag = 1;
					break;
				case 'e':
				case 'r':
					rflag = fflag = cflag = 1;
					break;
				case 'f':
					fflag = 1;
					break;
				case 'c':
					cflag = 1;
					break;
				case 'U':
					Uflag = 1;
					break;
				case 'C':
					Cflag = 1;
					break;
				case '2':
					complr = "/usr/fort/fc2";
					break;
				case '6':
					ratopt[1] = '6';
					ratopt[2] = argv[i][j+1];
					rflag = cflag = fflag = 1;
					break;
				case '9':
					eflopt[4] = '9';
					break;
				case '#':
					eflopt[5] = '#';
					break;
				case 'w':
					eflopt[6] = 'w';
					break;
				}
			}
		else {
	   passa:
			t = argv[i];
			switch( getsuf(t) ) {

				case 'e':
					eflcomp(t);
					break;

				case 'r':
					ratcomp(t);
					break;

				case 'f':
					fortcomp(t);
					llenter(setsuf(copy(t),'o'));
					break;

				default:
					llenter(copy(t));
					break;
				}
		}
	}
	if(rflag)
		dexit(0);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, dexit);
	if(tflag)
		printf("errcnt=%d, nl=%d\n", errcnt, nl);
	if (errcnt==0 & cflag==0 && nl!=0) {
		i = 0;
		av[0] = "ld";
		av[1] = "-x";
		av[2] = "/lib/fr0.o";
		j = 3;
		while(i<nl)
			av[j++] = llist[i++];
		av[j++] = "-lf";
		av[j++] = "/usr/lib/filib.a";
		av[j++] = "-l";
		av[j++] = 0;
		callsys("/bin/ld", av);
	}
	dexit(errcnt);
}

dexit(n)
int n;
{
	cunlink(ratout);
	cunlink(rattmp);
	cunlink(eflout);
	cunlink("f.tmp1");
	if(tflag)
		printf("%s status=%d\n", arg0, n);
	exit(n);
}

eflcomp(s) char *s; {
	nr = 0;
	if(vflag)
		printf("%s:\n",s);
	if( callprep( efl, s, eflout, eflopt, 0, 0 ) == 0 ) {
		splitup(eflout);
		dorlist(s);
		}
}



ratcomp(s) char *s; {
	int i, j, t;
	nr = 0;
	if(vflag)
		printf("%s:\n",s);
	if (mflag) {
		if( ( t = callprep( macro, s, rattmp, 0, 0, 0 ) ) < BADOPEN )
			t = callprep( ratfor, rattmp, ratout, ratopt, Cflag?"-C":0, 0 );
	} else
		t = callprep( ratfor, s, ratout, ratopt, Cflag?"-C":0, 0 );
	if( t < BADOPEN ) {
		splitup(ratout);
		dorlist(s);
		}
}

callprep( prep, file, output, opt1, opt2, opt3 )
char *prep, *file, *output, *opt1, *opt2, *opt3;
{
	int t, status, i, j;

	av[0] = prep;
	j = 1;
	if (opt1) av[j++] = opt1;
	if (opt2) av[j++] = opt2;
	if (opt3) av[j++] = opt3;
	av[j] = 0;
	if( tflag ) {
		printf("%s <%s ", av[0], file);
		for (i=1; av[i]; i++)
			printf("%s ", av[i]);
		printf("\n");
	}
	if( (t=fork())==0 ){
		close(1);
		if( (fout=creat(output, 0666)) < 0) {
			error( "can't open %s", output );
			dexit(BADOPEN);
			}
		close(0);
		if( (fin=open(file, 0)) < 0) {
			error( "can't open %s", file );
			dexit(BADOPEN);
			}
		execv(prep, av);
		error("can't execute %s", prep);
		dexit(1);
	}
	while( t!=wait(&status) );
	if( (t=(status&0377)) != 0 && t!=14 )
		dexit(1);
	t = (status>>8) & 0377;
	if( tflag )
		printf("status = %d\n", t);
	if( t ) ++errcnt;
	return ( t );
}

dorlist(s) char *s; {

	int i, j, t;
	int fstat;

	if( rflag ) return;
	fstat = 0;
	for(i=0; i<nr; i++){
		if( vflag ) printf("   ");
		if( fortcomp(rlist[i]) )
			fstat++;
		}
	if( fstat ) {
		for(i=0; i<nr; i++) {
			cunlink( setsuf( rlist[i], 'o' ) );
			if( fflag==0 ) cunlink( setsuf( rlist[i], 'f' ) );
			}
		return;
		}
	av[0] = "ld";
	av[1] = "-r";
	av[2] = "-x";
	j = 3;
	for(i=0; i<nr; i++)
		av[j++] = rlist[i];
	av[j] = 0;
	callsys("/bin/ld", av);
	t = setsuf(copy(s),'o');
	if( move( "a.out", t) )
		errcnt++;
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
		return(++errcnt);
	av[0] = "as";
	av[1] = "-";
	av[2] = "-o";
	av[3] = setsuf(s, 'o');
	av[4] = "f.tmp1";
	av[5] = 0;
	callsys("/bin/as", av);
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
	s -= 3;
	if (c<=14 && c>2 && *s++=='.')
		return(*s);
	return(0);
}

char *
setsuf(s, ch)
char s[];
{
	char *os;

	os = s;
	while( *s )
		if( *s++ == '/' )
			os = s;
	s[-1] = ch;
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

	if(tflag){
		printf("%s ", f);
		for(i=0; v[i]; i++)
			printf("%s ", v[i]);
		putchar('\n');
	}
	if ((t=fork())==0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		dexit(1);
	} else
		if (t == -1) {
			printf("Try again\n");
			return(1);
		}
	while(t!=wait(&status));
	if ((t=(status&0377)) != 0 && t!=14) {
		if (t!=2)		/* interrupt */
			printf("Fatal error in %s\n", f);
		dexit(1);
	}
	t = (status>>8) & 0377;
	if( tflag )
		printf("status = %d\n", t);
	return(t);
}

char *
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
	if( tflag )
		printf("unlink %s\n", f);
	if (f==0)
		return(0);
	return(unlink(f));
}

splitup(file) char *file; {
	char in[1500], fname[20];
	int buf[259];
	int i,fd,mainsw,c;
	if( (fin=open(file, 0)) < 0)
		error("can't open %s", file);
	while( gets(in) ){
		if( *in == 'c' || *in == 'C' ) continue;
		mainsw = getname(in, fname);
		savename(fname);
		if( (fd = fcreat(fname, buf)) < 0)
			error("can't open %s", fname);
		if(mainsw && Uflag) {
			puts(undecl,buf);
			puts(in,buf);
		} else {
			puts(in,buf);
			if( Uflag )
				puts(undecl,buf);
		}
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
	if( compar(s,"subroutine") ){ s += 10; goto bot; }
	else if( compar( s,"function") ){ s += 8; goto bot; }
	else if( compar(s,"real") ){ s += 4; goto loop; }
	else if( compar(s,"integer") ){ s += 7; goto loop; }
	else if( compar(s,"logical") ){ s += 7; goto loop; }
	else if( compar(s,"double") ){ s += 6; goto loop; }
	else if( compar(s,"precision") ){ s += 9; goto loop; }
	else if( compar(s,"complex") ){ s += 7; goto loop; }
	else if( compar(s,"*") ){	/* integer *16 */
		++s;
		while( (*s >= '0' && *s <= '9') || *s == ' ' || *s == '\t' )
			s++;
		goto loop;
	}
	else if( compar(s,"block") ){
		s = "BLOCKDATA ";
		s[9] = (bdcount++) + '0';
		goto bot;
	}
	else {
		for(i=0; f[i]="MAIN.f"[i]; i++);
		return(1);
	}
   bot:
	while( *s == ' ' || *s == '\t' )
		s++;
	for(i=0; alphanum(s[i]); i++)
		f[i] = s[i];
	f[i++] = '.';
	f[i++] = 'f';
	f[i++] = '\0';
	return(0);
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
	fout = 2;
	printf(s1,s2);
	putchar('\n');
	flush(1);
	errcnt++;
}
