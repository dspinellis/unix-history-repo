#include	<signal.h>
#include	<sys/types.h>
#include	<sys/stat.h>

#define	ONL	0
#define	TOSS	1
int	INCHAR	= 0;		/*index of incremented character in
					temporary file names. */

char	version[] = "Version 2/6/79";

char	grade;
char	remote[]= "$	remote	**,onl";
char	toss[]	= "$	sysout	toss";
int	remotsw;		/*toss-output flag*/
char	*mailfile = 0;
char	wantmail = 0;
char	*pp	= 0;		/*recipient of mail*/
char	*identf = 0;		/*ident card info*/
int	uidf	= 0;
char	gcosid[13];		/*gcos userid*/
char	cpflag = 'l';		/*copy/link flag*/
int	rmflag	= 0;		/*remove flag*/
int	debug	= 0;
int	gcdebug	= 0;		/*GCOS debug switch*/
int	archsw = 0;		/*archive switch*/

int	argc;
char	**argv;
char	*arg;
char	buf[80];		/*used by card */
int	nact = 0;		/*number of non-null files to process.*/
int	gsize	= 20;		/*size of current file in GCOS blocks.*/
long	usize	= 20*1200;	/*size of current file in bytes.*/
FILE	*tff;		/*temporary control card file*/
FILE	*nfile();
char	*getarg();
char	*sprintf();


comopt(o)		/*routine to test for common options.*/
char o;
{
	switch (o){

	case 'c':
		cpflag = 'c';
		break;

	case 'i':
		identf = getarg('i');
		break;

	case 'm':
		wantmail++;
		if(arg[2])
			pp = &arg[2];
		break;
	
	case 'n':		/*new option to suppress mail. MRW*/
		wantmail = 0;
		break;

	case 'o':
		remotsw = ONL;
		break;

	case 'r':
		rmflag++;
		break;

	case 's':
		if(arg[2] < '1' || arg[2] > '3')
			goto unknown;
		grade = arg[2];
		break;

	case 't':
		if(arg[2])
			goto unknown;
		remotsw = TOSS;
		break;

	case '#':
		debug = 1;
		break;

	case 'Z':			/*GCOS debugging switch*/
		gcdebug = 1;
		break;

unknown:
	default:
		return(0);
	}
	return(1);
}


#if LPR == 0

spool1()		/*set up common initial GCOS control cards.*/
{
	if(debug)
		tff = stdout;
	else
		if((tff = nfile(tfname)) == NULL){
			fprintf(stderr, "%s: Can't create %s.\n", NAME, tfname);
			out();
		}
	card('S', "");
	card('L', sprintf(buf, "$	sgrade	%c   %s", grade, version ) );
	if(ident())
		out();
	card('L', remote);
	if(remotsw == TOSS)
		card('L', toss);
}


spool2()			/*add final control cards, and spool job.*/
{
	if(wantmail)
		card('N', mailfile);
	card('L', "$	endjob");
	if(debug)
		out();
	fclose(tff);
	if(nact) {
		dfname[INCHAR]++;
		if(link(tfname, dfname) < 0){
			fprintf(stderr, "%s: Cannot rename %s\n", NAME, tfname);
			out();
		}
		unlink(tfname);
		execl("/usr/lib/dpd", "dpd", 0);
		execl("/etc/dpd", "dpd", 0);
		fprintf(stderr, "%s: Can't find dpd.\nFiles left in spooling dir.\n", NAME);
		exit(1);
	}
}

#endif


#if FGET == 0

filargs()		/*process file arguments for dpr, gcat, fsend, lpr.*/
{
	int i;
	FILE *f;

	if(argc == 1){
		if(mailfile == 0)
			mailfile = "pipe.end";
		if(copy(stdin, mailfile, GCAT) == -1)
			out();
		if(archsw)
			archive();
	}
	while(--argc) {
		arg = *++argv;
		switch(cpflag){

		case 'l':
			if(lfname[INCHAR]++ >= 'z')
				cpflag = rmflag ? 'c' : 'n';
			else if(link(arg, lfname) == 0){
				if(size(arg,arg) <= 0)
					continue;
				nuact(arg);
				card(BF, lfname);
				card('U', lfname);
				break;
			}

		case 'n':
			if(*arg == '/' && !rmflag){
				if(size(arg,arg) <= 0)
					continue;
				nuact(arg);
				card(BF, arg);
				break;
			}

		case 'c':
			f = fopen(arg, "r");
			if(f == NULL){
				fprintf(stderr, "%s: Cannot open %s\n", NAME, arg);
				continue;
			}
			i = copy(f, arg, GCAT);
			fclose(f);
			if(i == -1)
				continue;
			break;
		}
		if(archsw)
			archive();
		if(rmflag){
			if(unlink(arg) < 0)
				fprintf(stderr, "%s: Cannot remove %s\n", NAME, arg);
		}
		if(mailfile == 0)
			mailfile = arg;
	}
}

#endif


FILE *nfile(name)		/*generate a new file name, and open file.*/
char *name;
{
	FILE *f;

	if(name[INCHAR] >= 'z')
		return(NULL);
	name[INCHAR]++;
	if(!access(name, 0) || (f = fopen(name, "w")) == NULL)
		return(NULL);
	return(f);
}

#if FGET == 0
 
copy(f, gname, gcatsw)
FILE	*f;
char	*gname;
int	gcatsw;
{
	int c;
	FILE *ff;
	long cnt;

	if((ff = nfile(cfname)) == NULL){
		fprintf(stderr, "%s: Too many copy files; %s not copied\n", NAME, gname);
		return(-1);
	}
	cnt = 0;
	while((c = getc(f)) != EOF){
		if(gcatsw)
		    if(c != 0){
			fprintf(stderr, "%s: Bad input from %s.\n", NAME, gname);
			out();
		    }else  gcatsw = 0;
		if((putc(c, ff) == EOF) && ferror(ff)){
			fprintf(stderr, "%s: Write error on copy of %s.\n", NAME, gname);
			break;
		}
		cnt++;
		if(cnt > MAXCOPY){
			fprintf(stderr, "%s: Copy file %s is too large\n", NAME, gname);
			break;
		}
	}
	fclose(ff);
	if(size(cfname,gname) <= 0)
		return(-1);
	nuact(gname);
	card(BF, cfname);
	card('U', cfname);
	return(0);
}

#endif

card(c, s)
int c;
char	*s;
{
	putc( c, tff );

	while( (c = *s++) != '\0') putc( c, tff );

	c = putc( '\n', tff );

	if(c == EOF){
		fprintf(stderr, "%s: Error writing control file.\n", NAME);
		out();
		}
}

size(file, name)
char	*file, *name;
{
	struct stat stbuf;

	if(stat(file,&stbuf) < 0){
		fprintf(stderr, "%s: Cannot open %s\n", NAME, file);
		return(-1);
	}
	if(!stbuf.st_size){
		fprintf(stderr, "%s: File %s is empty.\n", NAME, name);
		return(0);
	}
	usize = stbuf.st_size;
	gsize = usize / 1200;
	gsize++;
	nact++;
	return(gsize);
}


char *
getarg(c)		/*get modifier for complex options --
			    from either same or next argument. MRW
			    e.g. either "-ffile" or "-f file"*/
char	c;
{

	if(arg[2])
		return(&arg[2]);
	else if(--argc>1)
		return(arg = (++argv)[1]);
	fprintf(stderr, "%s: Incomplete -%c option\n", NAME,c);
	out();
}

#include	<pwd.h>
struct passwd *getpwuid();

ident()
{
	int c, i, j, n, test, jsave;
	struct passwd *b1;
	static char b2[100];

	if((b1 = getpwuid(getuid())) == NULL) {
		fprintf(stderr, "%s: Invalid user id\n", NAME);
		return(1);
	}
	j = 0;
#if LPR == 0
	while(c = "$	ident	"[j])
		b2[j++] = c;

	i = 0;
	if(identf) 
		while(c = identf[i++])
			b2[j++] = c;
	else{
		jsave = j;		/*use either usg or pwb-style passwd. MRW*/
		while((c = b1->pw_gecos[i++]) && c != ':')
			if(c == ')')
				j = jsave;
			else
				b2[j++] = c;
	}
	b2[j++] = ',';
#endif

	i = 0;
	if(!pp)
		pp = &b2[j];
	while(c = b1->pw_name[i++])
		b2[j++] = c;
	b2[j] = '\0';

#if LPR == 0
	i = 0;
	n = 3;
	while(--n) {
		test = 0;
		while((c=b2[i++]) && c != ',') {
			if('0' <= c && c <= '9') test += c -'0';
			else test = 0;
		}
		if(test == 0) {
			b2[j] = '\0';
			fprintf(stderr, "%s: Invalid IDENT information - %s\n", NAME, b2);
			return (1);
		}
	}

	if(!uidf) {
		n = 0;
		while((c = b2[i++]) && c != ',') {
			if(n >= 12) break;
			gcosid[n++] = c;
		}
		gcosid[n++] = '\0';
	}
#endif
	card('L', b2);
	if(wantmail){
		card('M',pp);
		if(identf)
			card('Q', b2);	/*mail back $IDENT card.*/
	}
	return (0);
}

pidfn()			/*rewrite using mktemp. MRW*/
{
	int out();

	while(tfname[INCHAR] != 'X')
		INCHAR++;
	INCHAR--;
	mktemp(cfname);
	mktemp(dfname);
	mktemp(lfname);
	mktemp(tfname);
	mktemp(zfname);
	if(signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, out);
	if(signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, out);
	if(signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		signal(SIGQUIT, out);
	if(signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, out);
}

out()
{
	register i;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	i = INCHAR;
	for(; cfname[i] != FIRSTCHAR; cfname[i]--) 
		unlink(cfname);
	if(dfname[i] != FIRSTCHAR)
		unlink(dfname);
	for(; lfname[i] != FIRSTCHAR; lfname[i]--) 
		unlink(lfname);
	if(tfname[i] != FIRSTCHAR)
		unlink(tfname);
	for(; zfname[i] != FIRSTCHAR; zfname[i]--) 
		unlink(zfname);
	exit(1);
}
