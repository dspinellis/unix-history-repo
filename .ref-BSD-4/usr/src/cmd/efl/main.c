static char xxxvers[ ] = "\n@(#)EFL VERSION 1.14,  19 AUGUST 1980";

/* Compiler for the EFL Programming Language.  Written by:
		Stuart I. Feldman
		Bell Laboratories
		Murray Hill, New Jersey
*/


/* Flags:
	-d	EFL debugging output
	-v	verbose (print out Pass numbers and memory limits)
	-w	supress warning messages
	-f	put Fortran output on appropriate .f files
	-F	put Fortran code for input file x onto x.F
	-e	divert diagnostic output to next argument
	-#	do not pass comments through to output
*/


#include "defs"

int sysflag;

int nerrs	= 0;
int nbad	= 0;
int nwarns	= 0;
int stnos[MAXSTNO];
int nxtstno	= 0;
int constno	= 0;
int labno	= 0;

int dumpic	= NO;
int memdump	= NO;
int dbgflag	= NO;
int nowarnflag	= NO;
int nocommentflag	= NO;
int verbose	= NO;
int dumpcore	= NO;
char msg[200];

struct fileblock fcb[4];
struct fileblock *iifilep;
struct fileblock *ibfile	= &fcb[0];
struct fileblock *icfile	= &fcb[1];
struct fileblock *idfile	= &fcb[2];
struct fileblock *iefile	= &fcb[3];

FILE *diagfile	= {stderr};
FILE *codefile	= {stdout};
FILE *fileptrs[MAXINCLUDEDEPTH];
char *filenames[MAXINCLUDEDEPTH];
char *basefile;
int filelines[MAXINCLUDEDEPTH];
int filedepth	= 0;
char *efmacp	= NULL;
char *filemacs[MAXINCLUDEDEPTH];
int pushchars[MAXINCLUDEDEPTH];
int ateof	= NO;

int igeol	= NO;
int pushlex	= NO;
int eofneed	= NO;
int forcerr	 = NO;
int defneed	 = NO;
int prevbg	 = NO;
int comneed	 = NO;
int optneed	 = NO;
int lettneed	= NO;
int iobrlevel	= 0;

ptr comments	= NULL;
ptr prevcomments	= NULL;
ptr genequivs	= NULL;
ptr arrays	= NULL;
ptr generlist	= NULL;
ptr knownlist	= NULL;

ptr thisexec;
ptr thisctl;
chainp tempvarlist	= CHNULL;
chainp temptypelist	= CHNULL;
chainp hidlist	= CHNULL;
chainp commonlist	= CHNULL;
chainp gonelist	= CHNULL;
int blklevel	= 0;
int ctllevel	= 0;
int dclsect	= 0;
int instruct	= 0;
int inbound	= 0;
int inproc	= 0;
int ncases	= 0;

int graal	= 0;
ptr procname	= NULL;
int procclass	= 0;
ptr thisargs	= NULL;

int nhid[MAXBLOCKDEPTH];
int ndecl[MAXBLOCKDEPTH];

char ftnames[MAXFTNAMES][7];


int neflnames	= 0;

int nftnames;
int nftnm0;
int impltype[26];

int ftnefl[NFTNTYPES]	= { TYINT, TYREAL, TYLOG, TYCOMPLEX, TYLREAL,
				TYCHAR, TYLCOMPLEX };
int eflftn[NEFLTYPES];
int ftnmask[NFTNTYPES] 	= { 1, 2, 4, 8, 16, 32, 64 };
struct tailoring tailor;
struct system systab[] =
	{
		{ "portable", 0,	1, 10, 7, 15},
		{ "unix", UNIX,	4, 10, 7, 15 },
		{ "gcos", GCOS,	4, 10, 7, 15 },
		{ "gcosbcd", GCOSBCD,	6, 10, 7, 15},
		{ "cray", CRAY,	8, 10, 7, 15},
		{ "ibm", IBM,	4, 10, 7, 15 },
		{ NULL }
	};

double fieldmax	= FIELDMAX;

int langopt	= 2;
int dotsopt	= 0;
int dbgopt	= 0;
int dbglevel	= 0;

int nftnch;
int nftncont;
int indifs[MAXINDIFS];
int nxtindif;
int afterif	= 0;

#ifdef	gcos
#	define BIT(n)	(1 << (36 - 1 - n) )
#	define FORTRAN	BIT(1)
#	define FDS	BIT(4)
#	define EXEC	BIT(5)
#	define FORM	BIT(14)
#	define LNO	BIT(15)
#	define BCD	BIT(16)
#	define OPTZ	BIT(17)
	int	compile	= FORTRAN | FDS;
#endif


main(argc,argv)
register int argc;
register char **argv;
{
FILE *fd;
register char *p;
int neflnm0;

#ifdef unix
	int intrupt();
	sysflag = UNIX;

/*
	meter();
*/
	if( (signal(2,1) & 01) == 0)
		signal(2, intrupt);
#endif

#ifdef gcos
/*
	meter();
*/
	sysflag = (intss() ? GCOS : GCOSBCD);
#endif


crii();
--argc;
++argv;
tailinit(systab + sysflag);

while(argc>0 && ( (argv[0][0]=='-' && argv[0][1]!='\0') || eqlstrng(argv[0]) ))
	{
	if(argv[0][0] == '-')
	    for(p = argv[0]+1 ; *p ; ++p) switch(*p)
		{
		case ' ':
			break;

		case 'd':
		case 'D':
			switch( *++p)
				{
				case '1':
					dbgflag = YES;
					break;
				case '2':
					setyydeb();
					break;
				case '3':
					dumpcore = YES;
					break;
				case '4':
					dumpic = YES;
					break;
				case 'm':
				case 'M':
					memdump = YES;
					break;

				default:
					dbgflag = YES;
					--p;
					break;
				}
			break;

		case 'w':
		case 'W':
			nowarnflag = YES;
			break;

		case 'v':
		case 'V':
			verbose = YES;
			break;

		case '#':
			nocommentflag = YES;
			break;

		case 'C':
		case 'c':
			nocommentflag = NO;
			break;

#ifdef gcos
		case 'O':
		case 'o':
			compile |= OPTZ;
			break;

		case 'E':
		case 'e':
			compile = 0;
			break;
#endif

		default:
			fprintf(diagfile, "Illegal EFL flag %c\n", *p);
			exit(1);
		}
	--argc;
	++argv;
	}

kwinit();
geninit();
knowninit();
init();
implinit();
neflnm0 = neflnames;

#ifdef gcos
	if( intss() )
		compile = 0;
	else
		gcoutf();
#endif

/*	fprintf(diagfile, "EFL 1.10\n");	*/

if(argc==0)
	{
	filenames[0] = "-";
	dofile(stdin);
	}
else
	while(argc>0)
		{
		if( eqlstrng(argv[0]) )
			{
			--argc;
			++argv;
			continue;
			}
		if(argv[0][0]=='-' && argv[0][1]=='\0')
			{
			basefile = "";
			fd = stdin;
			}
		else	{
			basefile = argv[0];
			fd = fopen(argv[0], "r");
			}
		if(fd == NULL)
			{
			sprintf(msg, "Cannot open file %s", argv[0]);
			fprintf(diagfile, "%s.  Stop\n", msg);
			done(2);
			}
		filenames[0] = argv[0];
		filedepth = 0;

		nftnames = 0;
		nftnm0 = 0;
		neflnames = neflnm0;

		dofile(fd);
		if(fd != stdin)
			fclose(fd);
		--argc;
		++argv;
		}
p2flush();
if(verbose)
	fprintf(diagfile, "End of compilation\n");
/*
prhisto();
/* */
rmiis();

#ifdef gcos
	gccomp();
#endif

done(nbad);
}


dofile(fd)
FILE *fd;
{
int k;

fprintf(diagfile, "File %s:\n", filenames[0]);

#ifdef gcos
	if( fd==stdin && intss() && inquire(stdin, _TTY) )
		freopen("*src", "rt", stdin);
#endif

yyin = fileptrs[0] = fd;
yylineno = filelines[0] = 1;
filedepth = 0;
ateof = 0;

do	{
	nerrs = 0;
	nwarns = 0;
	eofneed = 0;
	forcerr = 0;
	comneed = 0;
	optneed = 0;
	defneed = 0;
	lettneed = 0;
	iobrlevel = 0;
	prevbg = 0;

	constno = 0;
	labno = 0;
	nxtstno = 0;
	afterif = 0;
	thisexec = 0;
	thisctl = 0;
	nxtindif = 0;
	inproc = 0;
	blklevel = 0;

	implinit();

	opiis();
	swii(icfile);

	if(k = yyparse())
		fprintf(diagfile, "Error in source file.\n");
	else  switch(graal)
		{
		case PARSERR:
			/*
			fprintf(diagfile, "error\n");
			*/
			break;

		case PARSEOF:
			break;

		case PARSOPT:
			propts();
			break;

		case PARSDCL:
			fprintf(diagfile, "external declaration\n");
			break;

		case PARSPROC:
			/* work already done in endproc */
			break;

		case PARSDEF:
			break;
		}

	cliis();
	if(nerrs) ++nbad;

	} while(graal!=PARSEOF && !ateof);
}

ptr bgnproc()
{
ptr bgnexec();

if(blklevel > 0)
	{
	execerr("procedure %s terminated prematurely", procnm() );
	endproc();
	}
ctllevel = 0;
procname = 0;
procclass = 0;
thisargs = 0;
dclsect = 0;
blklevel = 1;
nftnm0 = nftnames;
dclsect = 1;
ndecl[1] = 0;
nhid[1] = 0;

thisctl = allexcblock();
thisctl->tag = TCONTROL;
thisctl->subtype = STPROC;
inproc = 1;
return( bgnexec() );
}


endproc()
{
char comline[50], *concat();
ptr p;

inproc = 0;

if(nerrs == 0)
	{
	pass2();
	unhide();
	cleanst();
	if(dumpic)
		system( concat("od ", icfile->filename, comline) );
	if(memdump)
		prmem();
	}
else	{
	fprintf(diagfile, "**Procedure %s not generated\n", procnm());
	for( ; blklevel > 0 ; --blklevel)
		unhide();
	cleanst();
	}

if(nerrs==0 && nwarns>0)
	if(nwarns == 1)
		fprintf(diagfile,"*1 warning\n");
	else	fprintf(diagfile, "*%d warnings\n", nwarns);

blklevel = 0;
thisargs = 0;
procname = 0;
procclass = 0;
while(thisctl)
	{
	p = thisctl;
	thisctl = thisctl->prevctl;
	frexcblock(p);
	}

while(thisexec)
	{
	p = thisexec;
	thisexec = thisexec->prevexec;
	frexcblock(p);
	}

nftnames = nftnm0;
if(verbose)
	{
	fprintf(diagfile, "Highwater mark %d words. ", nmemused);
	fprintf(diagfile, "%ld words left over\n", totalloc-totfreed);
	}
}




implinit()
{
setimpl(TYREAL, 'a', 'z');
setimpl(TYINT,  'i', 'n');
}



init()
{
eflftn[TYINT] = FTNINT;
eflftn[TYREAL] = FTNREAL;
eflftn[TYLREAL] = FTNDOUBLE;
eflftn[TYLOG] = FTNLOG;
eflftn[TYCOMPLEX] = FTNCOMPLEX;
eflftn[TYCHAR] = FTNINT;
eflftn[TYFIELD] = FTNINT;
eflftn[TYLCOMPLEX] = FTNDOUBLE;
}




#ifdef gcos
meter()
{
FILE *mout;
char *cuserid(), *datime(), *s;
if(equals(s = cuserid(), "efl")) return;
mout = fopen("efl/eflmeter", "a");
if(mout == NULL)
	fprintf(diagfile,"cannot open meter file");

else	{
	fprintf(mout, "%s user %s at %s\n",
		( rutss()? "tss  " : "batch"), s, datime() );
	fclose(mout);
	}
}
#endif



#ifdef unix
meter()	/* temporary metering of non-SIF usage */
{
FILE *mout;
int tvec[2];
int uid;
char *ctime(), *p;

uid = getuid() & 0377;
if(uid == 91) return;	/* ignore sif uses */
mout = fopen("/usr/sif/efl/Meter", "a");
if(mout == NULL)
	fprintf(diagfile, "cannot open meter file");
else	{
	time(tvec);
	p = ctime(tvec);
	p[16] = '\0';
	fprintf(mout,"User %d, %s\n",  uid, p+4);
	fclose(mout);
	}
}

intrupt()
{
done(0);
}
#endif


done(k)
int k;
{
rmiis();
exit(k);
}





/* if string has an embedded equal sign, set option with it*/
eqlstrng(s)
char *s;
{
register char *t;

for(t = s; *t; ++t)
	if(*t == '=')
		{
		*t = '\0';
		while( *++t == ' ' )
			;
		setopt(s, t);
		return(YES);
		}

return(NO);
}

#ifdef gcos

/* redirect output unit */

gcoutf()
{
if (!intss())
	{
	fputs("\t\t    Version 2.10 : read INFO/EFL (03/27/80)\n", stderr);
	if (compile)
		{
		static char name[80] = "s*", opts[20] = "yw";
		char *opt = (char *)inquire(stdout, _OPTIONS);
		if (!strchr(opt, 't'))
			{ /* if stdout is diverted */
			sprintf(name, "%s\"s*\"",
				(char *)inquire(stdout, _FILENAME));
			strcpy(&opts[1], opt);
			}
		if (freopen(name, opts, stdout) == NULL)
			cant(name);
		}
	}
}



/* call in fortran compiler if necessary */

gccomp()
{
if (compile)
	{
	if (nbad > 0)	/* abort */
		cretsw(EXEC);

	else	{ /* good: call forty */
		FILE *dstar; /* to intercept "gosys" action */

		if ((dstar = fopen("d*", "wv")) == NULL)
			cant("d*");
		fputs("$\tforty\tascii", dstar);
		if (fopen("*1", "o") == NULL)
			cant("*1");
		fclose(stdout, "rl");
		cretsw(FORM | LNO | BCD);
		if (! tailor.ftncontnu)
			compile |= FORM;
		csetsw(compile);
		gosys("forty");
		}
	}
}


cant(s)
char *s;
{
ffiler(s);
done(1);
}
#endif
