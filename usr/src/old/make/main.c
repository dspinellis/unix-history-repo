static	char *sccsid = "@(#)main.c	4.2 (Berkeley) 82/04/20";
# include "defs"
/*
command make to update programs.
Flags:	'd'  print out debugging comments
	'p'  print out a version of the input graph
	's'  silent mode--don't print out commands
	'f'  the next argument is the name of the description file;
	     "makefile" is the default
	'i'  ignore error codes from the shell
	'S'  stop after any command fails (normally do parallel work)
	'n'   don't issue, just print, commands
	't'   touch (update time of) files but don't issue command
	'q'   don't do anything, but check if object is up to date;
	      returns exit code 0 if up to date, -1 if not
*/

struct nameblock *mainname	= NULL;
struct nameblock *firstname	= NULL;
struct lineblock *sufflist	= NULL;
struct varblock *firstvar	= NULL;
struct pattern *firstpat	= NULL;
struct dirhdr *firstod		= NULL;

#include <signal.h>
int sigivalue	= 0;
int sigqvalue	= 0;
int waitpid	= 0;

int dbgflag	= NO;
int prtrflag	= NO;
int silflag	= NO;
int noexflag	= NO;
int keepgoing	= NO;
int noruleflag	= NO;
int touchflag	= NO;
int questflag	= NO;
int ndocoms	= NO;
int ignerr	= NO;    /* default is to stop on error */
int okdel	= YES;
int inarglist;
#ifdef pwb
char *prompt	= ">";	/* other systems -- pick what you want */
#else
char *prompt	= "";	/* other systems -- pick what you want */
#endif
int nopdir	= 0;
char junkname[20];
char funny[128];

main(argc,argv)
int argc;
char *argv[];
{
register struct nameblock *p;
int i, j;
int descset, nfargs;
TIMETYPE tjunk;
char c, *s;
static char onechar[2] = "X";
#ifdef unix
int intrupt();



#endif

#ifdef METERFILE
meter(METERFILE);
#endif

descset = 0;

funny['\0'] = (META | TERMINAL);
for(s = "=|^();&<>*?[]:$`'\"\\\n" ; *s ; ++s)
	funny[*s] |= META;
for(s = "\n\t :;&>|" ; *s ; ++s)
	funny[*s] |= TERMINAL;


inarglist = 1;
for(i=1; i<argc; ++i)
	if(argv[i]!=0 && argv[i][0]!='-' && eqsign(argv[i]))
		argv[i] = 0;

setvar("$","$");
inarglist = 0;

for(i=1; i<argc; ++i)
    if(argv[i]!=0 && argv[i][0]=='-')
	{
	for(j=1 ; (c=argv[i][j])!='\0' ; ++j)  switch(c)
		{
		case 'd':
			dbgflag = YES;
			break;

		case 'p':
			prtrflag = YES;
			break;

		case 's':
			silflag = YES;
			break;

		case 'i':
			ignerr = YES;
			break;

		case 'S':
			keepgoing = NO;
			break;

		case 'k':
			keepgoing = YES;
			break;

		case 'n':
			noexflag = YES;
			break;

		case 'r':
			noruleflag = YES;
			break;

		case 't':
			touchflag = YES;
			break;

		case 'q':
			questflag = YES;
			break;

		case 'f':
			if(i >= argc-1)
			  fatal("No description argument after -f flag");
			if( rddescf(argv[i+1]) )
				fatal1("Cannot open %s", argv[i+1]);
			argv[i+1] = 0;
			++descset;
			break;

		default:
			onechar[0] = c;	/* to make lint happy */
			fatal1("Unknown flag argument %s", onechar);
		}

	argv[i] = 0;
	}

if( !descset )
#ifdef unix
	if( rddescf("makefile") )  rddescf("Makefile");
#endif
#ifdef gcos
	rddescf("makefile");
#endif

if(prtrflag) printdesc(NO);

if( srchname(".IGNORE") ) ++ignerr;
if( srchname(".SILENT") ) silflag = 1;
if(p=srchname(".SUFFIXES")) sufflist = p->linep;
if( !sufflist ) fprintf(stderr,"No suffix list.\n");

#ifdef unix
sigivalue = (int) signal(SIGINT, SIG_IGN) & 01;
sigqvalue = (int) signal(SIGQUIT, SIG_IGN) & 01;
enbint(intrupt);
#endif

nfargs = 0;

for(i=1; i<argc; ++i)
	if((s=argv[i]) != 0)
		{
		if((p=srchname(s)) == 0)
			{
			p = makename(s);
			}
		++nfargs;
		doname(p, 0, &tjunk);
		if(dbgflag) printdesc(YES);
		}

/*
If no file arguments have been encountered, make the first
name encountered that doesn't start with a dot
*/

if(nfargs == 0)
	if(mainname == 0)
		fatal("No arguments or description file");
	else	{
		doname(mainname, 0, &tjunk);
		if(dbgflag) printdesc(YES);
		}

exit(0);
}



#ifdef unix
intrupt()
{
struct varblock *varptr();
char *p;
TIMETYPE exists();

if(okdel && !noexflag && !touchflag &&
	(p = varptr("@")->varval) && exists(p)>0 && !isprecious(p) )
		{
		fprintf(stderr, "\n***  %s removed.", p);
		unlink(p);
		}

if(junkname[0])
	unlink(junkname);
fprintf(stderr, "\n");
exit(2);
}




isprecious(p)
char *p;
{
register struct lineblock *lp;
register struct depblock *dp;
register struct nameblock *np;

if(np = srchname(".PRECIOUS"))
	for(lp = np->linep ; lp ; lp = lp->nxtlineblock)
		for(dp = lp->depp ; dp ; dp = dp->nxtdepblock)
			if(! unequal(p, dp->depname->namep))
				return(YES);

return(NO);
}


enbint(k)
int (*k)();
{
if(sigivalue == 0)
	signal(SIGINT,k);
if(sigqvalue == 0)
	signal(SIGQUIT,k);
}
#endif

extern char *builtin[];

char **linesptr	= builtin;

FILE * fin;
int firstrd	= 0;


rddescf(descfile)
char *descfile;
{
FILE * k;

/* read and parse description */

if( !firstrd++ )
	{
	if( !noruleflag )
		rdd1( (FILE *) NULL);

#ifdef pwb
		{
		char *nlog, s[100];
		nlog = logdir();
		if ( (k=fopen( concat(nlog,"/makecomm",s), "r")) != NULL)
			rdd1(k);
		else if ( (k=fopen( concat(nlog,"/Makecomm",s), "r")) != NULL)
			rdd1(k);
	
		if ( (k=fopen("makecomm", "r")) != NULL)
			rdd1(k);
		else if ( (k=fopen("Makecomm", "r")) != NULL)
			rdd1(k);
		}
#endif

	}
if(! unequal(descfile, "-"))
	return( rdd1(stdin) );

if( (k = fopen(descfile,"r")) != NULL)
	return( rdd1(k) );

return(1);
}




rdd1(k)
FILE * k;
{
extern int yylineno;
extern char *zznextc;

fin = k;
yylineno = 0;
zznextc = 0;

if( yyparse() )
	fatal("Description file error");

if(fin != NULL)
	fclose(fin);

return(0);
}

printdesc(prntflag)
int prntflag;
{
struct nameblock *p;
struct depblock *dp;
struct varblock *vp;
struct dirhdr *od;
struct shblock *sp;
struct lineblock *lp;

#ifdef unix
if(prntflag)
	{
	printf("Open directories:\n");
	for (od = firstod; od; od = od->nxtopendir)
		printf("\t%d: %s\n", od->dirfc->dd_fd, od->dirn);
	}
#endif

if(firstvar != 0) printf("Macros:\n");
for(vp = firstvar; vp ; vp = vp->nxtvarblock)
	printf("\t%s = %s\n" , vp->varname , vp->varval);

for(p = firstname; p; p = p->nxtnameblock)
	{
	printf("\n\n%s",p->namep);
	if(p->linep != 0) printf(":");
	if(prntflag) printf("  done=%d",p->done);
	if(p==mainname) printf("  (MAIN NAME)");
	for(lp = p->linep ; lp ; lp = lp->nxtlineblock)
		{
		if( dp = lp->depp )
			{
			printf("\n depends on:");
			for(; dp ; dp = dp->nxtdepblock)
				if(dp->depname != 0)
					printf(" %s ", dp->depname->namep);
			}
	
		if(sp = lp->shp)
			{
			printf("\n commands:\n");
			for( ; sp!=0 ; sp = sp->nxtshblock)
				printf("\t%s\n", sp->shbp);
			}
		}
	}
printf("\n");
fflush(stdout);
}
