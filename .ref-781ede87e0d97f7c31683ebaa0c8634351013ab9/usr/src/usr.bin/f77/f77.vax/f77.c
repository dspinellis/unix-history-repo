/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static	char sccsid[] = "@(#)f77.c	5.7 (Berkeley) %G%";
#endif

/*
 * f77.c
 *
 * Driver program for the 4.2 BSD f77 compiler.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	f77.c,v $
 * Revision 5.4  85/12/17  19:12:14  donn
 * Dynamically allocate buffer; add lint fixes.
 * 
 * Revision 5.3  85/11/25  00:00:02  donn
 * 4.3 beta
 * 
 * Revision 5.2  85/08/10  05:16:14  donn
 * Ifdeffed 66 code, added -r8 flag.  From Jerry Berkman.
 * 
 * Revision 5.1  85/08/10  03:32:12  donn
 * 4.3 alpha
 * 
 * Revision 1.14  85/03/01  00:07:57  donn
 * Portability fix from Ralph Campbell.
 * 
 * Revision 1.13  85/02/12  19:31:47  donn
 * Use CATNAME to get the name of a concatenation command instead of
 * explicitly running 'cat' -- you can get the wrong 'cat' the old way!
 * 
 * Revision 1.12  85/01/14  06:42:30  donn
 * Changed to call the peephole optimizer with the '-f' flag, so that
 * floating point moves are translated to integer moves.
 * 
 * Revision 1.11  85/01/14  04:38:59  donn
 * Jerry's change to pass -O to f1 so it knows whether the peephole optimizer
 * will be run.  This is necessary in order to handle movf/movl translation.
 * 
 * Revision 1.10  85/01/14  03:59:12  donn
 * Added Jerry Berkman's fix for the '-q' flag.
 * 
 * Revision 1.9  84/11/09  01:51:26  donn
 * Cosmetic change to stupid() suggested by John McCarthy at Memorial
 * University, St. Johns.
 * 
 * Revision 1.8  84/09/14  16:02:34  donn
 * Added changes to notice when people do 'f77 -c foo.f -o bar.o' and tell
 * them why it doesn't do what they think it does.
 * 
 * Revision 1.7  84/08/24  21:08:31  donn
 * Added call to setrlimit() to prevent core dumps when not debugging.
 * Reorganized the include file arrangment somewhat.
 * 
 * Revision 1.6  84/08/24  20:20:24  donn
 * Changed stupidity check on Jerry Berkman's suggestion -- now it balks if
 * the load file exists and has a sensitive suffix.
 * 
 * Revision 1.5  84/08/15  18:56:44  donn
 * Added test for -O combined with -g, suggested by Raleigh Romine.  To keep
 * things simple, if both are specified then the second in the list is thrown
 * out and the user is warned.
 * 
 * Revision 1.4  84/08/05  21:33:15  donn
 * Added stupidity check -- f77 won't load on a file that it's asked to
 * compile as well.
 * 
 * Revision 1.3  84/08/04  22:58:24  donn
 * Improved error reporting -- we now explain why we died and what we did.
 * Only works on 4.2.  Added at the instigation of Jerry Berkman.
 * 
 * Revision 1.2  84/07/28  13:11:24  donn
 * Added Ralph Campbell's changes to reduce offsets to data.
 * 
 */

char *xxxvers = "\n@(#) F77 DRIVER, VERSION 4.2,   1984 JULY 28\n";
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <ctype.h>
#include <stdio.h>

#ifdef	SIGPROF
/*
 * Some 4.2 BSD capabilities.
 */
#include <sys/time.h>
#include <sys/resource.h>
#define	NOCORE		1
#include <sys/wait.h>
#define PSIGNAL		1
#endif

#include "defines.h"
#include "machdefs.h"
#include "pathnames.h"
#include "version.h"

static FILEP diagfile	= {stderr} ;
static int pid;
static int sigivalue	= 0;
static int sigqvalue	= 0;
static int sighvalue	= 0;
static int sigtvalue	= 0;

static char *pass1name	= PASS1NAME ;
static char *pass2name	= PASS2NAME ;
static char *pass2opt	= PASS2OPT ;
static char *asmname	= ASMNAME ;
static char *ldname	= LDNAME ;
static char *footname	= FOOTNAME;
static char *proffoot	= PROFFOOT;
static char *macroname	= "m4";
static char *shellname	= _PATH_BSHELL;
static char *cppname	= _PATH_CPP;
static char *aoutname	= "a.out" ;
static char *temppref	= TEMPPREF;

static char *infname;
static char textfname[44];
static char asmfname[44];
static char asmpass2[44];
static char initfname[44];
static char sortfname[44];
static char prepfname[44];
static char objfdefault[44];
static char optzfname[44];
static char setfname[44];

static char fflags[50]	= "-";
static char f2flags[50];
static char cflags[50]	= "-c";
#if TARGET == GCOS
	static char eflags[30]	= "system=gcos ";
#else
	static char eflags[30]	= "system=unix ";
#endif
static char rflags[30]	= "";
static char lflag[3]	= "-x";
static char *fflagp	= fflags+1;
static char *f2flagp	= f2flags;
static char *eflagp	= eflags+12;
static char *rflagp	= rflags;
static char *cppflags	= "";
static char **cppargs;
static char **loadargs;
static char **loadp;

static flag erred	= NO;
static flag loadflag	= YES;
static flag saveasmflag	= NO;
static flag profileflag	= NO;
static flag optimflag	= NO;
static flag debugflag	= NO;
static flag verbose	= NO;
static flag fortonly	= NO;
static flag macroflag	= NO;
static flag sdbflag	= NO;
static flag namesflag	= YES;

#if TARGET == PDP11
static flag nofloating	= NO;
#endif

static int ncpp;


main(argc, argv)
int argc;
char **argv;
{
register int i, n;
int c, status;
char *setdoto(), *lastchar(), *lastfield(), *copys(), *argvtos();
ptr ckalloc();
char *strcat();
register char *s;
char fortfile[20], *t;
char *buff;
int intrupt();
int new_aoutname = NO;

sigivalue = signal(SIGINT, SIG_IGN) == SIG_IGN;
sigqvalue = signal(SIGQUIT,SIG_IGN) == SIG_IGN;
sighvalue = signal(SIGHUP, SIG_IGN) == SIG_IGN;
sigtvalue = signal(SIGTERM,SIG_IGN) == SIG_IGN;
enbint(intrupt);

pid = getpid();
crfnames();

cppargs  = (char **) ckalloc( argc * sizeof(*cppargs) );
loadargs = (char **) ckalloc( (argc+20) * sizeof(*loadargs) );
loadargs[1] = "-X";
loadargs[2] = "-u";
#if HERE==PDP11 || HERE==VAX
	loadargs[3] = "_MAIN_";
#endif
#if HERE == INTERDATA
	loadargs[3] = "main";
#endif
loadp = loadargs + 4;

--argc;
++argv;

for (i = 0, n = 50; i < argc; ++i)
	n += strlen(argv[i]) + 1;
buff = (char *) ckalloc(n);

while(argc>0 && argv[0][0]=='-' && argv[0][1]!='\0')
	{
	for(s = argv[0]+1 ; *s ; ++s) switch(*s)
		{
		case 'T':  /* use special passes */
			switch(*++s)
				{
				case '1':
					pass1name = s+1; goto endfor;
				case '2':
					pass2name = s+1; goto endfor;
				case 'p':
					pass2opt = s+1; goto endfor;
				case 'a':
					asmname = s+1; goto endfor;
				case 'l':
					ldname = s+1; goto endfor;
				case 'F':
					footname = s+1; goto endfor;
				case 'm':
					macroname = s+1; goto endfor;
				case 't':
					temppref = s+1; goto endfor;
				default:
					fatali("bad option -T%c", *s);
				}
			break;

#ifdef ONLY66
		case '6':
			if(s[1]=='6')
				{
				*fflagp++ = *s++;
				goto copyfflag;
				}
			else	{
				fprintf(diagfile, "invalid flag 6%c\n", s[1]);
				done(1);
				}
#endif

		case 'w':
			if(s[1]=='6' && s[2]=='6')
				{
				*fflagp++ = *s++;
				*fflagp++ = *s++;
				}

		copyfflag:
		case 'u':
		case 'U':
		case '1':
		case 'C':
			*fflagp++ = *s;
			break;

		case 'O':
			if(sdbflag)
				{
				fprintf(diagfile, "-O and -g are incompatible; -O ignored\n");
				break;
				}
			optimflag = YES;
			*f2flagp++ = '-';
			*f2flagp++ = 'O';
			*f2flagp++ = ' ';
#if TARGET == INTERDATA
				*loadp++ = "-r";
				*loadp++ = "-d";
#endif
			*fflagp++ = 'O';
			break;

		case 'N':
			*fflagp++ = 'N';
			if( oneof(*++s, "qxscn") )
				*fflagp++ = *s++;
			else	{
				fprintf(diagfile, "invalid flag -N%c\n", *s);
				done(1);
				}
			while( isdigit(*s) )
				*fflagp++ = *s++;
			*fflagp++ = 'X';
			goto endfor;

		case 'm':
			if(s[1] == '4')
				++s;
			macroflag = YES;
			break;

		case 'S':
			(void) strcat(cflags, " -S");
			saveasmflag = YES;

		case 'c':
			if( new_aoutname == YES ){
				fprintf(diagfile, "-c prevents loading, -o %s ignored\n", aoutname);
				new_aoutname = NO;
				}
			loadflag = NO;
			break;

		case 'v':
			verbose = YES;
			fprintf(diagfile,"\nBerkeley F77, version %s\n",
				VERSIONNUMBER);
			break;

		case 'd':
			debugflag = YES;
			*fflagp++ = 'd';
			s++;
			while( isdigit(*s) || *s == ',' )
				*fflagp++ = *s++;
			*fflagp++ = 'X';
			goto endfor;

		case 'M':
			*loadp++ = "-M";
			break;

		case 'g':
			if(optimflag)
				{
				fprintf(diagfile, "-g and -O are incompatible; -g ignored\n");
				break;
				}
			(void) strcat(cflags," -g");
			sdbflag = YES;
			goto copyfflag;

		case 'p':
			profileflag = YES;
			(void) strcat(cflags," -p");
			*fflagp++ = 'p';
			if(s[1] == 'g')
				{
				proffoot = GPRFFOOT;
				s++;
				}
			break;

		case 'q':
			namesflag = NO;
			*fflagp++ = *s;
			break;

		case 'o':
			if( ! strcmp(s, "onetrip") )
				{
				*fflagp++ = '1';
				goto endfor;
				}
			new_aoutname = YES;
			aoutname = *++argv;
			--argc;
			if( loadflag == NO ){
				fprintf(diagfile, "-c prevents loading, -o %s ignored\n", aoutname);
				new_aoutname = NO;
				}
			break;

#if TARGET == PDP11
		case 'f':
			nofloating = YES;
			pass2name = NOFLPASS2;
		break;
#endif

		case 'F':
			fortonly = YES;
			loadflag = NO;
			break;
		case 'D':
		case 'I':
			cppargs[ncpp++] = *argv;
			goto endfor;

		case 'i':
			if((s[1]=='2' || s[1]=='4') && s[2] == '\0')
				{
				*fflagp++ = *s++;
				goto copyfflag;
				}
			fprintf(diagfile, "invalid flag -i%c\n", s[1]);
			done(1);

		case 'r':	/* -r8 - double the precision */
			if(s[1] == '8' && s[2] == '\0')
				{
				s++;
				goto copyfflag;
				}
			else
				{
				*loadp++ = "-r";
				break;
				}

		case 'l':	/* letter ell--library */
			s[-1] = '-';
			*loadp++ = s-1;
			goto endfor;

		case 'E':	/* EFL flag argument */
			while( *eflagp++ = *++s)
				;
			*eflagp++ = ' ';
			goto endfor;
		case 'R':
			while( *rflagp++ = *++s )
				;
			*rflagp++ = ' ';
			goto endfor;
		default:
			lflag[1] = *s;
			*loadp++ = copys(lflag);
			break;
		}
endfor:
	--argc;
	++argv;
	}

#ifdef	NOCORE
if(!debugflag)
	{
	struct rlimit r;

	r.rlim_cur = r.rlim_max = 0;
	(void) setrlimit(RLIMIT_CORE, &r);
	}
#endif	NOCORE

*fflagp = '\0';

if (ncpp > 0)
	cppflags = argvtos (ncpp,cppargs);

loadargs[0] = ldname;
#if TARGET == PDP11
	if(nofloating)
		*loadp++ = (profileflag ? NOFLPROF : NOFLFOOT);
	else
#endif
*loadp++ = (profileflag ? proffoot : footname);

for(i = 0 ; i<argc ; ++i)
	switch(c =  dotchar(infname = argv[i]) )
		{
		case 'r':	/* Ratfor file */
		case 'e':	/* EFL file */
			if( unreadable(argv[i]) )
				{
				erred = YES;
				break;
				}
			s = fortfile;
			t = lastfield(argv[i]);
			while( *s++ = *t++)
				;
			s[-2] = 'f';

			if(macroflag)
				{
				sprintf(buff, "%s %s >%s", macroname, infname, prepfname);
				if( sys(buff) )
					{
					rmf(prepfname);
					erred = YES;
					break;
					}
				infname = prepfname;
				}

			if(c == 'e')
				sprintf(buff, "efl %s %s >%s", eflags, infname, fortfile);
			else
				sprintf(buff, "ratfor %s %s >%s", rflags, infname, fortfile);
			status = sys(buff);
			if(macroflag)
				rmf(infname);
			if(status)
				{
				erred = YES;
				rmf(fortfile);
				break;
				}

			if( ! fortonly )
				{
				infname = argv[i] = lastfield(argv[i]);
				*lastchar(infname) = 'f';
	
				if( dofort(argv[i]) )
					erred = YES;
				else	{
					if( nodup(t = setdoto(argv[i])) )
						*loadp++ = t;
					rmf(fortfile);
					}
				}
			break;

		case 'F':	/* C preprocessor -> Fortran file */
			if( unreadable(argv[i]) )
				{
				erred = YES;
				break;
				}
			s = fortfile;
			t = lastfield(argv[i]);
			while( *s++ = *t++)
				;
			s[-2] = 'f';
			sprintf(buff,"%s %s %s >%s", cppname, cppflags, infname, fortfile);
			status = sys(buff);
			if(status)
				{
				erred = YES;
				rmf(fortfile);
				break;
				}

			if( ! fortonly )
				{
				infname = argv[i] = lastfield(argv[i]);
				*lastchar(infname) = 'f';

				if ( dofort(argv[i]) )
					erred = YES;
				else	{
					if (nodup(t = setdoto(argv[i])) )
						*loadp++ = t;
						rmf(fortfile);
						}
				}
			break;

		case 'f':	/* Fortran file */
			if( unreadable(argv[i]) )
				erred = YES;
			else if( dofort(argv[i]) )
				erred = YES;
			else if( nodup(t=setdoto(argv[i])) )
				*loadp++ = t;
			break;

		case 'c':	/* C file */
		case 's':	/* Assembler file */
			if( unreadable(argv[i]) )
				{
				erred = YES;
				break;
				}
#if HERE==PDP11 || HERE==VAX
			if( namesflag == YES )
				fprintf(diagfile, "%s:\n", argv[i]);
#endif
			sprintf(buff, "cc %s %s", cflags, argv[i] );
			if( sys(buff) )
				erred = YES;
			else
				if( nodup(t = setdoto(argv[i])) )
					*loadp++ = t;
			break;

		case 'o':
			if( nodup(argv[i]) )
				*loadp++ = argv[i];
			break;

		default:
			if( ! strcmp(argv[i], "-o") ) {
				aoutname = argv[++i];
				new_aoutname = YES;
				if( loadflag == NO ){
					fprintf(diagfile, "-c prevents loading, -o %s ignored\n", aoutname);
					new_aoutname = NO;
					}
			} else
				*loadp++ = argv[i];
			break;
		}

if( loadflag && stupid(aoutname) )
	erred = YES;
if(loadflag && !erred)
	doload(loadargs, loadp);
done(erred);
}



/*
 * argvtos() copies a list of arguments contained in an array of character
 * strings to a single dynamically allocated string. Each argument is
 * separated by one blank space. Returns a pointer to the string or null
 * if out of memory.
 */
#define SBUFINCR	1024
#define SBUFMAX		10240

char *
argvtos(argc, argv)
	char **argv;
	int  argc;
{
	register char *s;		/* string pointer */
	register int  i;		/* string buffer pointer */
	char *malloc();			/* memory allocator */
	char *realloc();		/* increase size of storage */
	char *sbuf;			/* string buffer */
	int nbytes;			/* bytes of memory required */
	int nu;				/* no. of SBUFINCR units required */
	int sbufsize;			/* current size of sbuf */
	int strlen();			/* string length */

	sbufsize = SBUFINCR;
	if ((sbuf = malloc((unsigned)sbufsize)) == NULL)
		{
		fatal("out of memory (argvtos)");
		/* NOTREACHED */
		}
	
	for (i = 0; argc-- > 0; ++argv)
		{
		if ((nbytes = (i+strlen(*argv)+1-sbufsize)) > 0)
			{
			nu = (nbytes+SBUFINCR-1)/SBUFINCR;
			sbufsize += nu * SBUFINCR;
			if (sbufsize > SBUFMAX)
				{
				fatal("argument length exceeded (argvtos)");
				/* NOTREACHED */
				}
			if ((sbuf = realloc(sbuf, (unsigned)sbufsize)) == NULL)
				{
				fatal("out of memory (argvtos)");
				/* NOTREACHED */
				}
			}
		for (s = *argv; *s != '\0'; i++, s++)
			sbuf[i] = *s;
		sbuf[i++] = ' ';
		}
	sbuf[--i] = '\0';
	return(sbuf);
}

dofort(s)
char *s;
{
int retcode;
char buff[200];

infname = s;
sprintf(buff, "%s %s %s %s %s %s",
	pass1name, fflags, s, asmfname, initfname, textfname);
switch( sys(buff) )
	{
	case 1:
		goto error;
	case 0:
		break;
	default:
		goto comperror;
	}

if( dopass2() )
	goto comperror;
doasm(s);
retcode = 0;

ret:
	rmf(asmfname);
	rmf(initfname);
	rmf(textfname);
	return(retcode);

error:
	fprintf(diagfile, "\nError.  No assembly.\n");
	retcode = 1;
	goto ret;

comperror:
	fprintf(diagfile, "\ncompiler error.\n");
	retcode = 2;
	goto ret;
}




dopass2()
{
char buff[100];

if(verbose)
	fprintf(diagfile, "PASS2.");

#if FAMILY==DMR
	sprintf(buff, "%s %s - %s", pass2name, textfname, asmpass2);
	return( sys(buff) );
#endif


#if FAMILY == PCC
#	if TARGET==INTERDATA
	sprintf(buff, "%s -A%s <%s >%s", pass2name, setfname, textfname, asmpass2);
#	else
	sprintf(buff, "%s %s %s >%s",
		pass2name, f2flags, textfname, asmpass2);
#	endif
	return( sys(buff) );
#endif
}




doasm(s)
char *s;
{
register char *lastc;
char *obj;
char buff[200];
char *lastchar(), *setdoto();

if(*s == '\0')
	s = objfdefault;
lastc = lastchar(s);
obj = setdoto(s);

#if TARGET==PDP11 || TARGET==VAX
#	ifdef PASS2OPT
	if(optimflag)
		{
		sprintf(buff, "%s -f %s %s", pass2opt, asmpass2, optzfname);
		if( sys(buff) )
			rmf(optzfname);
		else
			if (rename(optzfname, asmpass2))
				fatal("can't rename optimizer output file");
		}
#	endif
#endif

if(saveasmflag)
	{
	*lastc = 's';
#if TARGET == INTERDATA
	sprintf(buff, "%s %s %s %s %s >%s", CATNAME, asmfname, initfname,
		setfname, asmpass2, obj);
#else
#if TARGET == VAX
	if (sdbflag)
		sprintf(buff, "%s %s %s %s >%s",
			CATNAME, asmfname, asmpass2, initfname, obj);
	else
		sprintf(buff, "%s %s %s %s >%s",
			CATNAME, asmfname, initfname, asmpass2, obj);
#else
	sprintf(buff, "%s %s %s %s >%s",
		CATNAME, asmfname, initfname, asmpass2, obj);
#endif
#endif
	if( sys(buff) )
		fatal("can't concatenate assembly files");
	*lastc = 'o';
	}
else
	{
	if(verbose)
		fprintf(diagfile, "  ASM.");
#if TARGET == INTERDATA
	sprintf(buff, "%s -o %s %s %s %s %s", asmname, obj, asmfname,
		initfname, setfname, asmpass2);
#endif

#if TARGET == VAX
	/* vax assembler currently accepts only one input file */
	if (sdbflag)
		sprintf(buff, "%s %s %s >>%s",
			CATNAME, asmpass2, initfname, asmfname);
	else
		sprintf(buff, "%s %s %s >>%s",
			CATNAME, initfname, asmpass2, asmfname);
	if( sys(buff) )
		fatal("can't concatenate assembly files");
#ifdef UCBVAXASM
	sprintf(buff, "%s -J -o %s %s", asmname, obj, asmfname);
#else
	sprintf(buff, "%s -o %s %s", asmname, obj, asmfname);
#endif
#endif

#if TARGET == PDP11
	sprintf(buff, "%s -u -o %s %s %s", asmname, obj, asmfname, asmpass2);
#endif

#if TARGET!=INTERDATA && TARGET!=PDP11 && TARGET!=VAX
	sprintf(buff, "%s -o %s %s %s", asmname, obj, asmfname, asmpass2);
#endif

	if( sys(buff) )
		fatal("assembler error");
	if(verbose)
		fprintf(diagfile, "\n");
#if HERE==PDP11 && TARGET!=PDP11
	rmf(obj);
#endif
	}

rmf(asmpass2);
}



doload(v0, v)
register char *v0[], *v[];
{
char **p;
int waitpid;

if (profileflag)
	{
	for(p = p_liblist ; *p ; *v++ = *p++)
		;
	}
else	{
	for(p = liblist ; *p ; *v++ = *p++)
		;
	}

*v++ = "-o";
*v++ = aoutname;
*v = NULL;

if(verbose)
	fprintf(diagfile, "LOAD.");
if(debugflag)
	{
	for(p = v0 ; p<v ; ++p)
		fprintf(diagfile, "%s ", *p);
	fprintf(diagfile, "\n");
	}

#if HERE==PDP11 || HERE==INTERDATA || HERE==VAX
	if( (waitpid = fork()) == 0)
		{
		enbint(SIG_DFL);
		execv(ldname, v0);
		fatalstr("couldn't load %s", ldname);
		}
	if( await(waitpid) )
		erred = YES;
#endif

#if HERE==INTERDATA
	if(optimflag)
		{
		char buff1[100], buff2[100];
		sprintf(buff1, "nopt %s -o junk.%d", aoutname, pid);
		sprintf(buff2, "mv junk.%d %s", pid, aoutname);
		if( sys(buff1) || sys(buff2) )
			err("bad optimization");
		}
#endif

if(verbose)
	fprintf(diagfile, "\n");
}

/* Process control and Shell-simulating routines */

sys(str)
char *str;
{
register char *s, *t;
char *argv[100];
char *inname, *outname;
int append;
int waitpid;
int argc;


if(debugflag)
	fprintf(diagfile, "%s\n", str);
inname  = NULL;
outname = NULL;
argv[0] = shellname;
argc = 1;

t = str;
while( isspace(*t) )
	++t;
while(*t)
	{
	if(*t == '<')
		inname = t+1;
	else if(*t == '>')
		{
		if(t[1] == '>')
			{
			append = YES;
			outname = t+2;
			}
		else	{
			append = NO;
			outname = t+1;
			}
		}
	else
		argv[argc++] = t;
	while( !isspace(*t) && *t!='\0' )
		++t;
	if(*t)
		{
		*t++ = '\0';
		while( isspace(*t) )
			++t;
		}
	}

if(argc == 1)   /* no command */
	return(-1);
argv[argc] = 0;

if((waitpid = fork()) == 0)
	{
	if(inname)
		if(freopen(inname, "r", stdin) == NULL)
			fatalstr("Cannot open %s", inname);
	if(outname)
		if(freopen(outname, (append ? "a" : "w"), stdout) == NULL)
			fatalstr("Cannot open %s", outname);
	enbint(SIG_DFL);

	texec(argv[1], argv);

	fatalstr("Cannot load %s", argv[1]);
	}

return( await(waitpid) );
}





#include "errno.h"

/* modified version from the Shell */
texec(f, av)
char *f;
char **av;
{
extern int errno;

execv(f, av+1);

if (errno==ENOEXEC)
	{
	av[1] = f;
	execv(shellname, av);
	fatal("No shell!");
	}
if (errno==ENOMEM)
	fatalstr("%s: too large", f);
}






done(k)
int k;
{
static int recurs	= NO;

if(recurs == NO)
	{
	recurs = YES;
	rmfiles();
	}
exit(k);
}






enbint(k)
int (*k)();
{
if(sigivalue == 0)
	(void) signal(SIGINT,k);
if(sigqvalue == 0)
	(void) signal(SIGQUIT,k);
if(sighvalue == 0)
	(void) signal(SIGHUP,k);
if(sigtvalue == 0)
	(void) signal(SIGTERM,k);
}




intrupt()
{
done(2);
}


#ifdef PSIGNAL
/*
 * Fancy 4.2 BSD signal printing stuff.
 */
char harmless[NSIG] = { 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1 };
#endif


await(waitpid)
int waitpid;
{

#ifdef PSIGNAL
extern char *sys_siglist[];
union wait status;
#else PSIGNAL
int status;
#endif PSIGNAL

int w;

enbint(SIG_IGN);
while ( (w = wait(&status)) != waitpid)
	if(w == -1)
		fatal("bad wait code");
enbint(intrupt);

#ifdef PSIGNAL
if(status.w_termsig)
	{
	debugflag = 0;	/* Prevent us from dumping core ourselves */
	if(status.w_termsig != SIGINT && status.w_termsig < NSIG)
		fprintf(diagfile, "%s%s\n", sys_siglist[status.w_termsig],
			status.w_coredump ? " -- core dumped" : "");
	if(status.w_termsig < NSIG && ! harmless[status.w_termsig])
		fatal("see a system manager");
	else
		done(3);
	}
return(status.w_retcode);
#else PSIGNAL
if(status & 0377)
	{
	if(status != SIGINT)
		fprintf(diagfile, "Termination code %d\n", status);
	done(3);
	}
return(status>>8);
#endif PSIGNAL
}

/* File Name and File Manipulation Routines */

unreadable(s)
register char *s;
{
register FILE *fp;

if(fp = fopen(s, "r"))
	{
	fclose(fp);
	return(NO);
	}

else
	{
	fprintf(diagfile, "Error: Cannot read file %s\n", s);
	return(YES);
	}
}



stupid(s)
char *s;
{
char c;
extern char *index();

if( (c = dotchar(s))
  && index("focsreF", c)
  && access(s, 0) == 0 )
	{
	fprintf(diagfile, "Loading on %s would destroy it\n", s);
	return(YES);
	}
return(NO);
}



clf(p)
FILEP *p;
{
if(p!=NULL && *p!=NULL && *p!=stdout)
	{
	if(ferror(*p))
		fatal("writing error");
	fclose(*p);
	}
*p = NULL;
}

rmfiles()
{
rmf(textfname);
rmf(asmfname);
rmf(initfname);
rmf(asmpass2);
#if TARGET == INTERDATA
	rmf(setfname);
#endif
}








/* return -1 if file does not exist, 0 if it is of zero length
   and 1 if of positive length
*/
content(filename)
char *filename;
{
#ifdef VERSION6
	struct stat
		{
		char cjunk[9];
		char size0;
		int size1;
		int ijunk[12];
		} buf;
#else
	struct stat buf;
#endif

if(stat(filename,&buf) < 0)
	return(-1);
#ifdef VERSION6
	return(buf.size0 || buf.size1);
#else
	return( buf.st_size > 0 );
#endif
}




crfnames()
{
fname(textfname, "x");
fname(asmfname, "s");
fname(asmpass2, "a");
fname(initfname, "d");
fname(sortfname, "S");
fname(objfdefault, "o");
fname(prepfname, "p");
fname(optzfname, "z");
fname(setfname, "A");
}




rmf(fn)
register char *fn;
{
/* if(!debugflag && fn!=NULL && *fn!='\0') */

if(fn!=NULL && *fn!='\0')
	unlink(fn);
}





LOCAL fname(name, suff)
char *name, *suff;
{
sprintf(name, "%s/%s%d.%s", _PATH_TMP, temppref, pid, suff);
}




dotchar(s)
register char *s;
{
for( ; *s ; ++s)
	if(s[0]=='.' && s[1]!='\0' && s[2]=='\0')
		return( s[1] );
return(NO);
}



char *lastfield(s)
register char *s;
{
register char *t;
for(t = s; *s ; ++s)
	if(*s == '/')
		t = s+1;
return(t);
}



char *lastchar(s)
register char *s;
{
while(*s)
	++s;
return(s-1);
}

char *setdoto(s)
register char *s;
{
*lastchar(s) = 'o';
return( lastfield(s) );
}



badfile(s)
char *s;
{
fatalstr("cannot open intermediate file %s", s);
}



ptr ckalloc(n)
int n;
{
ptr p;
extern char *calloc();

if( p = (ptr) calloc(1, (unsigned) n) )
	return(p);

fatal("out of memory");
/* NOTREACHED */
}





char *copyn(n, s)
register int n;
register char *s;
{
register char *p, *q;

p = q = (char *) ckalloc(n);
while(n-- > 0)
	*q++ = *s++;
return(p);
}



char *copys(s)
char *s;
{
return( copyn( strlen(s)+1 , s) );
}





oneof(c,s)
register c;
register char *s;
{
while( *s )
	if(*s++ == c)
		return(YES);
return(NO);
}



nodup(s)
char *s;
{
register char **p;

for(p = loadargs ; p < loadp ; ++p)
	if( !strcmp(*p, s) )
		return(NO);

return(YES);
}



static fatal(t)
char *t;
{
fprintf(diagfile, "Compiler error in file %s: %s\n", infname, t);
if(debugflag)
	abort();
done(1);
exit(1);
}




static fatali(t,d)
char *t;
int d;
{
char buff[100];
sprintf(buff, t, d);
fatal(buff);
}




static fatalstr(t, s)
char *t, *s;
{
char buff[100];
sprintf(buff, t, s);
fatal(buff);
}
err(s)
char *s;
{
fprintf(diagfile, "Error in file %s: %s\n", infname, s);
}

