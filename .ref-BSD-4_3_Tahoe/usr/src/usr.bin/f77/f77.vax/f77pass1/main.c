/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.3 (Berkeley) 1/3/88";
#endif /* not lint */

/*
 * main.c
 *
 * Main routine for the f77 compiler, pass 1, 4.2 BSD.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	main.c,v $
 * Revision 5.2  85/08/10  04:57:16  donn
 * Jerry Berkman's changes to ifdef 66 code and add -r8/double flag..
 * 
 * Revision 5.1  85/08/10  03:48:26  donn
 * 4.3 alpha
 * 
 * Revision 3.2  85/01/14  04:21:31  donn
 * Added changes to implement Jerry's '-q' option.
 * 
 * Revision 3.1  84/10/29  05:47:03  donn
 * Added Jerry Berkman's change to line buffer stderr.
 * 
 */

char *xxxvers = "\n@(#) FORTRAN 77 PASS 1, VERSION 2.10,  16 AUGUST 1980\n";

#include "defs.h"
#include <signal.h>

#ifdef SDB
#	include <a.out.h>
#	ifndef N_SO
#		include <stab.h>
#	endif
#endif


LOCAL char *textname = "";
LOCAL char *asmname = "";
LOCAL char *initname = "";


extern intexit();

flag namesflag = YES;



main(argc, argv)
int argc;
char **argv;
{
char *s;
int k, retcode, *ip;
FILEP opf();
int flovflo();

#define DONE(c)	{ retcode = c; goto finis; }

signal(SIGFPE, flovflo);  /* catch overflows */
signal(SIGINT, intexit);

#if HERE == PDP11
	ldfps(01200);	/* trap on overflow */
#endif


setlinebuf(diagfile);

--argc;
++argv;

while(argc>0 && argv[0][0]=='-')
	{
	for(s = argv[0]+1 ; *s ; ++s) switch(*s)
		{
		case 'w':
			if(s[1]=='6' && s[2]=='6')
				{
				ftn66flag = YES;
				s += 2;
				}
			else
				nowarnflag = YES;
			break;

		case 'U':
			shiftcase = NO;
			break;

		case 'u':
			undeftype = YES;
			break;

		case 'O':
			optimflag = YES;
			break;

		case 'd':
			debugflag[0] = YES;

			while (*s == 'd' || *s == ',')
				{
				k = 0;
				while( isdigit(*++s) )
					k = 10*k + (*s - '0');
				if(k < 0 || k >= MAXDEBUGFLAG)
					fatali("bad debug number %d",k);
				debugflag[k] = YES;
				}
			break;

		case 'p':
			profileflag = YES;
			break;

		case '8':
			dblflag = YES;
			break;

		case 'C':
			checksubs = YES;
			break;

#ifdef ONLY66
		case '6':
			no66flag = YES;
			noextflag = YES;
			break;
#endif

		case '1':
			onetripflag = YES;
			break;

#ifdef SDB
		case 'g':
			sdbflag = YES;
			break;
#endif

		case 'q':
			namesflag = NO;
			break;

		case 'N':
			switch(*++s)
				{
				case 'q':
					ip = &maxequiv; goto getnum;
				case 'x':
					ip = &maxext; goto getnum;
				case 's':
					ip = &maxstno; goto getnum;
				case 'c':
					ip = &maxctl; goto getnum;
				case 'n':
					ip = &maxhash; goto getnum;

				default:
					fatali("invalid flag -N%c", *s);
				}
		getnum:
			k = 0;
			while( isdigit(*++s) )
				k = 10*k + (*s - '0');
			if(k <= 0)
				fatal("Table size too small");
			*ip = k;
			break;

		case 'i':
			if(*++s == '2')
				tyint = TYSHORT;
			else if(*s == '4')
				{
				shortsubs = NO;
				tyint = TYLONG;
				}
			else if(*s == 's')
				shortsubs = YES;
			else
				fatali("invalid flag -i%c\n", *s);
			tylogical = tyint;
			break;

		default:
			fatali("invalid flag %c\n", *s);
		}
	--argc;
	++argv;
	}

if(argc != 4)
	fatali("arg count %d", argc);
textname = argv[3];
initname = argv[2];
asmname = argv[1];
asmfile  = opf(argv[1]);
initfile = opf(argv[2]);
textfile = opf(argv[3]);

initkey();
if(inilex( copys(argv[0]) ))
	DONE(1);
if(namesflag == YES)
	fprintf(diagfile, "%s:\n", argv[0]);

#ifdef SDB
filenamestab(argv[0]);
#endif

fileinit();
procinit();
if(k = yyparse())
	{
	fprintf(diagfile, "Bad parse, return code %d\n", k);
	DONE(1);
	}
if(nerr > 0)
	DONE(1);
if(parstate != OUTSIDE)
	{
	warn("missing END statement");
	endproc();
	}
doext();
preven(ALIDOUBLE);
prtail();
#if FAMILY==PCC
	puteof();
#endif

if(nerr > 0)
	DONE(1);
DONE(0);


finis:
	done(retcode);
}



done(k)
int k;
{
  static char *ioerror = "i/o error on intermediate file %s\n";

  if (textfile != NULL && textfile != stdout)
    {
      if (ferror(textfile))
	{
	  fprintf(diagfile, ioerror, textname);
	  k = 3;
	}
      fclose(textfile);
    }

  if (asmfile != NULL && asmfile != stdout)
    {
      if (ferror(asmfile))
	{
	  fprintf(diagfile, ioerror, asmname);
	  k = 3;
	}
      fclose(asmfile);
    }

  if (initfile != NULL && initfile != stdout)
    {
      if (ferror(initfile))
	{
	  fprintf(diagfile, ioerror, initname);
	  k = 3;
	}
      fclose(initfile);
    }

  rmtmpfiles();

  exit(k);
}


LOCAL FILEP opf(fn)
char *fn;
{
FILEP fp;
if( fp = fopen(fn, "w") )
	return(fp);

fatalstr("cannot open intermediate file %s", fn);
/* NOTREACHED */
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




flovflo()
{
err("floating exception during constant evaluation");
#if HERE == VAX
	fatal("vax cannot recover from floating exception");
	rmtmpfiles();
	/* vax returns a reserved operand that generates
	   an illegal operand fault on next instruction,
	   which if ignored causes an infinite loop.
	*/
#endif
signal(SIGFPE, flovflo);
}



rmtmpfiles()
{
  close(vdatafile);
  unlink(vdatafname);
  close(vchkfile);
  unlink(vchkfname);
  close(cdatafile);
  unlink(cdatafname);
  close(cchkfile);
  unlink(cchkfname);
}



intexit()
{
  done(1);
}
