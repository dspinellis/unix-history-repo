#ifndef lint
static char Notice[] = "Copyright (c) 1985 Adobe Systems Incorporated";
static char *RCSID="$Header: psrev.c,v 2.1 85/11/24 11:51:02 shore Rel $";
#endif
/* psrev.c
 *
 * Copyright (c) 1985 Adobe Systems Incorporated
 *
 * page reversal and selection filter
 *
 * Original Version: Tom Malloy
 * Edit History:
 * Andrew Shore: Fri Nov 22 11:20:20 1985
 * End Edit History.
 *
 * RCSLOG:
 * $Log:	psrev.c,v $
 * Revision 2.1  85/11/24  11:51:02  shore
 * Product Release 2.0
 * 
 * Revision 1.4  85/11/22  11:31:05  shore
 * Last line of trailer was dropped if it didn't end in a newline
 * 
 * Revision 1.3  85/11/20  00:52:21  shore
 * Support for System V
 * getopt!
 * made lint a little happier
 * 
 * Revision 1.2  85/05/14  11:26:38  shore
 * 
 * 
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "transcript.h"

#define SCVERID "%!PS-Adobe-"
#define ichMaxPage 7
#define SCPAGE "%%Page:"
#define ichMaxTrailer 9
#define SCTRAILER "%%Trailer"

#define ipgMax 2000	/* Maximum number of pages - first page index is 0 */
#define irngMax 30	/* Maximum number of intervals in a page range spec */

private struct Range	/* continuous interval of pages to be printed */
			/* from a page range specification (i.e. -s) */
			/* page numbers match the second parameter */
			/* of the "%%Page: " comment */
    { int	pgnFst;
    int		pgnLst;
    };
private struct Params	/* interesting info collected from command line */
    { int	fReverse; /* true => reverse page order */
    int		fRemove;  /* true => remove input file */
    char	*scSrcFile; /* c-string containing source file name */
    char	*scDstFile; /* c-string containing destination file name */
    int		irngMac;  /* number of intervals in rgrange */
    struct Range rgrange[irngMax]; /* array of intervals of pages */
                                     /* to be printed.  One entry per */
                                     /* entry in the "-s" parameter */
    };

private struct Psd	/* PS file descriptor */
    { FILE	*fp;	/* unix file descriptor  of source */
    FILE	*fpTemp;/* temp file descriptor.  Contains a 
			   copy of source when fd is not a regular file */
    long int	posLineFst, posMac;
			/* file positions - current, beginning of line, eof */
    };

private struct stat S;
private char *prog;


/* Reads a signed integer from c-string */
/* Input is a pointer to pointer to string */
/* On return, it points to first character following the parsed numeral */
private IntGet(prgch)
char	**prgch;
{ int	i, fNeg;
char	*rgch, ch;

i = 0; rgch = *prgch;
if ((ch = *rgch) == '-') 
     { fNeg = TRUE; rgch++;} 
else { if (ch == '+') rgch++; fNeg = FALSE;}
while (((ch = *rgch) >= '0') && (ch <= '9'))
    { rgch++; i = 10*i + (ch - '0');}
*prgch = rgch;
return(fNeg ? -i : i);
}


/* Parses the "-s" parameter.  Fills the array params.rgrange */
/* with a page interval range for each entry int the comma separated list */
/* *prgch points at the character following the "s" command */
private SetPageRanges(rgch, pparams)
char	*rgch;
struct Params *pparams;
{ char	ch;
char	*scError;
struct Range *rgrange;
#define pgnMax 30000

scError = "Syntax error in page range specification while parsing ";
rgrange = pparams->rgrange; 
while (TRUE)
    {
    if ((ch = *rgch) == '-')
        { rgrange[pparams->irngMac].pgnFst = 1;
        rgch++;
        rgrange[pparams->irngMac].pgnLst = IntGet(&rgch);
        }
    else
        { 
        rgrange[pparams->irngMac].pgnFst = IntGet(&rgch);
        if ((ch = *rgch) == '-') 
            { rgch++;
            if ((( ch = *rgch) == ',') || (ch == '\0'))
                 rgrange[pparams->irngMac].pgnLst = pgnMax;
            else rgrange[pparams->irngMac].pgnLst = IntGet(&rgch);
            }
        else if ((( ch = *rgch) == ',') || (ch == '\0'))
             { rgrange[pparams->irngMac].pgnLst = 
                     rgrange[pparams->irngMac].pgnFst;
	     }
        else if (rgrange[pparams->irngMac].pgnFst > 0)
             { fprintf(stderr, "%s%s\n", scError, rgch);
             exit(4);
             }
        }
    if ((rgrange[pparams->irngMac].pgnFst == 0) || 
        (rgrange[pparams->irngMac].pgnLst == 0))
            { fprintf(stderr, "%s%s\n", 
                              scError, rgch);
             exit(4);
	    }
    if (++pparams->irngMac >= irngMax)
        { fprintf(stderr, "Too many intervals in page range specificaiton\n");
        exit(4);
	}
    if ((ch = *rgch) == ',') 
        rgch++;
    else if (ch == '\0')
         break;
    else return;
    }
}


/* Returns TRUE if ipg is to be printed; i.e. if it is in one of the
   intervals in pparams->rgrange[0..pparams->irngMac) */
private FPageInRange(pgn, pparams)
int pgn;
struct Params *pparams;
{ int	irng;
struct Range *rgrange;
#define ichTMax 50
#define ichColon 6

if (pparams->irngMac == 0) return(TRUE);
rgrange = pparams->rgrange;
for (irng = 0; irng < pparams->irngMac; irng++)
    { if ((pgn >= rgrange[irng].pgnFst) 
            && (pgn <= rgrange[irng].pgnLst))
        return(TRUE);
    }
return(FALSE);
}


/* Reads a line from the source PS file */
/* Fills the c-string, scDst, with the first ichMacRead characters */
/* of the line.  Returns TRUE if it hits end of file, FALSE otherwise */
private FEofReadSc(scDst, ichMacRead, ppsd)
char	scDst[];
int	ichMacRead;
struct Psd *ppsd;
{ int	ich;
int	ch;
static int ateof = FALSE;

scDst[0] = '\0';
ppsd->posLineFst = ftell(ppsd->fp);
if (ateof) return(TRUE);
for (ich = 0; ich < ichMacRead; ich++)
    { scDst[ich] = ch = getc(ppsd->fp);
    if ((ch == '\n') || (ch == EOF)) break;
    }
scDst[ich] = '\0';
while ((ch != '\n') && (ch != EOF)) 
    { ch = getc(ppsd->fp); 
    }
if (ch == EOF) {
    ateof = TRUE;
    return (ich == 0);
}
return(FALSE);
}

/* Returns TRUE if the source is a conforming PS file; */
/* i.e. first line of the source PS file contains "%!PS-Adobe-" */
private FConforming(ppsd)
struct Psd *ppsd;
{ 
#define ichMaxVerId 11
char	scVerIdT[ichMaxVerId+1];

if (!FEofReadSc(scVerIdT, ichMaxVerId, ppsd)) {
    if (strcmp(scVerIdT, SCVERID) == 0) return(TRUE);
}
return(FALSE);
}

/* Finds the beginning of pages.  Loads rgposPage with the file position */
/* of the "%%Page:" comment line */
private FindPageStarts(ppsd, rgposPage, rgpgnPage, pipgMac)
struct Psd *ppsd;
long	rgposPage[];
int	rgpgnPage[];
int	*pipgMac;
{ 
#define ichMaxScT 40
char scT[ichMaxScT+1];
char *scT1;

while (1)
    { if (FEofReadSc(scT, ichMaxScT, ppsd))
        { rgposPage[*pipgMac] = ppsd->posLineFst; break;}
    if (strncmp(scT, SCPAGE, ichMaxPage) == 0)
        { rgposPage[*pipgMac] = ppsd->posLineFst;
        scT1 = &scT[ichColon+1];
        /* skip blanks */
        while (*scT1 == ' ') scT1++;
        /* skip label */
        while ((*scT1 != ' ') && (*scT1 != '\n')) scT1++;
        /* skip blanks */
        while (*scT1 == ' ') scT1++;
        rgpgnPage[*pipgMac] = IntGet(&scT1);
        if ((*pipgMac) < (ipgMax-1))
            (*pipgMac)++;
        else break;
        }
    else if (strncmp(scT, SCTRAILER, ichMaxTrailer) == 0)
        { rgposPage[*pipgMac] = ppsd->posLineFst;
        break;
	}
    }
while (! FEofReadSc(scT, 1, ppsd)) ;
ppsd->posMac = ppsd->posLineFst;
}

/* Move the bytes from posFst to posLim from source to destination PS file */
private MovePage(fdPsSrc, posFst, posLim, fdPsDst)
int	fdPsSrc;
long int posFst, posLim;
int	fdPsDst;
{ 
#define ichMaxBuf 4096
char	rgchBuf[ichMaxBuf];
int cchRead, cchMove, cchWrite;
register unsigned nbr;

VOIDC lseek(fdPsSrc, posFst, 0);
cchMove = posLim - posFst;
while (cchMove > 0)
    {	nbr = (unsigned) (cchMove < ichMaxBuf) ? cchMove : ichMaxBuf;
	cchRead = read(fdPsSrc, rgchBuf, nbr);
    if ((cchRead != ichMaxBuf) && (cchRead != cchMove))
        { fprintf(stderr,"%s: problem reading source file\n",prog);
	  exit(2);
	}
    cchWrite = write(fdPsDst, rgchBuf, (unsigned) cchRead);
    if (cchWrite != cchRead)
        { fprintf(stderr,"%s: problem writing new file\n",prog);
	  exit(2);
	}
    cchMove = cchMove - cchRead;
    }
}

#define ARGS "Rrp:s:"

main(argc, argv)
int	argc;
char  *argv[]; 
{
    int	ipgMac, ipgT;
    struct Psd psd;
    struct	Params	params;
    long rgposPage[ipgMax+1];
    int	rgpgnPage[ipgMax+1];
    FILE	*fpPsDst;
    char	scTemp[50], *tempdir;
    int	fSpooledPage;
    int	fdPsSrc, fdPsDst;
    int	ch;
    register int argp;
    extern int optind;
    extern char *optarg;

    prog = *argv;
    if ((tempdir = envget("PSTEMPDIR")) == NULL) tempdir = TempDir;
    VOIDC mstrcat(scTemp, tempdir, REVTEMP,sizeof scTemp);
    ipgMac = 0;
    params.scSrcFile = NULL;
    params.scDstFile = NULL;
    params.fReverse = TRUE;
    params.fRemove = FALSE;
    params.irngMac = 0;

    psd.fp = NULL;
    psd.fpTemp = NULL;

    /* process the command line arguments */
    while ((argp = getopt(argc,argv,ARGS)) != EOF) {
	switch (argp) {
	    case 'r':
		params.fRemove = TRUE;
		break;
	    case 'R':
		params.fReverse = FALSE;
		break;
	    case 'p':
		params.scDstFile = optarg;
		break;
	    case 's':
		SetPageRanges(optarg,&params);
		break;
	    case '?':
	    default:
		fprintf(stderr,"%s: unknown option -%c\n",prog,argp);
		exit(2);
	}
    }
    if (optind < argc) {
	params.scSrcFile = argv[optind];
	if ((psd.fp = fopen(argv[optind],"r")) == NULL) {
	    fprintf(stderr,"%s: can't open %s\n",prog,params.scSrcFile);
	    exit(2);
	}
    }
    else psd.fp = stdin;

    VOIDC fstat(fileno(psd.fp), &S);
    /* if its not a regular file then copy it to a temp file and use */
    /* the temp from now on */
    if ((S.st_mode & S_IFMT) != S_IFREG) {
	VOIDC mktemp(scTemp);
	if ((psd.fpTemp = fopen(scTemp, "w")) == NULL) {
	    fprintf(stderr,"%s: could not open temp file\n",prog);
	    exit(2);
	}
	while ((ch = getc(psd.fp)) != EOF) putc(ch, psd.fpTemp);
	VOIDC fclose(psd.fpTemp);
	psd.fpTemp = fopen(scTemp, "r");
	VOIDC unlink(scTemp);
	psd.fp = psd.fpTemp;
    }

    if (FConforming(&psd)) {
	FindPageStarts(&psd, rgposPage, rgpgnPage, &ipgMac);
	if (ipgMac == ipgMax) {
	    fprintf(stderr, "%s: Too many pages in PS file, sorry\n",prog);
	    exit(1);
	}
	if (rgposPage[ipgMac] == psd.posMac) {
	    fprintf(stderr,"%s: PS file does not contain a conforming trailer\n",prog);
	    exit(1);
	}
	/* remove the input file if it was requested from */
	/* command line */
	if (params.fRemove) {
	    VOIDC unlink(params.scSrcFile);
	    params.scSrcFile = NULL;
	}
	if (params.scDstFile == NULL) {
	    fpPsDst = stdout;
	}
	else {
	    /* remove the input file if output file = input file */
	    if ((params.scSrcFile != NULL) 
	    && (strcmp(params.scSrcFile, params.scDstFile) == 0))
		VOIDC unlink(params.scSrcFile);
	    fpPsDst = fopen(params.scDstFile, "w");
	    if (fpPsDst == NULL) {
		fprintf(stderr, "%s: could not open output file %s\n", 
			    prog, params.scDstFile);
		exit(1);
	    }
	}
	fdPsSrc = fileno(psd.fp);
	fdPsDst = fileno(fpPsDst);
	MovePage(fdPsSrc, 0L, rgposPage[0], fdPsDst);
	fSpooledPage = FALSE;
	ipgT = params.fReverse ? ipgMac - 1 : 0;
	while (TRUE) {
	    if (FPageInRange(rgpgnPage[ipgT], &params)) {
		fSpooledPage = TRUE;
		MovePage(fdPsSrc,rgposPage[ipgT], rgposPage[ipgT+1], fdPsDst);
	    }
	    if (params.fReverse) {
		if (ipgT == 0) break;
		ipgT--;
	    }
	    else {
		ipgT++;
		if (ipgT == ipgMac) break;
	    }
	}
	if (!fSpooledPage)
	    fprintf(stderr, "%s: No pages in specified range!\n",prog);
	MovePage(fdPsSrc, rgposPage[ipgMac], psd.posMac, fdPsDst);
	VOIDC fclose(fpPsDst);
	if (psd.fpTemp != NULL) VOIDC fclose(psd.fpTemp);
	else VOIDC fclose(psd.fp);
	exit(0);
    }
    fprintf(stderr,"%s: PS file does not begin with a version identifier\n",
    	prog);
    exit(1);
}
