/*	@(#)psgrind.c	1.3 %G%	*/
/*
 *  psgrind - quick hack to grind C source files directly into
 *  PostScript.
 *
 *  John Coker
 *  University of California, Berkeley
 *
 *  The basis for this program is the enscript utility provided
 *  by TranScript to driver the Apple LaserWriter printer.  This
 *  code was taken and mangled without permission of any kind;
 *  don't tell anyone.  -john
 */

#define POSTSCRIPTPRINTER "gp"

#define HEADERFONT	"Helvetica-Bold"
#define BODYFONT 	"Helvetica"
#define KWORDFONT 	"Helvetica-Bold"
#define COMMENTFONT	"Helvetica-Oblique"
#define LITERALFONT  	"Courier"

#ifndef PSGRINDPRO
#define PSGRINDPRO	"/psgrind.pro"
#endif !PSGRINDPRO

#ifndef PSGRINDTEMP
#define PSGRINDTEMP	"/GRXXXXXX"
#endif !PSGRINDTEMP

#define UperInch 1440
#define PtsPerInch 72
#define UperPt 20
#define TruePageWidth  (UperInch*17/2)
#define PageWidth  (UperInch*(4*17-3)/8)
#define PageLength (UperInch*(8*11-3)/8)
#define TruePageLength (UperInch*11)

#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "transcript.h"

#define LPR "lpr"
#define REVERSE "psrev"

#define MAXBAD 20	/* number of bad chars to pass before complaint */

private struct stat S;
char   *ctime ();
char   *getenv ();
char   *rindex ();

#define FSIZEMAX 256		/* number of chars per font */

/* the layout of a font information block */
struct font {
    char    name[100];		/* PostScript font name */
    int     dsize;		/* size */
    int     Xwid[FSIZEMAX];	/* X widths for each character */
};

private struct font fonts[16];		/* 16 possible fonts at one time */
private int nf = 0;			/* number of fonts known about */

private int HeaderFont = -1;		/* handle for header font */
private int BodyFont = -1;		/* handle for body font */
private int KwordFont = -1;		/* handle for keyword font */
private int LiteralFont = -1;		/* handle for literal font */
private int CommentFont = -1;		/* handle for comment font */

private int TabWidth = TruePageWidth / 10;	/* width of a tab */
private int BSWidth;				/* width of a backspace */

private int UperLine = UperInch / 7;
private int UperHLine = UperInch / 7;

private char *prog;		/* program name argv[0] */
private char TempName[100];	/* name of temporary PostScript file */
private char OutName[256] = 0;	/* filename for disk output */
private int PipeOut = 0;	/* true if output to stdout (-p -) */
private int ListOmitted = 0;	/* list omitted chars on the tty */
private int LPTsimulate = 0;	/* true if an lpt should be simulated */
private int LinesLeft = 64;	/* lines left on page when in LPT mode */
private int col;		/* column number on current line */
private int LineNo;		/* line number in current file */
private int SeenText = 1;	/* true if seen some text on this page */
private int OutOnly = 0;	/* true if PS file only wanted */
private int Rotated = 0;	/* true if the page is to be rotated */
private int Reverse = 0;	/* output should be piped to psrev */
private char *PageSpec = 0;	/* ditto */
private int PreFeed = 0;	/* true if prefeed should be enabled */
private int TwoColumn = 0;	/* true of if in two-column mode */
private int FirstCol = 1;	/* true if we're printing column 1 */
private int NoTitle = 0;	/* true if title line is to be suppressed */
private int Cvted = 0;		/* true if converted a file to PS format */

private int IgnoreGarbage = 0;	/* true if garbage should be ignored */
private int SeenFile = 0;	/* true if a file has been processed */
private int SeenFont = 0;	/* true if we've seen a font request */
private int ScannedFonts = 0;	/* true if we've scanned the font file */
private char *FileName = 0;	/* name of file currently being PSed */
private char *FileDate = 0;	/* last mod date of file being PSed */
private char DateStr[27];	/* thanks, but no thanks ctime! */

private char *spoolJobClass = 0;
private char *spoolJobName = 0;
private char *PrinterName = 0;
private int spoolNotify = 0;
private int spoolNoBurst = 0;
private int spoolCopies = 1;

private char tempstr[256];	/* various path names */

private int CurFont;		/* current Font */
private int LastFont;		/* previous Font */

private int cX, cY;		/* current page positions */
private int dX, dY;		/* desired page positions */
private int lX, lY;		/* page positions of the start of the line */
private int crX, crY;		/* X and Y increments to apply to CR's */

#define None	0
#define RelX	1
#define	RelY	2
#define RelXY	3
#define AbsX	4
#define AbsY	8
#define AbsXY	12

private int movepending;	/* moveto pending coords on stack */
private int showpending;	/* number of characters waiting to be shown */
private char *UsersHeader = 0;	/* user specified heading */
private char *Header = 0;	/* generated header (usually FileName) */
private int Page = 0;		/* current page number */
private int TotalPages = 0;	/* total number of pages printed */
private int TruncChars = 0;	/* number of characters truncated */
private int UndefChars = 0;	/* number of characters skipped because
				   they weren't defined in some font */
private int BadChars   = 0;	/* number of bad characters seen so far */

private FILE *OutFile = 0;	/* output ps file */

/* decode a fontname string - e.g. Courier10 Helvetica-Bold12 */
private decodefont (name, f)
register char  *name;
register struct font *f; {
    register char *d, *p;

    SeenFont++;
    if (ScannedFonts) {
	fprintf(stderr,"Fonts must be specified before any files are processed\n");
	exit(1);
    }
    p = name;
    d = f->name;
    while (isascii(*p) && (isalpha(*p) || (*p == '-'))) {*d++ = *p++;}
    *d++ = '\0';
    if (isascii(*p) && isdigit(*p)) {
	f->dsize = 0;
	do
	    f->dsize = f->dsize * 10 + *p++ - '0';
	while ('0' <= *p && *p <= '9');
    }
    if (*p || !f->dsize || !f->name[0]) {
	fprintf (stderr, "Poorly formed font name: \"%s\"\n", name);
	exit (1);
    }
}


#define NOTDEF 0x8000
#define ForAllFonts(p) for(p = &fonts[nf-1]; p >= &fonts[0]; p--)

/* Scan the font metrics directory looking for entries that match the
 * entries in ``fonts''.  For entries
 * that are found the data in the font description is filled in,
 * if any are missing, it dies horribly.
 */
private ScanFont () {
    register struct font   *f;
    register FILE *FontData;		/* afm file */
    char    *c;
    int     ccode, cwidth, inChars;
    char   *MetricsDir = (char *) getenv ("METRICS");
    char    FontFile[512];		/* afm file name */
    char    afmbuf[BUFSIZ];

    if (MetricsDir == 0)
	MetricsDir = LibDir;

    if (!SeenFont & Rotated & TwoColumn) {
	fonts[HeaderFont].dsize = 10;
	fonts[BodyFont].dsize = 7;
	fonts[KwordFont].dsize = 7;
	fonts[LiteralFont].dsize = 8;
	fonts[CommentFont].dsize = 7;
    }

    /* loop through fonts, find and read metric entry in dir */
    ForAllFonts (f) {
	mstrcat(FontFile, MetricsDir, "/", sizeof FontFile);
	mstrcat(FontFile, FontFile, f->name, sizeof FontFile);
	mstrcat(FontFile, FontFile, ".afm", sizeof FontFile);
	if ((FontData = fopen(FontFile,"r")) == NULL){
	    fprintf(stderr,"Can't open font metrics file %s\n",FontFile);
	    exit(1);
	}
	/* read the .afm file to get the widths */
	for (ccode = 0; ccode < FSIZEMAX; ccode++) f->Xwid[ccode] = NOTDEF;

	inChars = 0;
	while(fgets(afmbuf, sizeof afmbuf, FontData) != NULL) {
	    /* strip off newline */
	    if ((c = index(afmbuf, '\n')) == 0) {
		fprintf(stderr, "AFM file %s line too long %s\n", FontFile, afmbuf);
		exit(1);
	    }
	    *c = '\0';
	    if (*afmbuf == '\0') continue;
	    if (strcmp(afmbuf, "StartCharMetrics") == 0) {
		inChars++;
		continue;
	    }
	    if (strcmp(afmbuf, "EndCharMetrics") == 0) break;
	    if (inChars == 1) {
		if (sscanf(afmbuf, "C %d ; WX %d ;",&ccode,&cwidth) != 2) {
		    fprintf(stderr,"Trouble with AFM file %s\n",FontFile);
		    exit(1);
		}
		/* get out once we see an unencoded char */
		if (ccode == -1) break;
		if (ccode > 255) continue;
		f->Xwid[ccode] = (cwidth * f->dsize * UperPt) / 1000;
		continue;
	    }
	}
    fclose (FontData);
    }

    TabWidth = fonts[BodyFont].Xwid['0'] * 8; /* 8 * figure width */
    BSWidth = fonts[BodyFont].Xwid[' ']; /* space width */ 

    UperLine = (fonts[BodyFont].dsize + 1) * UperPt;

    if (LPTsimulate)
	UperHLine = UperLine;
    else
	UperHLine = (fonts[HeaderFont].dsize + 1) * UperPt;

    crX = 0;
    crY = -UperLine;
}

/* Return a font number for the font with the indicated name
 * and size.  Adds info to the font list for the eventual search.
 */
private DefineFont (name, size)
char   *name; {
    register struct font   *p;
    p = &fonts[nf];
    strcpy (p->name, name);
    p->dsize = size;
    return (nf++);
}

/* add a shown character to the PS file */
private OUTputc (c)
unsigned char    c; {
    if (showpending == 0) {putc('(', OutFile); showpending++;}
    if (c == '\\' || c=='(' || c == ')') putc('\\', OutFile);
    if ((c > 0176) || (c < 040)) {
	putc('\\',OutFile);
	putc((c >> 6) +'0',OutFile);
	putc(((c >> 3) & 07)+'0', OutFile);
	putc((c & 07)+'0',OutFile);
    }
    else putc (c, OutFile);
}

/* Set the current font */
private SetFont (f) {
    FlushShow();
    LastFont = CurFont;
    fprintf(OutFile, "%d F\n", CurFont = f);
}

/* Reset to previous font */
private PrevFont () {
    int	temp;

    FlushShow();
    temp = CurFont;
    CurFont = LastFont;
    LastFont = temp;
    fprintf(OutFile, "%d F\n", CurFont);
}

/* put a character onto the page at the desired X and Y positions.
 * If the current position doesn't agree with the desired position, put out
 * movement directives.  Leave the current position updated
 * to account for the character.
 */
private ShowChar (c)
register int c; {
    register struct font   *f;
    register    nX, nY;
    static level = 0;

    level++;
    f = &fonts[CurFont];

    if (f->Xwid[c] == NOTDEF) {
	UndefChars++;
	if(ListOmitted)
	    printf("\'%c\' (%03o) not found in font %s\n",c,c,f->name);
	if(level<=1){
	    ShowChar('\\');
	    ShowChar((c>>6)+'0');
	    ShowChar(((c>>3)&07)+'0');
	    ShowChar((c&07)+'0');
	}
	level--;
	return;
    }
    nX = dX + f->Xwid[c]; /* resulting position after showing this char */
    nY = dY;

    if (c != ' ' || ((cX == dX) && (cY == dY))) {
	    /*
	     * If character doesn't fit on this line
	     * (and we're not at left margin), simulate newline
	     * and then call ourselves recursively.
	     */
	    if (((!Rotated && nX > PageWidth)
	        || (Rotated && nX > PageLength)) &&
	        dX > lX) {
		    LineNo++;
		    SeenText++;
		    dY = lY = lY + crY;
		    dX = lX = lX + crX;
		    if (((!Rotated) && (dY < UperLine))
		        || (Rotated && (dY < ((PageLength-TruePageWidth) +
						3*UperLine+480)))
		        || (LPTsimulate && (--LinesLeft <= 0)))
			    PageEject ();
		    col = 1;
		    ShowChar(c);
		    level--;
		    return;
	    }
	    if (cX != dX) {
	       if (cY != dY) {
		  FlushShow();
		  /* absolute x, relative y */
		  fprintf(OutFile,"%d %d",dX, dY);
		  movepending = AbsXY;
		  }
	       else {
		  FlushShow();
		  fprintf(OutFile,"%d",dX-cX); /* relative x */
		  movepending = RelX;
		  }
	    }
	    else if (cY != dY) {
		FlushShow();
		fprintf(OutFile,"%d",dY-cY); /* relative y */
		movepending = RelY;
		}
	    OUTputc (c);
	    showpending++;
	    cX = nX;
	    cY = nY;
	}
    dX = nX;
    dY = nY;

    level--;
}

/* put out a shown string to the PS file */
private ShowStr (s)
register char  *s; {
    while (*s) {
	if (*s >= 040) ShowChar (*s);
	s++;
    }
}

/* flush pending show */
private FlushShow() {
    if (showpending) {
       putc(')',OutFile);
	switch (movepending) {
	    case RelX:
		putc('X',OutFile);
		break;
	    case RelY:
	    	putc('Y',OutFile);
		break;
	    case AbsXY:
	    	putc('B',OutFile);
		break;
	    case None:
	    	putc('S',OutFile);
	    	break;
	}
	putc('\n',OutFile);
	movepending = None;
	showpending = 0;
    }
}

/* put out a page heading to the PS file */
private InitPage () {
    char    *basename();
    char    header[200];
    register int  OldFont = CurFont;

    TotalPages++;
    fprintf(OutFile, "%%%%Page: ? %d\n", TotalPages);
    fprintf(OutFile, "StartPage\n");
    LinesLeft = 64;
    SeenText = 0;
    cX = cY = -1;
    showpending = 0;
    FirstCol = 1;
    lX = dX = UperInch;
    lY = dY = PageLength - UperHLine * 3 / 2;
    if (Rotated) {
        fprintf(OutFile, "Landscape\n");
	lX = dX = UperInch / 4;
    }
    movepending = None;
    cX = dX; cY = dY;
    if (!NoTitle) {
	SetFont (HeaderFont);
        fprintf(OutFile, "%d %d ", cX, cY);
        movepending = AbsXY;
	if (UsersHeader) {
	    if (*UsersHeader == 0) {
		fprintf(OutFile,"()B\n");
		movepending = None;
		showpending = 0;
	    }
	    else ShowStr (UsersHeader);
	}
	else {
	    Page++;
	    if (FileName == 0)
		    sprintf(header, "Page %d", Page);
	    else
	    sprintf (header, "%s,  page %d", basename(FileName), Page);
	    ShowStr (header);
	}
	FlushShow();
	dX = lX = lX + crX * 3;
	dY = lY = lY + crY * 3;
    }
    else {
	/* fake it to force a moveto */
	cX = cY = 0;
    }
    SetFont (OldFont);
}

/* terminate a page. */
private ClosePage () {
    FlushShow();
    fprintf(OutFile,"EndPage\n");
}

/* skip to a new page */
private PageEject () {
    if (TwoColumn && FirstCol) {
	FirstCol = 0;
	lX = dX = TruePageWidth / 2;
	lY = dY = PageLength - (UperHLine * 3 / 2); 
	if (Rotated) {
	   lX = dX = TruePageLength / 2;
	   }
	if (!NoTitle) {
	    dX = lX = lX + crX * 3;
	    dY = lY = lY + crY * 3;
	}
    }
    else {
	ClosePage ();
	InitPage ();
    }
    LinesLeft = 64;
    SeenText = 0;
}

#if 0
/* commented out AIS Fri Feb 22 10:00:36 1985 */
private PageMessage (TotalPages) 
{
    if (TotalPages > 0) {
	printf ("psgrind formatted[ %d page%s * %d cop%s ]\n",
		TotalPages, TotalPages > 1 ? "s" : "",
		spoolCopies, spoolCopies > 1 ? "ies" : "y" );
    }
}
#endif

private CommentHeader() {
    long clock;
    struct passwd *pswd;
    char hostname[40];
    /* copy the file, prepending a new comment header */
    fprintf(OutFile,"%%!%s\n",COMMENTVERSION);
    fprintf(OutFile,"%%%%Creator: ");
    pswd = getpwuid(getuid());
    gethostname(hostname, (int) sizeof hostname);
    fprintf(OutFile,"%s:%s (%s)%s\n", hostname, pswd->pw_name,
    	pswd->pw_gecos, spoolJobClass);

    fprintf(OutFile,"%%%%Title: %s %s\n",
    	(FileName?FileName:"stdin"),
	spoolJobName);
    
    fprintf(OutFile,"%%%%CreationDate: %s",(time(&clock),ctime(&clock)));
}


/* list of C keywords to put in KwordFont */
private char *KeyWordList[] = {
    "asm", "auto", "break", "case", "char", "continue", "default", "do",
    "double", "else", "entry", "enum", "extern", "float", "for", "fortran",
    "goto", "if", "int", "long", "register", "return", "short", "sizeof",
    "static", "struct", "switch", "typedef", "union", "unsigned", "void",
    "while", NULL
};

/* macros identifying C identifiers */
#define isfirst(c)	(isalpha(c) || (c) == '_')
#define isident(c)	(isalnum(c) || (c) == '_')

/* Copy the standard input file to the PS file */
private CopyFile () {
    register int   c, last;
    int	InComment, InString, InChar, IsKword;
    char token[50], *tend = token + sizeof (token) - 1;
    register char *tp, **kwp;

    col = 1;
    if (OutFile == 0) {
	if (OutOnly && !(PageSpec || Reverse)) {
	   OutFile = PipeOut ? stdout : fopen(OutName,"w");
	}
	else {
	    mktemp(mstrcat(TempName,TempDir,PSGRINDTEMP,sizeof TempName));
	    OutFile = fopen (TempName, "w");
	}
    }
    if (OutFile == NULL) {
	fprintf(stderr, "Can't create PS file %s\n",TempName);
	exit(1);
    }
    if (!ScannedFonts) {
	ScannedFonts++;
	ScanFont();
    }
    if (!Cvted) {
	CommentHeader();
	if (nf) {
	    register struct font *f;
	    fprintf(OutFile,"%%%%DocumentFonts:");
 	    ForAllFonts(f) {
		fprintf(OutFile," %s",f->name);
	    }
	    fprintf(OutFile,"\n");
	}
	/* copy in fixed prolog */
	if (copyfile(mstrcat(tempstr,LibDir,PSGRINDPRO,sizeof tempstr),
		OutFile)) {
	    fprintf(stderr,"trouble copying prolog file \"%s\".\n",tempstr);
	    exit(1);
	}
	fprintf(OutFile,"StartEnscriptDoc %% end fixed prolog\n");
	DumpFonts();
	fprintf(OutFile,"%%%%EndProlog\n");
	if (PreFeed) {
	    fprintf(OutFile,"true DoPreFeed\n");
	}
    }
    Cvted++;

    Page = 0;
    LineNo = 1;
    BadChars = 0;		/* give each file a clean slate */
    InitPage ();
    last = '\0';
    InComment = InChar = InString = 0;
    while ((c = getchar ()) != EOF) {
	if ((c > 0177 || c < 0) && (!IgnoreGarbage)) {
	    if (BadChars++ > MAXBAD) {/* allow some kruft but not much */
	      fprintf(stderr,"\"%s\" not a text file? - char '\\%03o'@0%o\nTry -g.\n",
		    FileName ? FileName : "stdin", c, ftell (stdin) - 1);
	      exit(1);
	    }
	} else {
	    switch (c) {
		case 010: /* backspace */
		    dX -= BSWidth;
		    break;
		case 015: /* carriage return ^M */
		    dY = lY;
		    dX = lX;
		    break;
		case 012: /* linefeed ^J */
		    LineNo++;
		    if (dX != lX || dY != lY || !LPTsimulate || SeenText){
			SeenText++;
			dY = lY = lY + crY;
			dX = lX = lX + crX;
		    }
		    else
			LinesLeft = 64;
		    if (((!Rotated) && (dY < UperLine))
		    || (Rotated && (dY < ((PageLength-TruePageWidth) +
						3*UperLine+480)))
		    || (LPTsimulate && (--LinesLeft <= 0)))
			PageEject ();
		    col = 1;
		    break;
		case 014: /* form feed ^L */
		    PageEject ();
		    col = 1;
		    break;
		case 011: /* tab ^I */
		    col = (col - 1) / 8 * 8 + 9;
		    dX += TabWidth - ((dX - lX) % TabWidth);
		    break;
	        case '\\': /* special escape */
		    last = c;
		    if ((c = getchar()) == EOF)
			goto done;
		    ShowChar('\\');
		    col++;
		    if (c == '\n') {
			/* want to leave newlines alone */
			ungetc(c, stdin);
		    } else {
			ShowChar(c);
			col++;
		    }
		    break;
	        case '"': /* a string quote mark */
		    if (InComment || InChar) {
			/* just put out the quote */
			ShowChar('"');
			col++;
		    } else if (InString) {
			ShowChar('"');
			col++;
			PrevFont();
			InString = 0;
		    } else {
			SetFont(LiteralFont);
			ShowChar('"');
			col++;
			InString = 1;
		    }
		    break;
	        case '\'': /* a char quote mark */
		    if (InComment || InString) {
			/* just put out the character */
			ShowChar('\'');
			col++;
		    } else if (InChar) {
			ShowChar('\'');
			col++;
			PrevFont();
			InChar = 0;
		    } else {
			SetFont(LiteralFont);
			ShowChar('\'');
			col++;
			InChar = 1;
		    }
		    break;
	        case '/':
		    if (InComment && last == '*') {
			ShowChar('/');
			col++;
			SetFont(BodyFont);
			InComment = 0;
		    } else if ((c = getchar()) == '*' && !InComment) {
			SetFont(CommentFont);
			InComment = 1;
			ShowChar('/');
			ShowChar('*');
			col += 2;
		    } else {
			ungetc(c, stdin);
			ShowChar('/');
			col++;
			c = '/';
		    }
		    break;
		default: /* plain text, put it out */
		    if (!InComment && !InString && isfirst(c)) {
			tp = token;
			while (isident(c) && tp < tend) {
			    *tp++ = c;
			    last = c;
			    c = getchar();
			}
			*tp = '\0';
			ungetc(c, stdin);
			tp = token;
			IsKword = 0;
			for (kwp = KeyWordList;
			     *kwp != NULL && **kwp <= *tp; kwp++)
				if (!strcmp(*kwp, tp)) {
					IsKword = 1;
					break;
				}
			if (IsKword)
			    SetFont(KwordFont);
			ShowStr(tp);
			col += strlen(tp);
			if (IsKword)
			    SetFont(BodyFont);
		    } else if (fonts[CurFont].Xwid[c] != NOTDEF) {
			/* other normal character */
			ShowChar (c);
			col++;
		    } else { /* not in font, quote it */
			ShowChar ('\\');
			ShowChar ((c >> 6) + '0');
			ShowChar (((c >> 3) & 7) + '0');
			ShowChar ((c & 7) + '0');
			col += 4;
		    }
		    break;
	     }
	}
	last = c;
    }

done:
    ClosePage ();
}

/* dump the fonts to the PS file for setup */
private DumpFonts () {
    register struct font   *f;

    ForAllFonts (f) {
        fprintf(OutFile,"%d %d /%s\n",f-&fonts[0],f->dsize*UperPt,f->name);
    }
    fprintf(OutFile, "%d SetUpFonts\n", nf);
}


/*
 * close the PS file
 */
private ClosePS () {
    fprintf(OutFile,"%%%%Trailer\n");
    if (PreFeed) {
	fprintf(OutFile,"false DoPreFeed\n");
    }
    fprintf(OutFile,"EndEnscriptDoc\nEnscriptJob restore\n");
}

private ProcessArg (p)
register char  *p; {
    static  enum State {
	normal, PSname,
	H_fontname, B_fontname, K_fontname, C_fontname, L_fontname,
	grabheader, getclass, getjobname
    } state = normal;

    switch (state) {
	case PSname: 
	    strcpy (OutName, p);
	    if (strcmp(OutName,"-") == 0) PipeOut++;
	    state = normal;
	    break;
	case H_fontname: 
	    decodefont (p, &fonts[HeaderFont]);
	    state = normal;
	    break;
	case B_fontname: 
	    decodefont (p, &fonts[BodyFont]);
	    state = normal;
	    break;
	case K_fontname: 
	    decodefont (p, &fonts[KwordFont]);
	    state = normal;
	    break;
	case L_fontname: 
	    decodefont (p, &fonts[LiteralFont]);
	    state = normal;
	    break;
	case C_fontname: 
	    decodefont (p, &fonts[CommentFont]);
	    state = normal;
	    break;
	case grabheader: 
	    UsersHeader = p;
	    state = normal;
	    break;
	case getclass:
	    spoolJobClass = p;
	    state = normal;
	    break;
	case getjobname:
	    spoolJobName = p;
	    state = normal;
	    break;
	default: 
	    if (*p == '-') while (*++p) switch (*p) {
		case '1': 
		    TwoColumn = 0;
		    if (SeenFile) {
			fprintf(stderr,"Specify -1 before any files\n");
			exit(1);
		    }
		    break;
		case '2': 
		    TwoColumn++;
		    if (SeenFile){
			fprintf(stderr,"Specify -2 before any files\n");
			exit(1);
		    }
		    break;
		case 'v':
		    Reverse = 1;
		    break;
		case 's':
		     PageSpec = (++p);
		     while (*p != '\0') p++;
		     return;

		/* the following options allow uswer specification
		   of the five files used by the program */
		case 'H': state = H_fontname; break;
		case 'B': state = B_fontname; break;
		case 'K': state = K_fontname; break;
		case 'L': state = L_fontname; break;
		case 'C': state = C_fontname; break;

		case 'g': IgnoreGarbage++; break;
		case 'o': ListOmitted++; break;
		case 'p': OutOnly++; state = PSname; break;
		case 'r': 
		    Rotated++;
		    if (SeenFile){
			fprintf(stderr,"Specify rotation before any files\n");
			exit(1);
		    }
		    break;
		case 'R': 
		    Rotated = 0;
		    if (SeenFile){
			fprintf(stderr,"Specify rotation before any files\n");
			exit(1);
		    }
		    break;
		case 'k':
		    PreFeed++;
		    if (SeenFile){
			fprintf(stderr,"Specify prefeed before any files\n");
			exit(1);
		    }
		    break;

		/* the following switches are as in lpr(1) and */
		/* are passed through when spooling to a printer */
		case 'P': /* printer name */
		    PrinterName = (++p);
		    while (*p != '\0') p++;
		    return;
		case 'J': /* job name (title) for the Job: field */
		    state = getjobname;
		    break;
		case 'm': /* notify by mail */
		    spoolNotify = 1;
		    break;
		case 'h':
		    spoolNoBurst = 1;
		    break;
		case '#':
		    spoolCopies = atoi(++p);
		    if (spoolCopies < 1){
		        fprintf(stderr,"Bad argument for -# (number of copies)\n");
			exit(1);
		    }
		    break;

		default: 
		    printf ("Unknown option: %c\n", *p);
		    SeenFile++;
		    break;
		}
	    else {/* not a flag -- a filename */
		FileName = Header = p;
		if (freopen (FileName, "r", stdin) == NULL) {
		    printf ("Can't open %s\n", FileName);
		    exit (1);
		}
		fstat (fileno (stdin), &S);
		FileDate = strcpy(DateStr,ctime (&S.st_mtime));
		CopyFile ();
		fclose (stdin);
		SeenFile = 1;
	    }
    }
}

main (argc, argv)
char  **argv; {
    register char  *p, *arg;

    prog = *argv;

    BodyFont = LastFont = CurFont = DefineFont (BODYFONT, 10);
    HeaderFont = DefineFont (HEADERFONT, 12);
    KwordFont = DefineFont (KWORDFONT, 10);
    CommentFont = DefineFont (COMMENTFONT, 10);
    LiteralFont = DefineFont (LITERALFONT, 11);

    /* process args in environment variable PSGRIND */
    if (p = getenv ("PSGRIND"))
	while (1) {
	    register char   quote = ' ';
	    while (*p == ' ')
		p++;
	    if (*p == '"' || *p == '\'')
		quote = *p++;
	    arg = p;
	    while (*p != quote && *p != '\0')
		p++;
	    if (*p == '\0') {
		if (*arg)
		    ProcessArg (arg);
		break;
	    }
	    *p++ = '\0';
	    ProcessArg (arg);
	}

    /* process the command line arguments */
    while (argc > 1) {
	argc--;
	ProcessArg (*++argv);
    }

    if (!SeenFile) {
	FileName = Header = 0;
	FileDate = "";
	fstat (fileno (stdin), &S);

	if ((S.st_mode & S_IFMT) == S_IFREG)
	    FileDate = strcpy(DateStr, ctime (&S.st_mtime));
	CopyFile ();
    }

    if (Cvted) {
	ClosePS ();
	fclose (OutFile);
	OutFile = 0;
    }
    if (TruncChars)
	printf ("%d characters omitted because of long lines.\n",
		TruncChars);
    if (UndefChars)
	printf ("%d characters omitted because of incomplete fonts.\n",
		UndefChars);
/*  PageMessage (TotalPages); */
    if (Cvted) {
	if (OutOnly) {
	    if (Reverse || PageSpec) {
		char temparg[200];
		char *sargs[200];
		int args = 0;

		int cpid = 0;
		/* feed Temporary through psrev */
		freopen(TempName, "r", stdin);
		if (!PipeOut) freopen(OutName, "w", stdout);
		unlink(TempName);

		addarg(sargs, REVERSE, &args);
		addarg(sargs, "-r", &args);
		if (!Reverse) addarg(sargs, "-R", &args);

		if (PageSpec) {
		    sprintf(temparg,"-s%s",PageSpec);
		    addarg(sargs, temparg, &args);
		}
		if ((cpid = fork()) < 0) pexit(prog,1);
		if (cpid == 0) {
		    execvp(REVERSE, sargs);
		    pexit(prog,1);
		}
		else {
		    wait(0);
		}
	    }
	  /*  fprintf (stderr,"PS file left on %s\n", OutName); */
	}
	else
	    SpoolIt();
    }
}

private addarg(argv, argstr, argc)
char **argv;
char *argstr;
register int *argc;
{
    register char *p = (char *) malloc (strlen(argstr) + 1);
    strcpy (p, argstr);
    argv[(*argc)++] = p;
    argv[*argc] = '\0';
}
    
private SpoolIt()
{
    char temparg[200];
    char *argstr[200];
    int nargs = 0;

    char *rargs[40];
    int  nr = 0;
    int cpid =0;
    int fdpipe[2];

    addarg(argstr, LPR, &nargs);
    if (spoolCopies > 1) {
	sprintf(temparg,"-#%d",spoolCopies);
	addarg(argstr, temparg, &nargs);
    }
    if (PrinterName) {
	sprintf(temparg,"-P%s",PrinterName);
	addarg(argstr, temparg, &nargs);
    }
    else if (getenv("PRINTER") == 0) {
	/* no printer name known anywhere, use default */
	sprintf(temparg,"-P%s",POSTSCRIPTPRINTER);
	addarg(argstr, temparg, &nargs);
    }
    if (spoolJobClass) {
	addarg(argstr, "-C", &nargs);
	addarg(argstr, spoolJobClass, &nargs);
    }
    addarg(argstr, "-J", &nargs);
    if (spoolJobName) {
	addarg(argstr, spoolJobName, &nargs);
    }
    else {
	if (!FileName) addarg(argstr, "stdin", &nargs);
	else addarg(argstr, FileName, &nargs);
    }
    if (spoolNotify) {
	addarg(argstr, "-m", &nargs);
    }
    if (spoolNoBurst) {
	addarg(argstr, "-h", &nargs);
    }

    if (Reverse || PageSpec) {
	/* lpr input will be stdin */

	addarg(rargs, REVERSE, &nr);
	addarg(rargs, "-r", &nr);
	if (!Reverse) addarg(rargs, "-R", &nr);
	if (PageSpec) {
	    sprintf(temparg,"-s%s",PageSpec);
	    addarg(rargs, temparg, &nr);
        }
/*	addarg(rargs, TempName, &nr); */

	freopen(TempName,"r",stdin);
	unlink(TempName);
	if (pipe(fdpipe)) pexit(prog,1);
	if ((cpid = fork()) < 0) pexit(prog,1);
	else if (!cpid) { /* child */
	    if (close(1)) {
		pexit(prog,1);
	    }
	    /* set stdout to be the output pipe */
	    if (dup (fdpipe[1]) == -1) {
		pexit(prog,1);
	    }
	    /* don't want to read or write the pipe itself, since dup */
	    if (close (fdpipe[1]) || close (fdpipe[0])) {
		pexit(prog,1);
	    }
	    /* leave stderr alone */
	    execvp (REVERSE, rargs);
	    pexit(prog,1);
	}
	else {
	    /* parent */
	    /* replace stdin with pipe */
	    if (close(0)) {
		pexit(prog,1);
	    }

	    if (dup(fdpipe[0]) == -1) {
		pexit(prog,1);
	    }
	    if (close (fdpipe[0]) || close (fdpipe[1])) {
		pexit(prog,1);
	    }

	    /* leave stdout and stderr alone */
	    execvp(LPR, argstr);
	    pexit(prog,1);
	}
    }
    else { /* just do lpr */
	/* remove the temporary file after spooling */
	addarg(argstr, "-r", &nargs); /* should we use a symbolic link too? */
	addarg(argstr, TempName, &nargs);
	execvp(LPR, argstr);
	pexit(prog,1);
    }
}

char *
basename(path)
	char	*path;
{
	register char	*cp;

	for (cp = path; *cp != '\0'; cp++)
		;
	for (--cp; cp > path && *cp != '/'; cp--)
		;
	if (*cp == '/' && *(cp+1) != '\0')
		return (cp + 1);
	else
		return (path);
}
