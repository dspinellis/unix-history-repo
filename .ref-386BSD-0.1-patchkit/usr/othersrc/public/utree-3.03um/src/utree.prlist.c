/*
 *      UTREE.PRLIST.C
 *      Filter for utree 3.02-um tree list files for different devices.
 *      3.02-um klin, Sun Nov 10 13:13:35 1991
 *              klin, Sun Nov 24 12:07:16 1991, XENIX graphset handling
 *      3.03-um klin, Sun Feb 16 15:34:37 1992, Minor changes
 *            a klin, Sun Mar 15 19:08:25 1992, Minor changes for AIX 3.2
 *            d klin, Tue Apr  7 09:14:38 1992, Bug fix in option handling
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
static char sccsid[] = "@(#) utree 3.03d-um (klin) Apr  7 1992 utree.prlist.c";

/*
 *      Usage:  utree.prlist [-Vhr] [-d dev] [-f fnt] [i ind] [-s siz] listfile
 *      Options:        -V      Display program version.
 *                      -T dev  or
 *                      -d dev  Output for device dev.
 *                              Supported devices are:
 *                              ascii   ascii meta characters.
 *                              850     printers with ibm pc850 charset.
 *                              ps      postscript.
 *                              term    terminal (default).
 *                      -f fnt  Font to use (postscript only)
 *                      -h      Display usage and help.
 *                      -i ind  Set tree indention to ind.
 *                      -s siz  Font size (postscript only)
 *      Remarks:        Formatted utree tree list files can be created
 *                      with the write-tree command (o) and option list (l)
 *                      from tree-screen.
 *      Directory:      /usr/local/bin
 *      Environment:    $TERM
 *      Defines:        AIX     for IBM AIX systems
 *                      BSD     for BSD systems
 *                      XENIX   for XENIX systems
 *      Libraries:      -lm -lcurses (SYSV), -ltermcap or -ltermlib (BSD)
 */

static char usage[] = "@(#) Usage: utree.prlist [-Vr][-d dev][-f fnt][-i ind][-s siz] listfile";
static char pvers[] = "UTREE 3.03-um";

#include <stdio.h>
#include <signal.h>

#define MAXLEN  512             /* Buffer length                */
#define DEFIND  6               /* Default indention            */
#define MININD  3               /* Minimal indention            */
#define MAXIND  9               /* Maxinal indention            */
#define OPTIONS "T:Vd:f:hi:rs:" /* Optionstring for getopt()    */
#define UNULL   ((unsigned char *) 0)

static char *pname;
static char *title = "Tree";
static char root[MAXLEN];

/* The generic graphical character set                          */
static unsigned char *grafset = UNULL;
#define H grafset[0]            /* Horizontal bar               */
#define V grafset[1]            /* Vertical bar                 */
#define U grafset[2]            /* Upper left corner            */
#define L grafset[3]            /* Lower left corner            */
#define T grafset[4]            /* Left tee                     */
#define I grafset[5]            /* Indention                    */
#define S grafset[6]            /* Space                        */

/* The fix coded character sets for ascii, acsc, ibm and ps     */
/*                                    H   V   U   L   T   I   S */
static unsigned char asciiset[7] = { '-','|','+','+','|',' ',' ' };
static unsigned char acscset[7]  = { '-','|','+','+','|',' ',' ' };
static unsigned char ibmset[7]   = { 196,179,218,192,195,' ',' ' };
static unsigned char psset[7]    = { 'H','V','U','L','T','I','S' };

static int indent = DEFIND;     /* Indention                    */
static int rflag  = 1;          /* Print rootdirectory flag     */
static int isps   = 0;          /* Postscript output            */

static unsigned char *acsc  = UNULL;    /* Alternate char set   */
static unsigned char *enacs = UNULL;    /* Enable alt char set  */
static unsigned char *smacs = UNULL;    /* Alt char set on      */
static unsigned char *rmacs = UNULL;    /* Alt char set off     */
#define ACSCON  if(acsc && smacs) tputs(smacs, 1, putchar)
#define ACSCOFF if(acsc && rmacs) tputs(rmacs, 1, putchar)
#undef  putchar

/* Postscript definitions and variables                         */
#define PSFONT  "Courier-Bold"  /* Default postscript font      */
#define PSSIZE   10.0           /* Default postscript font size */
#define PSMIN    50.0           /* Lower y position             */
#define PSMAX   780.0           /* Upper y position             */
#define PSIND    85.0           /* Most left x position         */
#define PSWID   475.0           /* Page header width            */
#define PSEOF   (0x04)          /* C-d: EOF for postscript      */
static int psp = 0;             /* Page counter                 */
static double psy;              /* Current y position           */
static double pps = PSSIZE;     /* Font size                    */
static double pvs = PSSIZE+2;   /* Vertical space               */
static char *pft = PSFONT;      /* Font                         */

extern double atof();
#ifdef  BSD
#define strrchr rindex
extern char *rindex();
#else   /* !BSD */
extern char *strrchr();
#endif  /* BSD */

/*
 *      Output one character
 */
static int putchar(c)
  int c;
{
  putc((unsigned char) c, stdout);
  return(c);

} /* putchar() */

/*
 *      Catch signals SIGINT, SIGQUIT and SIGTERM
 */
static int catchsig(sig)
  int sig;
{
  ACSCOFF;
  if(sig == SIGQUIT)
    abort();
  exit(1);

} /* catchsig() */

/*
 *      Init terminal and graphical character set for terminal
 */
static void initterm(f)
  int f;
{
  static char termbuf[1024];
  char termcap[1024], *term;
  char *tp = termbuf;
  extern char *getenv();
  extern unsigned char *tgetstr();

  grafset = asciiset;           /* Default character set */
  if(f && !isatty(fileno(stdout)))
    return;
  else if((term = getenv("TERM")) && tgetent(termcap, term) > 0) {
#ifdef  AIX
    if(acsc = tgetstr("bx", &tp)) {
      grafset = acscset;
      if( !(smacs = tgetstr("as", &tp)))
	smacs = tgetstr("f1", &tp);     /* AIX 3.2: font1 instead of smacs */
      if( !(rmacs = tgetstr("ae", &tp)))
	rmacs = tgetstr("f0", &tp);     /* AIX 3.2: font0 instead of rmacs */
      U = acsc[0];
      L = acsc[5];
      H = acsc[1];
      T = acsc[9];
      V = acsc[3];
    }
#else   /* !AIX */
    if(acsc = tgetstr("ac", &tp)) {
      grafset = acscset;
      if(enacs = tgetstr("eA", &tp))
	tputs(enacs, 1, putchar);
      smacs = tgetstr("as", &tp);
      rmacs = tgetstr("ae", &tp);
      do
	switch(*acsc) {
	  default:              /* Skip */
	    ++acsc; break;
	  case 'l':             /* Upper left corner */
	    U = *++acsc; break;
	  case 'm':             /* Lower left corner */
	    L = *++acsc; break;
	  case 'q':             /* Horizontal bar */
	    H = *++acsc; break;
	  case 't':             /* Left tee */
	    T = *++acsc; break;
	  case 'x':             /* Vertical bar */
	    V = *++acsc; break;
	}
      while(*acsc && *++acsc);
    }
# ifdef XENIX
    else {
      unsigned char *gp;

      grafset = acscset;
      acsc  = acscset;          /* Use as flag */
      smacs = tgetstr("GS", &tp);
      rmacs = tgetstr("GE", &tp);
      if(gp = tgetstr("G2", &tp))
	U = *gp;                /* Upper left corner */
      if(gp = tgetstr("G3", &tp))
	L = *gp;                /* Lower left corner */
      if(gp = tgetstr("GH", &tp))
	H = *gp;                /* Horizontal bar */
      if(gp = tgetstr("GR", &tp))
	T = *gp;                /* Left tee */
      if(gp = tgetstr("GV", &tp))
	V = *gp;                /* Vertical bar */
    }
# endif /* XENIX */
#endif  /* AIX */
    signal(SIGINT,  catchsig);
    signal(SIGQUIT, catchsig);
    signal(SIGTERM, catchsig);
  }

} /* initterm() */

/*
 *      Select graphical character set for device d
 */
static int initgraf(d)
  char *d;
{
  switch(*d) {
    default:                    /* Bad device */
      return(0);
    case 'a':                   /* ASCII meta characters */
      grafset = asciiset;
      break;
    case '8':                   /* IBM PC850 character set */
      grafset = ibmset;
      break;
    case 'p':                   /* Postscript */
      grafset = psset;
      ++isps;
      break;
    case 't':
      initterm(0);              /* Terminal */
      break;
  }
  return(1);

} /* initgraf() */

/*
 *      Write out graphic character c from current character set
 */
static void putgraf(c)
  int c;
{
  switch(c) {
    case 'I':                   /* Indention */
      putchar(I); break;
    case 'S':                   /* Space */
      putchar(S); break;
    default:                    /* Graphical characters */
      ACSCON;
      switch(c) {
	case 'H':               /* Horizontal bar */
	  putchar(H); break;
	case 'L':               /* Lower left corner */
	  putchar(L); break;
	case 'T':               /* Left tee */
	  putchar(T); break;
	case 'U':               /* Upper left corner */
	  putchar(U); break;
	case 'V':               /* Vertical bar */
	  putchar(V); break;
      }
      ACSCOFF;
      break;
  }
  if(isps)
    putchar(' ');

} /* putgraf() */

/*
 *      Write header lines
 */
static void putheader()
{
  if(isps) {
    printf("%%!PS-Adobe-2.0\n");
    printf("%%%%Creator: %s\n", &sccsid[5]);
    printf("%%%%Title: %s: %s\n", title, root);
    printf("%%%%DocumentFont: %s\n", pft);
    printf("%%%%Pages: (atend)\n");
    printf("%% Copyright (c) 1991 by Peter Klingebiel & UNIX Magazin Muenchen\n");
    printf("%%%%EndComments\n");
    printf("%% Variables for font, fontsize, vertical space and indentions\n");
    printf("/ft /%s def\n", pft);
    printf("/ps %3.2lf def\n", pps);
    printf("/vs %3.2lf def\n", pvs);
    printf("/s1 %3.2lf def\n", pvs / 6.0);
    printf("/s2 %3.2lf def\n", pvs / 3.0);
    printf("/s3 %3.2lf def\n", pvs * 2.0 / 3.0);
    printf("%% Procedures for drawing and printing\n");
    if(rflag) {
      printf("/Z { gsave 0.95 setgray\n");
      printf("     0 ps s3 add     rlineto  %3.2lf 0 rlineto\n", PSWID);
      printf("     0 ps s3 add neg rlineto -%3.2lf 0 rlineto\n", PSWID);
      printf("     fill grestore s2 s2 rmoveto\n");
      printf("     gsave show grestore %3.2lf setlinewidth } bind def\n", pps / 20.0);
    }
    else
      printf("/Z { %3.2lf setlinewidth } bind def\n", pps / 20.0);
    printf("/F { findfont exch scalefont setfont } bind def\n");
    printf("/P { gsave s1 s1 rmoveto show grestore stroke } bind def\n");
    printf("/G { moveto } bind def\n");
    printf("/U { 0 s2 rlineto s2 0 rlineto 0 s2 neg rmoveto } bind def\n");
    printf("/L { 0 vs rmoveto 0 s3 neg rlineto s2 0 rlineto 0 s2 neg rmoveto } bind def\n");
    printf("/V { 0 vs rlineto s2 vs neg rmoveto } bind def\n");
    printf("/T { 0 vs rlineto 0 s3 neg rmoveto s2 0 rlineto 0 s2 neg rmoveto } bind def\n");
    printf("/S { s2 0 rmoveto } bind def\n");
    printf("/I { s2 %d mul 0 rmoveto } bind def\n", indent);
    printf("/H { 0 s2 rmoveto s2 %d mul 0 rlineto 0 s2 neg rmoveto } bind def\n", indent-1);
    printf("%%%%EndProlog\n");
    printf("ps ft F\n");
  }
  else if(rflag)
    printf("\n%s  %s: %s\n\n", pvers, title, root);

} /* putheader() */

/*
 *      Write trailing lines
 */
static void puttrailer()
{
  if(isps) {
    printf("showpage\n");
    printf("%%%%Trailer\n");
    printf("%%%%Pages: %d\n", psp);
    putchar(PSEOF);
  }

} /* puttrailer() */

/*
 *      Scan one entry line and write out
 */
static void putentry(s)
  char *s;
{
  int c, i;

  if(isps) {
    if(psp == 0 || psy < PSMIN) {
      if(psp)
	printf("showpage\n");
      printf("%%%%Page %d\n", ++psp);
      if(rflag) {
	printf("%3.2lf %3.2lf G (%s  %s    Page %d) Z\n", PSIND, PSMAX, title, root, psp);
	psy = PSMAX - 2.0 * pvs;
      }
      else {
	printf("Z\n");
	psy = PSMAX;
      }
    }
    printf("%3.2lf %3.2lf G ", PSIND, psy);
    psy -= pvs;
  }
  else
    putgraf(*s++);              /* First column */
  while(c = *s++)
    switch(c) {
      default:                  /* Bad format */
	return;
      case 'H':                 /* Horizontal bar + file name */
	if(isps) {
	  putgraf(c);
	  printf("(%s) P\n", s);
	}
	else {
	  for(i = 1; i < indent; i++)
	    putgraf(c);
	  printf(" %s\n", s);
	}
	return;
      case 'I':                 /* Indention */
	if(isps)
	  putgraf(c);
	else
	  for(i = 0; i < indent; i++)
	    putgraf(c);
	break;
      case 'S':                 /* One space */
      case 'L':                 /* Lower left corner */
      case 'T':                 /* Left tee */
      case 'U':                 /* Upper left corner */
      case 'V':                 /* Vertical bar */
	putgraf(c);
	break;
    }

} /* putentry() */

/*
 *      Main routine
 */
main(argc, argv)
  int argc;
  char **argv;
{
  char line[MAXLEN], *list;
  FILE *fp;
  int c;
  extern char *optarg;          /* From getopt() */
  extern int optind;

  /* Get programs name */
  if(pname = strrchr(argv[0], '/'))
    ++pname;
  else
    pname = argv[0];

  /* Parse and check command line options */
  while((c = getopt(argc, argv, OPTIONS)) != EOF) {
    switch(c) {
      default:                  /* Bad option */
	fprintf(stderr, "%s\n", &usage[5]);
	exit(1);
      case 'V':                 /* Version */
	fprintf(stderr, "%s\n", &sccsid[5]);
	exit(0);
      case 'h':                 /* Some help */
	fprintf(stderr, "%s\n", &usage[5]);
	fprintf(stderr, "\t-T dev\t\tOutput for device dev\n");
	fprintf(stderr, "\t-V\t\tDisplay program version\n");
	fprintf(stderr, "\t-d dev\t\tOutput for device dev\n");
	fprintf(stderr, "\t-f fnt\t\tUse font fnt (ps only, default: Courier-Bold)\n");
	fprintf(stderr, "\t-h\t\tDisplay some help\n");
	fprintf(stderr, "\t-i ind\t\tSet tree indention to ind (3..9)\n");
	fprintf(stderr, "\t-s siz\t\tUse font size siz (ps only, default: 10)\n");
	fprintf(stderr, "\t\t\tCurrently supported output devices are:\n");
	fprintf(stderr, "\t\t\tascii\tAscii meta graphic characters\n");
	fprintf(stderr, "\t\t\t850\tPrinters using IBM PC850 character set\n");
	fprintf(stderr, "\t\t\tps\tPostscript\n");
	fprintf(stderr, "\t\t\tterm\tTerminal (default)\n");
	exit(0);
      case 'T':                 /* Device for graphical character set */
      case 'd':
	if(initgraf(optarg))
	  break;
	fprintf(stderr, "%s: device not supported -- %s\n", pname, optarg);
	exit(1);
      case 'r':                 /* Suppress pathname of rootdirectory */
	rflag = 0;
	break;
      case 'i':                 /* Indention */
	if((indent = atoi(optarg)) < MININD || indent > MAXIND) {
	  fprintf(stderr, "%s: bad indention -- %d\n", pname, indent);
	  exit(1);
	}
	break;
     case 'f':                  /* Postscript font */
       pft = optarg;
       break;
     case 's':                  /* Font size */
       pps = atof(optarg);
       pvs = pps + 2.0;
       break;
    }
  }

  /* Get and tree list file name */
  if((argc - optind) != 1) {
    fprintf(stderr, "%s\n", &usage[5]);
    exit(1);
  }
  else
    list = argv[optind];

  /* Open directory tree list file */
  if( !(fp = fopen(list, "r"))) {
    fprintf(stderr, "%s: Cannot open list file %s\n", pname, list);
    exit(1);
  }

  /* Init default graphical character set if needed */
  if(grafset == UNULL)
    initterm(1);

  /* Read, scan and write tree list file line by line */
  while(fgets(line, sizeof(line), fp)) {
    line[strlen(line)-1] = '\0';
    switch(c = line[0]) {
      default:                  /* Ignore */
	break;
      case 'R':                 /* Pathname of rootdirectory */
	strncpy(root, &line[1], sizeof(root));
	putheader();
	break;
      case 'H':                 /* Valid tree list entry */
      case 'L':
      case 'U':
      case 'V':
	putentry(line);
	break;
    }
  }
  fclose(fp);

  /* Write trailer and exit */
  puttrailer();
  exit(0);

} /* main() */
