/*
 *      MAIN.C
 *      UTREE main routine.
 *      3.01-um klin, Sat May  4 15:45:02 1991
 *              klin, Tue Oct 15 12:26:53 1991, Option -L added
 *              klin, Sat Oct 26 15:04:54 1991, writedlist() changed
 *      3.02-um klin, Fri Nov  1 10:46:14 1991, Option -u changed to -n
 *                                              Option -u added
 *              klin, Sun Nov 10 19:37:45 1991, buildlist() changed
 *              klin, Sun Nov 24 19:30:43 1991, Cd to current directory before
 *                                              executing some commands
 *      3.03-um klin, Tue Feb 11 22:35:55 1992, Shell command history added
 *              klin, Sat Feb 15 19:18:30 1992, Video handling and option -v
 *                                              changed
 *                                              Partinioning of directory and
 *                                              file windows changed and
 *                                              option -p added
 *              klin, Sun Feb 23 18:45:19 1992, Keybindings, variable AUTOSAVE
 *                                              and option -o added
 *            d klin, Thu Apr  2 09:07:41 1992, Handle -o
 *            e klin, Sat Apr 11 11:06:20 1992, Color handling and option -C added
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 main.c";
#endif  /* !lint */

/*
 *      Usage:  utree   [-CLSVabcghnoqrstuw] [-d var=[val]] [-d cmd:[typ]] [-f lst]
 *                      [-f lst] [-i ind] [-l lev] [-p lin] [-v mod] [-x cmd]
 *                      [rootdir]
 *
 *      Options:        -C              Don't use colors
 *                      -L              Follow symbolic links
 *                      -S              Ignore minimal screen size.
 *                      -V              Display program version.
 *                      -a              Read in all dirs including hidden.
 *                      -b              No bell
 *                      -c              No clock display/update in echoline.
 *                      -d var=[val]    Define/undefine variable var.
 *                      -d typ:[cmd]    Define/undefine command for type typ.
 *                      -f lst          Build tree from list file lst.
 *                      -g              No graphic characters.
 *                      -h              Display some help about options.
 *                      -i ind          Set tree indention to ind (3..9)
 *                      -l lev          Build tree up to level lev.
 *                      -n              No scan tree for changes.
 *                      -o              Omit saving definition/history changes.
 *                      -p lin          Set file window to lin lines (1..9)
 *                      -q              Build tree up to level 2 (like -l 2).
 *                      -r              Recreate tree list (always scan disk).
 *                      -s              No hardware scrolling.
 *                      -t              Sort files by modification times.
 *                      -u              Update directory tree.
 *                      -v mod          Set video attribute settings (0,1,2)
 *                      -w              No warning about unreadable dirs.
 *                      -x cmd          Use string cmd as initial input.
 *                      The options bcginopsvw may also be set in the environment
 *                      variable $UTREE.
 *
 *      Directory:      /usr/local/bin
 *
 *      Environment:    $EDITOR $HOME $PAGER $SHELL $TERM $UTREE $UTLIB
 *
 *      Files:          $UTLIB/utree.help       - Utree help pages.
 *                      $UTLIB/utree            - Global startup file.
 *                      $UTLIB/utree-$TERM      - Global key bindings for $TERM
 *                      $HOME/.utree            - User's startupfile.
 *                      $HOME/.utreelist        - User's tree list file.
 *                      $HOME/.utreehist        - Shell command history.
 *                      $HOME/.utree-$TERM      - Users key bindings for $TERM
 *                      $PATH/utree.prlist      - Tree display program
 *                      $PATH/utree.backup      - Backup shell script.
 *                      $PATH/utree.mklist      - Create tree list.
 *
 *      Libraries:      -lcurses (SYSV), -ltermcap or -ltermlib (BSD)
 *
 *      Bugs:           Changes in filesystem after shell or editor escape
 *                      are not always detected!
 *                      Directory tree depth > 32 may confuse utree!
 *                      Symbolic links to directories may confuse utree!
 *                      Chown() and chgrp() for normal users on some BSD
 *                      systems are not allowed!
 */

#define _MAIN_                          /* Declare external variables   */
#include "defs.h"                       /* defined in header files      */

/* ---- Gobal variables and definitions ------------------------------- */

#ifdef  USEANSICOLORS
# define USES1  "[-CLSVabcghnoqrstuw] [-d var=[val]] [-d cmd:[typ]] [-f lst]"
# define USES2  "[-i ind] [-l lev] [-p lin] [-v mod] [-x cmd] [rootdir]"
# define UOPTS  "CLSVabcd:f:ghi:nol:p:qrstuv:wx:" /* String for getopt() */
#else
# define USES1  "[-LSVabcghnoqrstuw] [-d var=[val]] [-d cmd:[typ]] [-f lst]"
# define USES2  "[-i ind] [-l lev] [-p lin] [-v mod] [-x cmd] [rootdir]"
# define UOPTS  "LSVabcd:f:ghi:nol:p:qrstuv:wx:" /* String for getopt() */
#endif  /* USEANSICOLORS */

GLOBL int helpline,   echoline;         /* Help/echo lines              */
GLOBL int firstline,  lastline;         /* First/last lines             */
GLOBL int firstdline, lastdline;        /* First/last tree lines        */
GLOBL int ndlines;                      /* # dir lines on tree screen   */
GLOBL int firstfline, lastfline;        /* First/last file lines        */
GLOBL int nflines;                      /* # file lines on tree screen  */
GLOBL int maxnflines = DEFFIL;          /* Max file lines               */
GLOBL int fperpage,   fperline;         /* Files per page/line          */
GLOBL int treeflag,   fileflag;         /* Screen update flags          */
GLOBL int filecount,  dircount;         /* # of files/directories       */
GLOBL int indent = -1;                  /* Tree indention: 2..9         */
GLOBL int videomode = VMODE2;           /* Video mode: all attributes   */
GLOBL int videoattr = VA_NORMAL;        /* Video attribute              */
GLOBL int graphattr = GC_OFF;           /* Graphic character set flag   */
GLOBL int maxindent = 0;                /* User defined tree indention  */
GLOBL int maxlevel;                     /* Current max tree level       */
GLOBL int buildflag  = 0;               /* Rebuild directory flag       */
GLOBL int writeflag  = 0;               /* Rewrite tree list file       */
GLOBL int sortbytime = 0;               /* Sort file by mod times       */
GLOBL int hiddendirs = 0;               /* Build up hidden directories  */
GLOBL int sizechange = 0;               /* Screen size changed          */
GLOBL int (*statfun)() = lstat;         /* Follow symbolic links        */
GLOBL dlist *droot   = DNULL;           /* Root of directory list       */
GLOBL dlist *cdlist  = DNULL;           /* Current directory list       */
GLOBL dlist *cwlist  = DNULL;           /* Current working directory    */
GLOBL char fpattern[PATLEN] = { '\0' }; /* Search pattern file search   */
GLOBL char gpattern[PATLEN] = { '\0' }; /* Search pattern file grep     */
GLOBL char tpattern[PATLEN] = { '\0' }; /* Tag pattern file tag         */
GLOBL char rootdir[NAMELEN];            /* Root directory name          */
GLOBL char utreemenu[MINCOLS];          /* User command tree menu line  */
GLOBL char ufilemenu[MINCOLS];          /* User command file menu line  */
GLOBL char *prgname;                    /* Program name/version         */
GLOBL char *home;                       /* Home directory               */
#ifdef  BSD
GLOBL jmp_buf winchjump;                /* Global goto for SIGWINCH     */
GLOBL int atread;                       /* Flag if we are in getchar()  */
#endif
#ifdef  USEANSICOLORS
GLOBL int usecolors = 1;                /* Use colors if defined        */
#endif  /* USEANSICOLORS */

GLOBL char *who;                        /* Name of current command      */
GLOBL char *hitkey = "(Hit a key)";

/* ---- External variables and functions ------------------------------ */

EXTRN char *optarg;                     /* From getopt()                */
EXTRN int optind;                       /*   "     "                    */
EXTRN char *initscreen();
EXTRN int exitscreen();
EXTRN char *calloc();
EXTRN char *writedlist();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Display usage message and some help about options if flag f is set */
LOCAL VOID usage(f)
  register int f;
{
  (void) fprintf(stderr, "Usage:\t%s\t%s\n\t\t%s\n", prgname, USES1, USES2);
  if(f) {
#ifdef  USEANSICOLORS
    (void) fprintf(stderr, "\t-C\t\tDon't use colors on color terminals\n");
#endif  /* USEANSICOLORS */
    (void) fprintf(stderr, "\t-L\t\tFollow symbolic links\n");
    (void) fprintf(stderr, "\t-S\t\tIgnore minimal screen size\n");
    (void) fprintf(stderr, "\t-V\t\tDisplay program version\n");
    (void) fprintf(stderr, "\t-a\t\tRead in all (incl hidden) directories\n");
    (void) fprintf(stderr, "\t-b\t\tSuppress ringing of the bell\n");
    (void) fprintf(stderr, "\t-c\t\tDon't display and update a clock\n");
    (void) fprintf(stderr, "\t-d var=[val]\tSet/unset variable\n");
    (void) fprintf(stderr, "\t-d typ:[cmd]\tSet/unset filetype command\n");
    (void) fprintf(stderr, "\t-f lst\t\tBuild tree from list file lst\n");
    (void) fprintf(stderr, "\t-g\t\tDon't use graphic characters\n");
    (void) fprintf(stderr, "\t-h\t\tDisplay some help about options\n");
    (void) fprintf(stderr, "\t-i ind\t\tSet tree indention to ind (3..9)\n");
    (void) fprintf(stderr, "\t-l lev\t\tBuild tree up to level lev\n");
    (void) fprintf(stderr, "\t-n\t\tDon't scan tree for changes in tree\n");
    (void) fprintf(stderr, "\t-o\t\tOmit saving definition/history changes\n");
    (void) fprintf(stderr, "\t-p lin\t\tSet file lines on tree screen to lin (1..9)\n");
    (void) fprintf(stderr, "\t-q\t\tBuild tree up to level 2 (like -l 2)\n");
    (void) fprintf(stderr, "\t-r\t\tRecreate tree list (always scan disk)\n");
    (void) fprintf(stderr, "\t-s\t\tDon't use hardware scrolling\n");
    (void) fprintf(stderr, "\t-t\t\tSort files by modification times\n");
    (void) fprintf(stderr, "\t-u\t\tUpdate file lists in directory tree\n");
    (void) fprintf(stderr, "\t-v mod\t\tSet video attribute mode to mod (0, 1 or 2)\n");
    (void) fprintf(stderr, "\t-w\t\tNo warning about unreadable directories\n");
    (void) fprintf(stderr, "\t-x cmd\t\tUse string cmd as initial input\n");
  }

} /* usage() */

/* Exit utree on error */
LOCAL VOID uerror(err)
  register char *err;
{
  (void) cursorxy(0, lines - 1);
  clearline();
  flushout();
  terminalreset(1);
  (void) fprintf(stderr, "%s: ", prgname);
  perror(err);
  exit(1);
  /*NOTREACHED*/

} /* uerror() */

/*
 *      SETUP VARIABLES
 */
/* Set up terminal dependent variables */
LOCAL int setupscreen(sig)
  register int sig;
{
#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
  struct winsize ws;

  /* Signal SIGWINCH catched: get new screen size */
  if(sig && ioctl(fileno(stdin), TIOCGWINSZ, (char *) &ws) == 0) {
    if(ws.ws_row > 0)
      lines = ws.ws_row;
    if(ws.ws_col > 0)
      columns = ws.ws_col;
    sizechange = 1;
    if(lines < MINLINS)
      bell(1);
    if(columns < MINCOLS)
      bell(1);
    flushout();
  }
  /* Catch signal SIGWINCH for window changes */
  (void) signal(SIGWINCH, (SIGNL(*)()) setupscreen);
#endif  /* SIGWINCH && TIOCGWINSZ */

  /* Set up some screen size dependent variables */
  echoline   = 0;
  helpline   = 1;
  firstline  = 2;
  lastline   = lines-1;
  firstdline = firstline;
  lastfline  = lastline;
  fperline   = columns/FWINSZ;
  fperpage   = (lines-2) * fperline;
  if(fperline <= 0)
    fperline = 1;
  /* Check some variables */
  if(fperpage <= 0)
    fperpage = 1;
  checklines(1);
  (void) setvideomode(videomode);

#if     defined(BSD) && defined(SIGWINCH) && defined(TIOCGWINSZ)
  /* BSD: Because signal SIGWINCH doesn't interrupt the systemcall */
  /*      read use the global goto longjmp() to return to getchar  */
  if(sig && atread && sizechange)
    longjmp(winchjump, 1);
#endif  /* BSD && SIGWINCH && TIOCGWINSZ */

} /* setupscreen() */

/*
 *      GET STARTUPFILES
 */

/* Build startupfile name from file f and copy it to buffer b */
GLOBL int startup(b, f)
  register char *b, *f;
{
  register char *e;

  /* First: Try user's home directory */
  (void) sprintf(b, ".%s", f);
  (void) strcpy(b, pathname(b, home));
  if(access(b, 04) == 0)
    return(1);
  /* Second: Try libdir if defined in environment */
  if(e = getenv("UTLIB")) {
    (void) strcpy(b, pathname(f, e));
    if(access(b, 04) == 0)
      return(1);
  }
  /* Third: Try global libdir if defined */
#ifdef  UTLIB
  (void) strcpy(b, pathname(f, UTLIB));
  if(access(b, 04) == 0)
    return(1);
#endif  /* UTLIB */
  return(0);

} /* startup() */

/*
 *      MEMORY ALLOCATION
 */

/* Allocate memory. Exit on error */
GLOBL char *ualloc(n, s)
  register unsigned n, s;
{
  register char *p;

  if(p = calloc(n, s))
    return(p);
  uerror("ualloc");
  /*NOTREACHED*/

} /* ualloc() */

/* Free allocated memory */
GLOBL VOID ufree(p)
  register char *p;
{
  if(p)
    free(p);

} /* ufree() */

/*
 *      MAIN ROUTINE
 */

GLOBL VOID main(argc, argv)
  int argc;
  char **argv;
{
  char list[NAMELEN], name[NAMELEN], cwd[NAMELEN];
  struct stat st;
  FILE *fp;
  char *lst = NULL;
  char *cp, *root, *term;
  int hard = 0, level = 0, siz = 1, update = 0;
  int opt, any, rval, i;

  /* Set up programs name and commandline variables */
  prgname = basename(argv[0]);

  /* Get users home directory */
  if((home = getenv("HOME")) == NULL) {
    (void) fprintf(stderr, "%s: Can't get your home directory\n", prgname);
    exit(1);
  }

  /* Check if utree is running from a terminal */
  if( !isatty(fileno(stdin)) || !isatty(fileno(stdout))) {
    (void) fprintf(stderr, "%s: Not attached to a terminal\n", prgname);
    exit(1);
  }

  /* Init utree variables before parsing command line options */
  initvariables();

  /* Parse and check command line options */
  while((opt = getopt(argc, argv, UOPTS)) != EOF)
    switch(opt) {
      default:                  /* Bad option or missing argument */
	usage(0);
	exit(1);
#ifdef  USEANSICOLORS
      case 'C':                 /* Don't use colors */
	(void) setvariable("UC=", VC_SET);
	break;
#endif  /* USEANSICOLORS */
      case 'L':                 /* Follow symbolic links */
	statfun = stat;
	break;
      case 'S':                 /* Ignore minimal screen size */
	siz = 0;
	break;
      case 'V':                 /* Show utree version */
	utreeversion();
	exit(0);
      case 'a':                 /* Build up all directories */
	hiddendirs = 1;
	break;
      case 'b':                 /* No bell */
	(void) setvariable("BL=", VC_SET);
	break;
      case 'c':                 /* No clock showing and update */
	(void) setvariable("CL=", VC_SET);
	break;
      case 'd':                 /* Define or undefine variables */
	if(strchr(optarg, '=')) {
	  (void) setvariable(optarg, VC_SET);
	  break;
	}
	else if(strchr(optarg, ':')) {
	  (void) setcommand(optarg, VC_SET);
	  break;
	}
	(void) fprintf(stderr, "%s: bad assignment -- %s\n",
		       prgname, optarg);
	usage(0);
	exit(1);
      case 'f':                 /* Tree list file given */
	lst = optarg;
	break;
      case 'g':                 /* No graphic characters */
	(void) setvariable("GC=", VC_SET);
	break;
      case 'h':                 /* Display some help */
	utreeversion();
	usage(1);
	exit(0);
      case 'i':                 /* Tree level indention */
	(void) sprintf(list, "TI=%s", optarg);
	if(setvariable(list, VC_SET) < 0) {
	  (void) fprintf(stderr, "%s: bad tree indention (must be 3 .. 9) -- %s\n",
			 prgname, optarg);
	  usage(0);
	  exit(1);
	}
	break;
      case 'l':                 /* Level to build up the tree */
	if((level = atoi(optarg)) <= 0) {
	  (void) fprintf(stderr, "%s: bad tree level (must be > 0) -- %s\n",
			 prgname, optarg);
	  usage(0);
	  exit(1);
	}
	break;
      case 'n':                 /* No scan tree for changes */
	(void) setvariable("ST=", VC_SET);
	break;
      case 'o':                 /* Omit saving definition changes */
	(void) setvariable("AS=", VC_SET);
	break;
      case 'p':                 /* Number of file lines on tree screen */
	(void) sprintf(list, "FL=%s", optarg);
	if(setvariable(list, VC_SET) < 0) {
	  (void) fprintf(stderr, "%s: bad number of lines (must be 1 .. 9) -- %s\n",
			 prgname, optarg);
	  usage(0);
	  exit(1);
	}
	break;
      case 'q':                 /* Build tree up to level 2 */
	level = 2;
	break;
      case 'r':                 /* Hard build up the tree */
	hard = writeflag = 1;
	break;
      case 's':                 /* No terminal scrolling */
	(void) setvariable("TS=", VC_SET);
	break;
      case 't':                 /* Sort files by modification times */
	(void) setvariable("LS=", VC_SET);
	break;
      case 'u':                 /* Update filelists in tree */
	update = buildflag = 1;
	break;
      case 'v':                 /* Set video mode */
	(void) sprintf(list, "VM=%s", optarg);
	if(setvariable(list, VC_SET) < 0) {
	  (void) fprintf(stderr, "%s: bad mode (use 0, 1 or 2) -- %s\n",
			 prgname, optarg);
	  usage(0);
	  exit(1);
	}
	break;
      case 'w':                 /* No warning about unreadable directories */
	(void) setvariable("WD=", VC_SET);
	break;
      case 'x':                 /* Fill input buffer */
	if(ungetstring(optarg)) {
	  (void) fprintf(stderr, "%s: bad input string -- %s\n",
			 prgname, optarg);
	  usage(0);
	  exit(1);
	}
	break;
    }

  /* Get current working directory */
  (void) getcwd(cwd, NAMELEN-2);

  /* Check list file if given and setup rootdirectory */
  if(lst) {
    if( !(fp = fopen(lst, "r"))) {
      (void) fprintf(stderr, "%s: Cannot open list file %s\n", prgname, lst);
      exit(1);
    }
    while(fgets(name, sizeof(name), fp)) {
      if(VALID(name[0]) && (name[0] == '/' || name[0] == '.')) {
	if(name[i = strlen(name) - 1] == '\n')
	  name[i] = '\0';
	(void) fclose(fp);
	goto OUT;
      }
    }
    (void) fclose(fp);
    (void) fprintf(stderr, "%s: No root found in list file %s\n", prgname, lst);
    exit(1);
  }
OUT:

  /* Get root directory from where to build up the directory tree */
  switch(argc - optind) {
    case 0:                     /* Root from list file */
      if(lst) {
	(void) strcpy(list, pathname(lst, cwd));
	root = pathname(name, cwd);
      }
      else {                    /* Root is users home directory */
	(void) strcpy(list, pathname(UTLIST, home));
	root = home;
      }
      break;
    case 1:                     /* Root is given in command line */
      root = pathname(argv[optind], cwd);
      break;
    default:                    /* Too many arguments */
      usage(0);
      exit(1);
  }

  /* Test and change to root directory */
  if( !ISDIR(root, st)) {
    (void) fprintf(stderr, "%s: Root %s is not a directory\n", prgname, root);
    exit(1);
  }
  if(chdir(root)) {
    (void) fprintf(stderr, "%s: Can't change to root %s\n", prgname, root);
    exit(1);
  }
  (void) strcpy(rootdir, root);
  any = strncmp(root, home, strlen(home));

  /* Init screen and check screen size */
  term = getenv("TERM");
  if(cp = initscreen(term)) {
    (void) fprintf(stderr, "%s: %s\n", prgname, cp);
    exit(1);
  }
  else if(siz && (columns < MINCOLS || lines < MINLINS)) {
    (void) fprintf(stderr, "%s: Screen too small, %dx%d instead of %dx%d\n",
		   prgname, columns, lines, MINCOLS, MINLINS);
    exit(1);
  }
  (void) setupscreen(0);

  /* Init history list, help pages, key bindings and graphical character set */
  inithistory();
  inithelp();
  initgraphics(VARSET(V_GC));

  /* Catch signals and set terminal to raw mode. ATTENTION:  */
  /* SIGKILL cannot be caught. This leaves tty in raw state! */
  terminalraw(1);
  (void) signal(SIGQUIT, (SIGNL(*)()) exitscreen);
  (void) signal(SIGINT,  (SIGNL(*)()) exitscreen);
  (void) signal(SIGTERM, (SIGNL(*)()) exitscreen);
  enablesignals();

  /* Show initial screen */
#ifdef  USEANSICOLORS
  if(colorcap)
    colorset(usecolors ? CS_INIT : CS_RESET);
  else
#endif  /* USEANSICOLORS */
  clearscreen();
  (void) putversion(helpline, NULL);
  flushout();

  /* Build up directory tree and file lists */
  if(lst)
    rval = buildlist(rootdir, cwd, list);
  else if(level) {
    update = 0;
    rval = buildread(rootdir, 1, level, 0);
  }
  else if(hard || any) {
    update = 0;
    rval = buildread(rootdir, 1, HLEVEL, 0);
  }
  else if((rval = buildlist(rootdir, cwd, list)) != RV_OK) {
    update = 0;
    writeflag = 1;
    rval = buildread(rootdir, 1, HLEVEL, 0);
  }

  /* Exit on error at building tree */
  if(rval != RV_OK)
    uerror(rootdir);

  /* Disable/ignore signals */
  disablesignals();
  (void) signal(SIGQUIT, SIG_IGN);
  (void) signal(SIGINT,  SIG_IGN);
  cursorset(CF_INVISIBLE);
  flushout();

  /* Call the tree menu */
  cwlist = droot;
  rval = treemenu(update);

  /* Write/rewrite tree and command history lists if needed */
  if(EQU(rootdir, home)) {
    if(writeflag && !(level || any)) {
      (void) strcpy(list, pathname(UTLIST, home));
      (void) writedlist(list, droot, "home", 'd');
    }
  }

  /* Save history list, variables, commands and key bindings */
  savehistory();
  savevariables();
  savebindings(term);

  /* Clear screen, reset terminal and exit */
  clearscreen();
  exitscreen(rval == RV_ERR ? 1 : 0);

} /* main() */
