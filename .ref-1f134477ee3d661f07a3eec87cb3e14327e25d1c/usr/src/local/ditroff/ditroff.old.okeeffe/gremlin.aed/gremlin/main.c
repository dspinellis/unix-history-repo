/* @(#)main.c	1.7	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 *      This is the main routine for the gremlin picture editor.
 */


#include "gremlin.h"
#include "grem2.h"
#include <signal.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

/* imports from config.c */

extern char GMapFile[];

/* database imports */

extern ELT *DBInit(), *DBRead();
extern POINT *PTInit(), *PTMakePoint();

/* imports from menu.c */

extern MNInterpretCursor();
extern MNIcon(), MNInitMenu(), MNDisplayMenu();

/* graphics imports */
extern GRInit(), GRClose();
extern GRSetGrid(), GRDisplayGrid(), GRBlankGrid();
extern GRDisableTablet(), GREnableTablet();
extern GRSetMap(), GRsetwmask();
extern artmode;

/* imports from undodb.c */

extern UNForget();
extern UNELT *unlist, *unback;

/* imports from long.c */

extern char *Editfile;
extern CP(), LGCommand();
extern LGQuit();

/* imports from short.c */

extern SHCommand();
extern SHRedis();
extern SHUpdate();

/* other */

extern char *FindAED();

/* Forward references within this file: */

extern OnStop(), OnCommand();

/* Imports from textio.c: */

extern TxInit(), TxRedisplay(), TxClose(), TxPutMsg(), TxMsgOK();
extern TxLine(), TxPutString();
extern char TxGetChar();
extern short inline;
extern TxPutString();
extern TXFIELD TAlign, TAdjust, TBrush, TFont, TGravity, TCSize;
extern TXFIELD TEdit, TJustmode;


/* Library routines: */

extern char *malloc(), *sprintf(), *strcat(), *strcpy();

/* Version number */

char SccsId [] = "@(#)main.c	1.7	(Berkeley)	%G%";

#ifdef SIGTINT
static int lintrup = LINTRUP;    /* Constant for local mode bit */
#endif SIGTINT

/* Declaration of Globals */

ELT *PICTURE, *cset, arhead;
int CBRUSH, CFONT, CSIZE, CJUST, Gridsize, Gridon, Orientation; 
int Alignment, SEQ, Adjustment, GravityOn, Consume, CHANGED;
float PX, PY, Lastx, Lasty;
POINT *POINTLIST, *BACKPOINT, MENPOINT[NUSER];
ELT *MEN[NUSER];
char cmdbuf[400];
int SEARCH = TRUE;
int jmodes = 16;
char *textpos[] = {"bl", "bc", "br", "cl", "cc", "cr", "tl", "tc", "tr",
                   "lb", "cb", "rb", "lc",       "rc", "lt", "ct", "rt" };
char *dispmode[] = {"BL", "BR", "CC", "XX", "XX", "XX", "XX", "XX", 
                    "XX", "XX", "TL", "TC", "TR", "CL", "CR", "BC" };
int textmode[] = { 0, 15, 1, 13, 2, 14, 10, 11, 12, 
                   0, 15, 1, 13,    14, 10, 11, 12 };

/* symbolic types, brushes, etc. */

char *lines[] = { "broken", "dashed", "dotted", 
                  "medium", "narrow", "thick", NULL };
int lnum[] = { 2, 4, 1, 6, 5, 3 };
char types[] = { 'l', 'r', 'c', 'v', 'a', 'u', '\0' };
char *fonts[] = { "Bold", "Italics", "Roman", "Special",
                  "bold", "italics", "roman", "special", NULL };
int fnum[] = { 3, 2, 1, 4, 3, 2, 1, 4 };

error(s)
char *s;
/*
 *      This routine prints an error message and sets the Consume flag
 * so that points are not cleared.
 */
{
    putchar('\7');
    TxPutMsg(s);
    Consume = FALSE;
}


main(argc, argv)
int argc;
char *argv[];
{
    FILE *fp, *POpen(), *fopen(),*grtty, *tablet, *map, *startup;
    POINT *p1, pos;
    int x1, y1, x2, y2, i, j;
    int button, cux, cuy;
    char colours[768], msg[50], sw, *arg, *file;
    char *ptr, *display, *type, *getenv(), home[100];
    char *path = ".";
    struct stat buf;
    ino_t inode;

    display = FindAED();
    if (display == NULL) display = "/dev/null";
    Orientation = 1;
    file = NULL;

    /* Parse the command line. */

    argc -= 1;  argv++;                /* Skip program name. */
    while (argc > 0)
    {
        argc -= 1;
        arg = *argv++;
        if (arg[0] != '-') file = arg;
        else
        {
            sw = *++arg;
            switch (sw)
            {
            case 'g':
                if (*++arg == '\0')
                if (argc-- > 0) arg = *argv++;
                if (argc < 0) error("usage: gremlin -g <display name>");
                display = arg;
                if (*display != '/') display = strcat("/dev/", arg);
                break;
            case 'p':
                if (*++arg == '\0')
                if (argc-- > 0) arg = *argv++;
                if (argc < 0) error("usage: gremlin -p <path>");
                path = arg;
                SEARCH = TRUE;
                break;
            case 'h':
                Orientation = 0;
                break;
            case 'v':
                Orientation = 1;
                break;
            default:
                (void) sprintf(msg, "unknown switch: %c", sw);
                error(msg);
            }
        }
    }

    PSetPath(path);
    if ((map = fopen(GMapFile, "r")) == NULL) {
	error ("can't open color map");
    } else
	for (i=0; i<768; ++i)    /* read in color map */
	{
	    (void) fscanf(map,"%o",&j);
	    colours[i] = j&0377;
	}

    /* Open the display, and call all of the initialization routines.
     * Initialize all of the globals defined in this file.
     */
    type = getenv("TERM");
    TxInit(type);

    /* Ignore quit signals, catch interrupts and stops. */

    signal(SIGINT, SIG_IGN);
    signal(SIGTSTP, OnStop);
    signal(SIGTTIN, OnStop);
    signal(SIGTTOU, OnStop);

#ifdef SIGTINT
    signal(SIGTINT, OnCommand);
    sighold(SIGTINT);
#endif SIGTINT

    grtty = fopen(display,"w");
    if (grtty == NULL)
    {
        error("couldn't open display");
        LGQuit(display);
    };
    tablet = fopen(display,"r");
    if (tablet == NULL) tablet = fopen("/dev/null", "r");
    GRInit(grtty,0);
    GRSetMap(colours);

#ifdef SIGTINT
    (void) ioctl(fileno(stdin), TIOCLBIS, (char *) &lintrup);
#endif SIGTINT

    TxRedisplay();    /* display text screen */

    CBRUSH = 6;
    CFONT = 1;
    CSIZE = 1;
    CJUST = 0;
    Gridon = FALSE;
    Adjustment = NOADJ;
    Alignment = 4;
    artmode = FALSE;
    SEQ = 0;
    GravityOn = FALSE;
    CHANGED = FALSE;
    POINTLIST = PTInit();
    BACKPOINT = PTInit();
    unlist = unback = nullun;
    p1 = PTInit();       /* initialize arrowhead template */
    (void) PTMakePoint(0.0, 0.0, &p1);
    (void) PTMakePoint(-5.0, 3.0, &p1);
    (void) PTMakePoint(-3.0, 0.0, &p1);
    (void) PTMakePoint(-5.0, -3.0, &p1);
    (void) PTMakePoint(0.0, 0.0, &p1);
    arhead.type = VECTOR;
    arhead.ptlist = p1;
    arhead.brushf = 0;   /* brush filled in when used */
    arhead.size = 0;
    arhead.textpt = malloc(1);
    *(arhead.textpt) = '\0';

    PICTURE = DBInit();    /* initialize picture databse */
    cset = DBInit();       /* and current set   */
    for (i=0; i<4; ++i)    /* and user symbols  */
        MEN[i] = DBInit();
    Editfile = malloc(100);
    if (file == NULL)
        *Editfile = '\0';
    else
    {
        (void) strcpy(Editfile, file);
        fp = POpen(Editfile, (char **) NULL, SEARCH);
        if (fp == NULL)  error("(creating new file)");
        else PICTURE = DBRead(Editfile,&Orientation, &pos);
    }
    unlist = unback = nullun;
    TxPutString(&TEdit, Editfile);
    TxPutMsg("        Gremlin - Version 2.3 (1982)");
    MNIcon();
    MNInitMenu(Orientation);      /* Initialize Menu */
    SHUpdate();         /* Display menu and picture, if any */

    if (Orientation == 0)    /* initialize grid */
    {
        x1 = y1 = 0;
        x2 = 511;
        y2 = 395;
    }
    else
    {
        x1 = 116;
        y1 = 0;
        x2 = 511;
        y2 = 483;
    };
    Gridsize = 16;
    GRSetGrid(x1, y1, x2, y2, Gridsize);

        /* read start-up file */
        /* look in home directory */
    sprintf(home,"%s/.gremlinrc",getenv("HOME"));
    startup = fopen(home, "r");
    if (startup != NULL)
    {
        fstat(startup, &buf);
        inode = buf.st_ino;
        ptr = fgets(cmdbuf, 400, startup);
        while (ptr != NULL)
        {
            for (i=0; (cmdbuf[i] != '\n'); ++i) 
                if (i > 399) break;
            cmdbuf[i] = '\0';    /* remove trailing carriage return */
            if (cmdbuf[0] == ':') LGCommand(&(cmdbuf[1]));
            else SHCommand(&(cmdbuf[0]));
            ptr = fgets(cmdbuf, 400, startup);
        }  /* end while */
    }  /* end if startup */
    else inode = 0;

            /* look in current directory */
    startup = fopen(".gremlinrc", "r");
    if (startup != NULL)
    {
        fstat(startup, &buf);
        if (buf.st_ino != inode)  /* This isn't the same file as above */
        {
            ptr = fgets(cmdbuf, 400, startup);
            while (ptr != NULL)
            {
                for (i=0; (cmdbuf[i] != '\n'); ++i) 
                    if (i > 399) break;
                cmdbuf[i] = '\0';    /* remove trailing carriage return */
                if (cmdbuf[0] == ':') LGCommand(&(cmdbuf[1]));
                else SHCommand(&(cmdbuf[0]));
                ptr = fgets(cmdbuf, 400, startup);
            }  /* end while */
        }  /* end if buf */
    }  /* end if startup */

    while ( TRUE )    /* Exits through the 'quit' command */
    {
        Consume = TRUE;
        UNForget();

#ifdef SIGTINT
        GREnableTablet();
              /*  Allow keyboard interrupts while waiting
               * for cursor input.
               */
        sigrelse(SIGTINT);
        button = GRGetButton(tablet, &cux, &cuy);
              /*  Ignore keyboard interrupts while processing commands.
               */
        sighold(SIGTINT);
        GRDisableTablet();
        TxMsgOK();
        MNInterpretCursor(button, cux, cuy);
        if (Consume) CP(); 
#else SIGTINT
        i = 1 << fileno(stdin);
        GREnableTablet();
        i |= (1 << fileno(tablet));
        (void) select(20, &i, 0, 0, 0);
        if (i & (1 << fileno(stdin)))
        {
            TxMsgOK();
            OnCommand();
        }
        else
        {
            if ((button = GRGetButton(tablet, &cux, &cuy)) != -4) 
            {
                GRDisableTablet();
                TxMsgOK();
                MNInterpretCursor(button, cux, cuy);
                if (Consume) CP(); 
            }
        }
#endif SIGTINT

    }  /* end while */

}  /* end main */


OnStop(signo)
int signo;

/*-----------------------------------------------------------------------------
 *    This procedure handles stop signals.
 *
 *    Results:    None.
 *
 *    Side Effects:
 *    The text display is reset, we wait to get restarted, then
 *    redisplay text.
 *-----------------------------------------------------------------------------
 */

{
    TxClose();
    signal(signo, SIG_DFL);
    sigsetmask(0);
    (void) kill(0, signo);
    signal(signo, OnStop);
    SHRedis();
}


OnCommand()

/*-----------------------------------------------------------------------------
 *    This routine responds to interrupts from the command terminal and
 *    then processes commands as long as there is input.
 *
 *    Results:    None.
 *
 *    Side Effects:
 *    Whatever is done by the commands.
 *-----------------------------------------------------------------------------
 */

{

#ifdef SIGTINT
    long charcount;
#else SIGTINT
    int i;
    struct timeval selectpoll;
#endif SIGTINT

    static char cmd, lastcmd;
    int repeat;

    GRDisableTablet();

#ifdef SIGTINT
    (void) ioctl(fileno(stdin), TIOCLBIC, (char *) &lintrup);
#endif SIGTINT


    while (TRUE)
    {

    /* If there is no more input, then reenable the signal and return */


#ifdef SIGTINT
    (void) ioctl(fileno(stdin), FIONREAD, (char *) &charcount);
    if (charcount == 0)
    {
        GREnableTablet();
        (void) ioctl(fileno(stdin), TIOCLBIS, (char *) &lintrup);
        return;
    }
#else SIGTINT
    i = 1 << fileno(stdin);
    selectpoll.tv_sec = 0l;
    selectpoll.tv_usec = 0l;
    if (select(20, &i, 0, 0, &selectpoll) <= 0)
    {
        GREnableTablet();
        return;
    }
#endif SIGTINT


    /* Read the command and call the long or short command routine */

    cmd = TxGetChar();
    if (cmd == '.') 
    {
        repeat = TRUE;
        cmd = lastcmd;
    }
    else 
    {
        repeat = FALSE;
        lastcmd = cmd;
    }
    if (cmd == ':') 
    {
        if (!repeat) TxGetLine(":",cmdbuf,400);
        LGCommand(cmdbuf);
    }
    else 
    {
        TxLine(inline);
        if (cmd != '\') putchar(cmd);
        (void) fflush(stdout);
        SHCommand(&cmd);
    }
    TxLine(inline);
    printf("                                                                              ");
    TxLine(inline);
    if (Consume) CP();
    Consume = TRUE;
    UNForget();
    }
}

