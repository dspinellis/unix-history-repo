/*
 * @(#)main.c	1.2	%G%
 *
 * Main program for the SUN Gremlin picture editor.
 *
 * Many of the routines in SUN Gremlin are direct descendants of
 * their counterparts in the original Gremlin written for the AED
 * by Barry Roitblat.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include <suntool/fullscreen.h>
#include <suntool/menu.h>
#include <suntool/wmgr.h>
#include <sunwindow/cms.h>
#include <sunwindow/win_ioctl.h>
#include <sun/fbio.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <errno.h>
#include "gremlin.h"
#include "icondata.h"

#define RETAIN TRUE		/* use retained subwindow for pix_sw */

#ifdef maybefaster
char *_image;
int _bytesperline, _maxx, _maxy;
#endif

/* database imports */

extern ELT *DBRead();
extern POINT *PTMakePoint();
extern FILE *POpen();

/* imports from startup.c */

extern STERROR;
extern STgremlinrc();

/* imports from menu.c */

extern MNDisplayMenu();
extern MNInitMenu();

/* graphics imports */

extern GRBlankGrid();
extern GRCurrentSet();
extern GRCurrentSetOn();
extern GRDisplayGrid();
extern GRFontInit();

/* imports from undodb.c */

extern UNForget();
extern UNELT *unlist;
extern UNELT *unback;
 
/* imports from long.c */

extern SHOWPOINTS;
extern char *Editfile;
extern CP();
extern LGQuit();
extern nop();

/* imports from short.c */

extern SHCommand();
extern SHUpdate();

/* imports from C */

extern char *malloc();
extern char *sprintf();
extern char *strcat();
extern char *strcpy();

/* imports from strings.c */

extern char version[];
extern char GLibrary[];

/* Declaration of Globals */

ELT arhead;
ELT *cset;
ELT *MEN[NUSER];
ELT *PICTURE;

POINT *BACKPOINT;
POINT MENPOINT[NUSER];
POINT *POINTLIST;

int Artmode = FALSE;
int Adjustment = NOADJ;
int Alignment = 4;
int CBRUSH = 5;
int CFONT = 1;
int CJUST = 0;
int CSIZE = 1;
int CSTIPPLE = 1;
int GravityOn = FALSE;
int Gridon = FALSE;
int Gridsize = 32;
int Orientation;
int SEQ = 0;
int CHANGED = FALSE;
int SEARCH = TRUE;
int SymbolicLines = 0;
int newfileformat = 1;			/* 1=sungremlinfile, 0=gremlinfile */
int TOOLINSTALLED = 0;
int (*lastcommand)();			/* last command's routine pointer */
int lasttext = FALSE;			/* TRUE if last command uses text */

float PX, PY;				/* user point coordinate */
float Lastx, Lasty;			/* last user point coordinate */

long timeon_ms = 580;			/* current set flash on period */
long timeon_s = 0;
long timeoff_ms = 180;			/* current set flash off period */
long timeoff_s = 0;

int FLASH_READY = 0;			/* TRUE if time to flash current set */
int CsetOn = 1;				/* TRUE if current set displayed */
static struct itimerval itime;		/* for ALARM signals */
static alrm_sighandler();

int SUN_XORIGIN = 0;			/* top left corner in gremlin coords */
int SUN_YORIGIN = 511;
	
static char SccsId [] = "@(#)main.c	1.2   (Berkeley)      %G%";

/* imports from menu.c */

extern menu_left();
extern menu_middle();
extern menu_right();
extern menu_winexit();
extern mouse_move();


/* imports from pix.c */

extern pix_left();
extern pix_middle();
extern pix_right();
extern pix_winexit();


/* imports from suntext.c */

extern text_left();
extern text_middle();
extern text_right();
extern text_winexit();
extern text_putvalue();
extern text_output();
extern TxInit();
extern TxPutMsg();

struct cursor kbd_cursor  =  {
    4, 8, PIX_SRC^PIX_DST, &kbdcursor_pr
};

struct cursor main_cursor  =  {
    0, 0, PIX_SRC^PIX_DST, &uparrow_pr
};

static struct cursor menu_cursor = {
    3, 3, PIX_SRC^PIX_DST, &diamond_pr
};

static struct icon gremlin_icon = {
    TOOL_ICONWIDTH, TOOL_ICONHEIGHT, NULL,
    {0, 0, TOOL_ICONWIDTH, TOOL_ICONHEIGHT},
    &gremlin_icon_pr, {0, 0, 0, 0}, NULL, NULL, 0
};

/* tool window stuff */

#define DEV_FB "/dev/fb"
struct tool *tool;
struct rect tool_size;
int tool_fd;
int rootfd;
char namestripe[256];
static tool_selected();
static init_tool();
static sigwinched();

/* for handling tool manager menu */

extern struct menu *wmgr_toolmenu;
extern struct menuitem *menu_display();

/* text subwindow */

struct toolsw *text_sw;
struct pixwin *text_pw;
struct rect text_size;
int text_fd;
struct pixfont *text_pf;
static init_text();
static text_selected();
static text_sighandler();

/* menu subwindow */

struct toolsw *menu_sw;
struct pixwin *menu_pw;
struct rect menu_size;
int menu_fd;
static init_menu();
static menu_selected();
static menu_sighandler();

/* pix subwindow */

struct toolsw *pix_sw;
struct pixwin *pix_pw;
struct rect pix_size;
int pix_fd;
static init_pix();
static pix_selected();
static pix_sighandler();
struct pixrect *cset_pr;
struct pixrect *scratch_pr;
struct pixrect *retained_pr;

/*
 * This routine prints an error message in the text subwindow.
 */
error(s)
char *s;
{
    TxPutMsg(s);
}


main(argc, argv)
int argc;
char *argv[];
{
    char *arg, *file, *gremlinrc, *usage;

    file = "";
    gremlinrc = "";
    usage = "usage: gremlin [-o] [-s <.gremlinrc>] [file]\n";

    while (--argc > 0) {
	arg = *++argv;
	if (*arg != '-')
	    file = arg;
	else {
	    switch (*++arg) {
		case 's':
		    if (*++arg == '\0')
			if (--argc > 0)
			    arg = *++argv;
		    if (argc < 0) {
			printf("%s", usage);
			exit(1);
		    }
		    gremlinrc = arg;
		    break;
		case 'o':
		    newfileformat = 0;
		    break;
		default:
		    printf("%s", usage);
		    exit(1);
	    }
	}
    }

    strcpy(namestripe, version);
    tool = tool_create(namestripe, TOOL_NAMESTRIPE, (struct rect *) NULL, 
			&gremlin_icon /*(struct icon *) NULL*/);
    if (tool == (struct tool *) NULL)
	exit(1);

    text_sw = tool_createsubwindow(tool, "text_sw", TOOL_SWEXTENDTOEDGE, 
					TEXTSW_HEIGHT);

    menu_sw = tool_createsubwindow(tool, "menu_sw", 
					MENUSW_WIDTH, TOOL_SWEXTENDTOEDGE);

    pix_sw = tool_createsubwindow(tool, "pix_sw", TOOL_SWEXTENDTOEDGE, 
						TOOL_SWEXTENDTOEDGE);

    if ((text_sw == (struct toolsw *)NULL) ||
		(menu_sw == (struct toolsw *)NULL) ||
			(pix_sw == (struct toolsw *)NULL)) {
	printf("error creating subwindows\n");
	exit(1);
    }

    init_tool();
    init_text();
    init_menu();
    init_pix();

    /* set up gremlin variables */
    main_init(file, gremlinrc);

    /* install tool in tree of windows */
    signal(SIGWINCH, sigwinched);
    tool_install(tool);
    TOOLINSTALLED = 1;

    /* if current set flash off period non-zero, set up SIGALRM */
    if ((timeoff_ms != 0l) || (timeoff_s != 0l)) {
	signal(SIGALRM, alrm_sighandler);
	itime.it_interval.tv_sec = itime.it_value.tv_sec = timeon_s;
	itime.it_interval.tv_usec = itime.it_value.tv_usec = timeon_ms * 1000;
	setitimer(ITIMER_REAL, &itime, (struct itimerval *) NULL);
    }

    /* handle input */
    tool_select(tool, 0);

    /* cleanup */
    tool_destroy(tool);
    exit(0);
}


/*
 * Set input mask for the tool border.
 */
static
set_tool_input()
{
    struct inputmask im;

    input_imnull(&im);
    win_setinputcodebit(&im, MS_LEFT);
    win_setinputcodebit(&im, MS_MIDDLE);
    win_setinputcodebit(&im, MS_RIGHT);
    win_setinputcodebit(&im, LOC_STILL);

    win_setinputmask(tool_fd, &im, NULL, WIN_NULLLINK);
}


static
init_tool()
{
    tool_fd = tool->tl_windowfd;
    tool->tl_io.tio_selected = tool_selected;
    
    set_tool_input();

    if ((rootfd = open("/dev/win0", 0)) == -1) {
	printf("can't open root window\n");
	exit(1);
    }

    fix_tool_size();
}


/*
 * Set the size of the entire gremlin window.
 */
fix_tool_size()
{
    struct rect icon_rect;

    if (wmgr_iswindowopen(tool_fd)) 
	win_getrect(tool_fd, &tool_size);
    else
	win_getsavedrect(tool_fd, &tool_size);

    tool_size.r_width = 2 * tool_borderwidth(tool) + 
			    tool_subwindowspacing(tool) + 
			    MENUSW_WIDTH + PIXSW_WIDTH;
    tool_size.r_height = tool_stripeheight(tool) + tool_borderwidth(tool) + 
			    tool_subwindowspacing(tool) +
			    2 +	/* don't know why */
			    TEXTSW_HEIGHT + PIXSW_HEIGHT;

    /* don't allow window to open in lower left corner */
    if (tool_size.r_left < 100)
	tool_size.r_left = 100;
    if (rect_bottom(&tool_size) >= 700)
	tool_size.r_top -= rect_bottom(&tool_size) - 699;

    /* land icon near upper left corner */
    icon_rect.r_left = 0;
    icon_rect.r_top = 64;
    icon_rect.r_width = 64;
    icon_rect.r_height = 64;

    if (wmgr_iswindowopen(tool_fd)) {
	win_setrect(tool_fd, &tool_size);
	win_setsavedrect(tool_fd, &icon_rect);
    }
    else {
	win_setrect(tool_fd, &icon_rect);
	win_setsavedrect(tool_fd, &tool_size);
    }
}


static
init_text()
{
    struct inputmask im;

    text_fd = text_sw->ts_windowfd;
    text_pw = pw_open(text_fd);

    text_sw->ts_io.tio_handlesigwinch = text_sighandler;
    text_sw->ts_io.tio_selected = text_selected;

    win_setcursor(text_fd, &kbd_cursor);

    input_imnull(&im);
    win_setinputcodebit(&im, MS_LEFT);
    win_setinputcodebit(&im, MS_MIDDLE);
    win_setinputcodebit(&im, MS_RIGHT);
    win_setinputcodebit(&im, LOC_STILL);
    win_setinputcodebit(&im, LOC_WINEXIT);

    im.im_flags |= IM_ASCII;
    win_setinputmask(text_fd, &im, NULL, WIN_NULLLINK);
}


static
init_menu()
{
    struct inputmask im;

    menu_fd = menu_sw->ts_windowfd;
    menu_pw = pw_open(menu_fd);

    menu_sw->ts_io.tio_handlesigwinch = menu_sighandler;
    menu_sw->ts_io.tio_selected = menu_selected;

    win_setcursor(menu_fd, &menu_cursor);

    input_imnull(&im);
    win_setinputcodebit(&im, MS_LEFT);
    win_setinputcodebit(&im, MS_MIDDLE);
    win_setinputcodebit(&im, MS_RIGHT);
    win_setinputcodebit(&im, LOC_MOVE);
    win_setinputcodebit(&im, LOC_WINEXIT);
    win_setinputcodebit(&im, LOC_STILL);

    im.im_flags |= IM_ASCII;
    win_setinputmask(menu_fd, &im, NULL, WIN_NULLLINK);
}


static
init_pix()
{
    struct inputmask im;
    struct fbtype fb;
    struct mpr_data *md;
    int height, width;
    int fd;

    pix_fd = pix_sw->ts_windowfd;
    pix_pw = pw_open(pix_fd);

    pix_sw->ts_io.tio_handlesigwinch = pix_sighandler;
    pix_sw->ts_io.tio_selected = pix_selected;

    win_setcursor(pix_fd, &main_cursor);

    input_imnull(&im);
    win_setinputcodebit(&im, MS_LEFT);
    win_setinputcodebit(&im, MS_MIDDLE);
    win_setinputcodebit(&im, MS_RIGHT);
    win_setinputcodebit(&im, LOC_STILL);
    win_setinputcodebit(&im, LOC_WINEXIT);

    im.im_flags |= IM_ASCII;
    win_setinputmask(pix_fd, &im, NULL, WIN_NULLLINK);

    /* determine screen physical dimensions */
    if ((fd = open(DEV_FB, 1)) < 0) {		/* must be open for writing */
	printf("init_pix: can't open %s\n", DEV_FB);
	exit(1);
    }

    /* get frame buffer characteristics */
    if (ioctl(fd, FBIOGTYPE, (char *) &fb) < 0) {
	printf("init_pix: ioctl (FBIOGTYPE)\n");
	exit(1);
    }
    close(fd);

    /* determine maximum physical size of picture subwindow */
    height = fb.fb_height - TEXTSW_HEIGHT - 
			    tool_stripeheight(tool) - 
			    tool_borderwidth(tool) - 
			    tool_subwindowspacing(tool);
    width = fb.fb_width - MENUSW_WIDTH - 
			    tool_subwindowspacing(tool) -
			    tool_borderwidth(tool) * 2;

    /* create pixrect for retaining image of current set */
    cset_pr = mem_create(width, height, 1);
    if (cset_pr == NULL) {
	printf("can't create cset_pr\n");
	exit(1);
    }

    /* create pixrect to do all element drawing in memory (faster) */
    scratch_pr = mem_create(width, height, 1);
    if (scratch_pr == NULL) {
	printf("can't create scratch_pr\n");
	exit(1);
    }

#ifdef RETAIN
    /* create retained pixrect for picture subwindow */
    retained_pr = mem_create(width, height, 1);
    if (retained_pr == (struct pixrect *) NULL) {
	printf("can't create retained_pr\n");
	exit(1);
    }

    /* add retained pixrect to the pix subwindow pixwin */
    pix_pw->pw_prretained = retained_pr;

    /* 
     * The pix subwindow width and height MUST be initialized before
     * any drawing can take place when using a retained pixrect.
     * The display routine DISScreenAdd() uses these dimensions to
     * minimize the area of the mem_pixrect (scratch_pr) which must
     * be cleared prior to drawing an element and is called via
     * SHUpdate() at the end of main_init().
     */
    pix_size.r_width = PIXSW_WIDTH;
    pix_size.r_height = PIXSW_HEIGHT;
#endif

#ifdef maybefaster
    md = (struct mpr_data *) scratch_pr->pr_data;
    _image = (char *) md->md_image;
    _bytesperline = md->md_linebytes;
    _maxx = _bytesperline << 3;
    _maxy = scratch_pr->pr_size.y;
#endif
}


/* 
 * This routine catches the normal tool manager quit menuitem
 * and handles it slightly differently if no write has occurred
 * since the last change to the picture.
 * WARNING: this routine depends upon menu_display() returning
 * mi->mi_data = 2 for the QUIT menuitem and
 * mi->mi_data = 1 for the REDISPLAY menuitem.
 */
static
tool_selected(nullsw, ibits, obits, ebits, timer)
caddr_t *nullsw;
int *ibits, *obits, *ebits;
struct timeval **timer;
{
    struct inputevent ie;
    struct inputmask im;
    struct menuitem *mi;

    if (input_readevent(tool_fd, &ie) < 0) {
	printf("error: tool_selected()\n");
	return;
    }

    switch (ie.ie_code) {
	case LOC_STILL:
/*	    GRCurrentSetOn();			*/
	    break;
	case MS_LEFT:
	    if (wmgr_iswindowopen(tool_fd))
		wmgr_top(tool_fd, rootfd);
	    else
		wmgr_open(tool_fd, rootfd);
	    break;
	case MS_MIDDLE:
	    wmgr_changerect(tool_fd, tool_fd, &ie, 1, 1);
	    break;
	case MS_RIGHT:
	    /* force mouse button input only for pop-up menu */
	    input_imnull(&im);
	    win_setinputcodebit(&im, MS_LEFT);
	    win_setinputcodebit(&im, MS_MIDDLE);
	    win_setinputcodebit(&im, MS_RIGHT);
	    win_setinputmask(tool_fd, &im, NULL, WIN_NULLLINK);

	    wmgr_setupmenu(tool_fd);
	    mi = menu_display(&wmgr_toolmenu, &ie, tool_fd);
	    if (mi == (struct menuitem *) NULL)
		break;

	    if (((int) mi->mi_data == 1)		/* REDISPLAY !! */
			    && wmgr_iswindowopen(tool_fd))
		SHUpdate();
	    else if ((int) mi->mi_data == 2) 		/* QUIT !! */
		LGQuit();
	    else
		wmgr_handletoolmenuitem(wmgr_toolmenu, mi, tool_fd, rootfd);

	    set_tool_input();	/* reset tool input */
	    break;
    }

    *ibits = *obits = *ebits = 0;
}


static
text_selected(nullsw, ibits, obits, ebits, timer)
caddr_t *nullsw;
int *ibits, *obits, *ebits;
struct timeval **timer;
{
    struct inputevent ie;

    if (input_readevent(text_fd, &ie) < 0) {
	printf("error: text_selected()\n");
	return;
    }

    switch (ie.ie_code) {
	case LOC_STILL:
	    check_cset();
	    break;
	case MS_LEFT:
	    text_left(&ie);
	    break;
	case MS_MIDDLE:
	    text_middle(&ie);
	    break;
	case MS_RIGHT:
	    text_right(&ie);
	    break;
	case LOC_WINEXIT:
	    text_winexit(&ie);
	    break;
	default:
	    if ((ie.ie_code <= ASCII_LAST) && (ie.ie_code >= ASCII_FIRST))
		text_output(ie.ie_code);
	    break;
    }

    *ibits = *obits = *ebits = 0;
}


static
menu_selected(nullsw, ibits, obits, ebits, timer)
caddr_t *nullsw;
int *ibits, *obits, *ebits;
struct timeval **timer;
{
    struct inputevent ie;
    char shcmd[2];

    if (input_readevent(menu_fd, &ie) < 0) {
	printf("error: menu_selected()\n");
	return;
    }

    switch (ie.ie_code) {
	case LOC_MOVE:
	    mouse_move(&ie);
	    break;
	case LOC_STILL:
	    check_cset();
	    break;
	case MS_LEFT:
	    menu_left(&ie);
	    break;
	case MS_MIDDLE:
	    menu_middle(&ie);
	    break;
	case MS_RIGHT:
	    menu_right(&ie);
	    break;
	case LOC_WINEXIT:
	    menu_winexit(&ie);
	    break;
	default:
	    if ((ie.ie_code <= ASCII_LAST) && (ie.ie_code >= ASCII_FIRST)) {
		if ((shcmd[0] = ie.ie_code) != '.')
		    lasttext = FALSE;
		shcmd[1] = '\0';
		SHCommand(shcmd);
	    }
	    break;
    }

    *ibits = *obits = *ebits = 0;
}


static
pix_selected(nullsw, ibits, obits, ebits, timer)
caddr_t *nullsw;
int *ibits, *obits, *ebits;
struct timeval **timer;
{
    struct inputevent ie;
    char shcmd[2];

    if (input_readevent(pix_fd, &ie) < 0) {
	printf("error: pix_selected()\n");
	return;
    }

    switch (ie.ie_code) {
	case LOC_STILL:
	    check_cset();
	    break;
	case MS_LEFT:
	    pix_left(&ie);
	    break;
	case MS_MIDDLE:
	    pix_middle(&ie);
	    break;
	case MS_RIGHT:
	    pix_right(&ie);
	    break;
	case LOC_WINEXIT:
	    pix_winexit(&ie);
	    break;
	default:
	    if ((ie.ie_code <= ASCII_LAST) && (ie.ie_code >= ASCII_FIRST)) {
		if ((shcmd[0] = ie.ie_code) != '.')
		    lasttext = FALSE;
		shcmd[1] = '\0';
		SHCommand(shcmd);
	    }
	    break;
    }

    *ibits = *obits = *ebits = 0;
}


static
sigwinched()
{
    tool_sigwinch(tool);

    win_getrect(tool_fd, &tool_size);
    /*
    printf("tool: left %d, top %d, width %d, height %d\n", tool_size.r_left,
	tool_size.r_top, tool_size.r_width, tool_size.r_height);
    */
}


static
text_sighandler()
{
    pw_damaged(text_pw);

    win_getrect(text_fd, &text_size);
    pw_writebackground(text_pw, 0, 0, 1024, 1024, PIX_SRC);
    text_putvalue();

    pw_donedamaged(text_pw);
}


static
menu_sighandler()
{
    pw_damaged(menu_pw);

    win_getrect(menu_fd, &menu_size);
    pw_writebackground(menu_pw, 0, 0, 1024, 1024, PIX_SRC);
    MNDisplayMenu();
    /*
    printf("menu: left %d, top %d, width %d, height %d\n", menu_size.r_left,
	menu_size.r_top, menu_size.r_width, menu_size.r_height);
    */

    pw_donedamaged(menu_pw);
}


static
pix_sighandler()
{
    GRCurrentSetOn();	/* make current set on */
    pw_damaged(pix_pw);

    if (STERROR) {	/* check for startup error reading .gremlinrc */
	TxPutMsg("error in .gremlinrc");
	STERROR = 0;
    }

    win_getrect(pix_fd, &pix_size);

#ifdef RETAIN
    pw_repairretained(pix_pw);
#else
    SHUpdate();
#endif

    pw_donedamaged(pix_pw);
}


/*
 *  One time only start-up initialization.
 */
main_init(file, gremlinrc)
char *file;
char *gremlinrc;
{
    FILE *fp;
    POINT *pos;
    int i, fd;
    char *prealname;

    signal(SIGINT, SIG_IGN);			/* ignore interrupts */
    TxInit();					/* text subwindow init */
    MNInitMenu();				/* menu subwindow init */
    PSetPath(".");				/* file search path */
    Editfile = malloc(128);			/* current picture name */

    POINTLIST = PTInit();			/* drawing points */
    BACKPOINT = PTInit();			/* backup point list */

    PICTURE = DBInit();				/* picture database */
    cset = DBInit();				/* current set */
    for (i=0; i<4; ++i)				/* set buffers */
	MEN[i] = DBInit();

    unlist = unback = NULL;			/* undo pointers */
    make_arrowhead();				/* for > command */
    lastcommand = nop;				/* no last command yet */

    STgremlinrc(gremlinrc);			/* .gremlinrc processing */
    GRFontInit();	       /* must be called after CSIZE & CFONT set */

    strcpy(Editfile, file);			/* find edit file */
    if (*file != '\0') {
	fp = POpen(Editfile, &prealname, SEARCH);

	if (fp == NULL) {
	    strcat(namestripe, file);
	    error("creating new file");
	}
	else {
	    fclose(fp);
	    strcat(namestripe, prealname);
	    PICTURE = DBRead(Editfile, &Orientation, &pos);
	    if ((fd = open(prealname, O_WRONLY | O_APPEND)) < 0)
		strcat(namestripe, " (read only)");
	    else
		close(fd);
	}
    }
    else {
	strcat(namestripe, "new file");
    }

#ifdef RETAIN
    /* 
     * Update pix subwindow now so that the retained pixrect is set before
     * the first SIGWINCH.
     */
    SHUpdate();
#endif
}  /* end main_init */


/*
 *  Make arrowhead element for later drawing.
 */
make_arrowhead()
{
    POINT *pos;

    pos = PTInit();			/* initialize arrowhead template */
    (void) PTMakePoint(0.0, 0.0, &pos);
    (void) PTMakePoint(-5.51, 3.51, &pos);
    (void) PTMakePoint(-3.51, 0.0, &pos);
    (void) PTMakePoint(-5.51, -3.51, &pos);
    (void) PTMakePoint(0.0, 0.0, &pos);
    arhead.type = VECTOR;
    arhead.ptlist = pos;
    arhead.brushf = 0;			/* brush filled in when used */
    arhead.size = 0;
    arhead.textpt = malloc(1);
    *(arhead.textpt) = '\0';
}


/*
 * Nothing has happened for a while, so check to see if its time
 * to flash the current set.  If so, set the SIGALRM timer for the
 * appropriate period.
 */
check_cset()
{
    if (FLASH_READY /*&& wmgr_iswindowopen(tool_fd) */) {
	GRCurrentSet();				/* XOR current set */

	if (CsetOn) {				/* set off period */
	    itime.it_interval.tv_sec = 
	    itime.it_value.tv_sec = timeon_s;
	    itime.it_interval.tv_usec = 
	    itime.it_value.tv_usec = timeon_ms * 1000;
	}
	else {					/* set on period */
	    itime.it_interval.tv_sec = 
	    itime.it_value.tv_sec = timeoff_s;
	    itime.it_interval.tv_usec = 
	    itime.it_value.tv_usec = timeoff_ms * 1000;
	}

	setitimer(ITIMER_REAL, &itime, NULL);
	FLASH_READY = 0;
    }
}


/*
 * This routine handles the timer signals indicating that it is time
 * to flash the current set.  Since the screen may not be in a consistent
 * state, we simply set the FLASH_READY flag and return.  When a LOC_STILL
 * input event arrives later, this flag will be checked and the current
 * set flashed then.
 */
static
alrm_sighandler()
{
    FLASH_READY = 1;
}
