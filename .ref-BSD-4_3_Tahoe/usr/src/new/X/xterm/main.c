/*
 *	@(#)main.c	1.15 (Berkeley/CSRG) 5/31/88
 *	$Source: /u1/X/xterm/RCS/main.c,v $
 *	$Header: main.c,v 10.101 86/12/01 16:58:10 swick Rel $
 */

#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* main.c */

#ifndef lint
static char csrg_id[] = "@(#)main.c	1.15\t(Berkeley/CSRG)\t5/31/88";
static char sccs_id[] = "@(#)main.c\tX10/6.6B\t12/28/86";
#endif	lint

#include <sys/param.h>	/* for NOFILE */
#include <pwd.h>
#include <grp.h>
#include <sgtty.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <stdio.h>
#include <sys/file.h>
#include <errno.h>
#include <signal.h>
#include <strings.h>
#include <setjmp.h>
#include <utmp.h>
#include <X/Xlib.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "data.h"
#include "error.h"
#include "main.h"

int switchfb[] = {0, 2, 1, 3};

static int reapchild ();

static char *brdr_color;
static char **command_to_exec;
#ifdef TIOCCONS
static int Console;
#endif TIOCCONS
static struct  sgttyb d_sg = {
        0, 0, 0177, CKILL, EVENP|ODDP|ECHO|XTABS|CRMOD
};
static struct  tchars d_tc = {
        CINTR, CQUIT, CSTART,
        CSTOP, CEOF, CBRK,
};
static struct  ltchars d_ltc = {
        CSUSP, CDSUSP, CRPRNT,
        CFLUSH, CWERASE, CLNEXT
};
static int d_disipline = NTTYDISC;
static int d_lmode = LCRTBS|LCRTERA|LCRTKIL|LCTLECH;
static char def_bold_font[] = DEFBOLDFONT;
static char def_font[] = DEFFONT;
static char def_title_font[] = DEFTITLEFONT;
static char def_icon_font[] = DEFICONFONT;
static char display[256];
static char etc_utmp[] = "/etc/utmp";
static char *get_ty;
static char *iconbitmap;
static int inhibit;
static int log_on;
static int login_shell;
static char passedPty[2];	/* name if pty if slave */
static int loginpty;
static char *tekiconbitmap;
static int tslot;
static char *xdef[] = {
	"ActiveIcon",		/* DEF_ACTIVEICON */
	"AllowIconInput",	/* DEF_ALLOWICONINPUT */
	"AutoRaise",		/* DEF_AUTORAISE */
	"Background",		/* DEF_BACKGROUND */
	"BodyFont",		/* DEF_BODYFONT */
	"BoldFont",		/* DEF_BOLDFONT */
	"Border",		/* DEF_BORDER */
	"BorderWidth",		/* DEF_BORDERWIDTH */
	"C132",			/* DEF_C132 */
	"Curses",		/* DEF_CURSES */
	"Cursor",		/* DEF_CURSOR */
	"cursorShape",		/* DEF_CURSORSHAPE */
	"DeiconifyWarp",	/* DEF_DEICONWARP */
	"Foreground",		/* DEF_FOREGROUND */
	"GrayBorder",		/* DEF_GRAYBORDER */
	"IconBitmap",		/* DEF_ICONBITMAP */
	"IconFont",		/* DEF_ICONFONT */
	"IconStartup",		/* DEF_ICONSTARTUP */
	"InternalBorder",	/* DEF_INTERNALBORDER */
	"JumpScroll",		/* DEF_JUMPSCROLL */
#ifdef KEYBD
	"KeyBoard",		/* DEF_KEYBOARD */
#endif KEYBD
	"LogFile",		/* DEF_LOGFILE */
	"Logging",		/* DEF_LOGGING */
	"LogInhibit",		/* DEF_LOGINHIBIT */
	"LoginShell",		/* DEF_LOGINSHELL */
	"MarginBell",		/* DEF_MARGINBELL */
	"Mouse",		/* DEF_MOUSE */
	"NMarginBell",		/* DEF_NMARGINBELL */
	"PageOverlap",		/* DEF_PAGEOVERLAP */
	"PageScroll",		/* DEF_PAGESCROLL */
	"ReverseVideo",		/* DEF_REVERSEVIDEO */
	"ReverseWrap",		/* DEF_REVERSEWRAP */
	"SaveLines",		/* DEF_SAVELINES */
	"ScrollBar",		/* DEF_SCROLLBAR */
	"ScrollInput",		/* DEF_SCROLLINPUT */
	"ScrollKey",		/* DEF_SCROLLKEY */
	"SignalInhibit",	/* DEF_SIGNALINHIBIT */
	"StatusLine",		/* DEF_STATUSLINE */
	"StatusNormal",		/* DEF_STATUSNORMAL */
	"TekIconBitmap",	/* DEF_TEKICONBITMAP */
	"TekInhibit",		/* DEF_TEKINHIBIT */
	"TextUnderIcon",	/* DEF_TEXTUNDERICON */
	"TitleBar",		/* DEF_TITLEBAR */
	"TitleFont",		/* DEF_TITLEFONT */
	"VisualBell",		/* DEF_VISUALBELL */
	"VisBellDelay",		/* DEF_VISBELLDELAY */
	"AudibleBell",		/* DEF_AUDIBLEBELL */
	"DropMenu",		/* DEF_DROPMENU	*/
	"UniqueSuffix",		/* DEF_UNIQUESUFFIX */
#ifdef ALLOWUNSHIFTEDSELECTION
	"UnshiftedSelection",	/* DEF_UNSHIFTEDSELECTION */
#endif
	0,
};
#ifdef UTMP
static int added_utmp_entry;
#endif UTMP

main (argc, argv)
int argc;
char **argv;
{
	register Screen *screen = &term.screen;
	register char *strind;
	register int i, pty;
	register char **cp;
	short fnflag = 0;	/* True iff -fn option used */
	short fbflag = 0;	/* True iff -fb option used */
	int Xsocket, mode;
	extern onalarm();
	char *malloc();
	char *basename();
	int xerror(), xioerror();
#ifdef KEYBD
	extern char *keyboardtype;	/* used in XKeyBind.c */
	char *getenv();
#endif KEYBD

	xterm_name = (strcmp(*argv, "-") == 0) ? "xterm" : basename(*argv);

	term.flags = WRAPAROUND | SMOOTHSCROLL | AUTOREPEAT;
	screen->border = DEFBORDER;
	screen->borderwidth = DEFBORDERWIDTH;
	screen->reversestatus = TRUE;
	screen->mappedVwin = &screen->fullVwin;
	screen->mappedTwin = &screen->fullTwin;
	screen->audiblebell = TRUE;
	screen->visbelldelay = VISBELLDELAY;
	f_b = def_bold_font;
	f_n = def_font;
	f_t = def_title_font;
	f_i = def_icon_font;

	display[0] = '\0';
#ifdef KEYBD
	if((strind = getenv("KEYBD")) && *strind) {
		if((keyboardtype = malloc(strlen(strind) + 1)) == NULL)
			SysError(ERROR_KMALLOC);
		strcpy(keyboardtype, strind);
	}
#endif KEYBD

	/*
	 * go get options out of default file
	 */
	for(i = 0, cp = xdef ; *cp ; i++, cp++) {
		if(!(strind = XGetDefault(xterm_name, *cp)))
			continue;
		switch(i) {
		 case DEF_ACTIVEICON:
			if (strcmp (strind, "on") == 0)
				screen->active_icon = TRUE;
			continue;
		 case DEF_ALLOWICONINPUT:
		 	if (strcmp (strind, "on") == 0)
			        term.flags |= ICONINPUT;
			continue;
		 case DEF_AUTORAISE:
			if (strcmp (strind, "on") == 0) 
				screen->autoraise = TRUE;
			continue;
		 case DEF_BACKGROUND:
			back_color = strind;
			continue;
		 case DEF_BODYFONT:
			f_n = strind;
			fnflag = TRUE;
			continue;
		 case DEF_BOLDFONT:
			f_b = strind;
			fbflag = TRUE;
			continue;
		 case DEF_BORDER:
			brdr_color = strind;
			continue;
		 case DEF_BORDERWIDTH:
			screen->borderwidth = atoi (strind);
			continue;
		 case DEF_C132:
			if (strcmp (strind, "on") == 0) 
				screen->c132 = TRUE;
			continue;
		 case DEF_CURSES:
			if (strcmp (strind, "on") == 0) 
				screen->curses = TRUE;
			continue;
		 case DEF_CURSOR:
			curs_color = strind;
			continue;
		 case DEF_CURSORSHAPE:
			if (curs_shape)
				free(curs_shape);
			if ((curs_shape = malloc(strlen(strind) + 1)) == NULL)
				SysError(ERROR_KMALLOC); /* not correct error */
			strcpy(curs_shape, strind);
			continue;
		 case DEF_DEICONWARP:
			if (strcmp (strind, "on") == 0)
				screen->deiconwarp = TRUE;
			continue;
		 case DEF_FOREGROUND:
			fore_color = strind;
			continue;
		 case DEF_GRAYBORDER:
			if (strcmp (strind, "off") == 0)
				grayborder=FALSE;
			continue;
		 case DEF_ICONBITMAP:
			iconbitmap = strind;
			continue;
		 case DEF_ICONFONT:
		 	f_i = strind;
			continue;
		 case DEF_ICONSTARTUP:
			if (strcmp (strind, "on") == 0)
				screen->icon_show = -1;
			continue;
		 case DEF_INTERNALBORDER:
			screen->border = atoi (strind);
			continue;
		 case DEF_JUMPSCROLL:
			if (strcmp (strind, "on") == 0) {
				screen->jumpscroll = TRUE;
				term.flags &= ~SMOOTHSCROLL;
			}
			continue;
#ifdef KEYBD
		 case DEF_KEYBOARD:
			if(keyboardtype)
				free(keyboardtype);
			keyboardtype = strind;
			continue;
#endif KEYBD
		 case DEF_LOGFILE:
			if(screen->logfile = malloc(strlen(strind) + 1))
				strcpy(screen->logfile, strind);
			continue;
		 case DEF_LOGGING:
			if (strcmp (strind, "on") == 0) 
				log_on = TRUE;
			continue;
		 case DEF_LOGINHIBIT:
			if (strcmp (strind, "on") == 0) 
				inhibit |= I_LOG;
			continue;
		 case DEF_LOGINSHELL:
			if (strcmp (strind, "on") == 0) 
				login_shell = TRUE;
			continue;
		 case DEF_MARGINBELL:
			if (strcmp (strind, "on") == 0) 
				screen->marginbell = TRUE;
			continue;
		 case DEF_MOUSE:
			mous_color = strind;
			continue;
		 case DEF_NMARGINBELL:
			n_marginbell = atoi (strind);
			continue;
		 case DEF_PAGEOVERLAP:
			if((screen->pageoverlap = atoi (strind) - 1) < 0)
				screen->pageoverlap = -1;
			continue;
		 case DEF_PAGESCROLL:
			if (strcmp (strind, "on") == 0)
				screen->pagemode = TRUE;
			continue;
		 case DEF_REVERSEVIDEO:
			if (strcmp (strind, "on") == 0)
				re_verse = TRUE;
			continue;
		 case DEF_REVERSEWRAP:
			if (strcmp (strind, "on") == 0) 
				term.flags |= REVERSEWRAP;
			continue;
		 case DEF_SAVELINES:
			save_lines = atoi (strind);
			continue;
		 case DEF_SCROLLBAR:
			if (strcmp (strind, "on") == 0) 
				screen->scrollbar = SCROLLBARWIDTH;
			continue;
		 case DEF_SCROLLINPUT:
			if (strcmp (strind, "on") == 0) 
				screen->scrollinput = TRUE;
			continue;
		 case DEF_SCROLLKEY:
			if (strcmp (strind, "on") == 0) 
				screen->scrollkey = TRUE;
			continue;
		 case DEF_SIGNALINHIBIT:
			if (strcmp (strind, "on") == 0) 
				inhibit |= I_SIGNAL;
			continue;
		 case DEF_STATUSLINE:
			if (strcmp (strind, "on") == 0) 
				screen->statusline = TRUE;
			continue;
		 case DEF_STATUSNORMAL:
			screen->reversestatus = (strcmp (strind, "on") != 0);
			continue;
		 case DEF_TEKICONBITMAP:
			tekiconbitmap = strind;
			continue;
		 case DEF_TEKINHIBIT:
			if (strcmp (strind, "on") == 0) 
				inhibit |= I_TEK;
			continue;
		 case DEF_TEXTUNDERICON:
			if (strcmp (strind, "on") == 0) 
				screen->textundericon = TRUE;
			continue;
		 case DEF_TITLEBAR:
			if (strcmp (strind, "on") == 0) 
			    screen->fullVwin.titlebar = TRUE;
			continue;
		 case DEF_TITLEFONT:
			f_t = strind;
			continue;
		 case DEF_VISUALBELL:
			if (strcmp (strind, "on") == 0) 
				screen->visualbell = TRUE;
			continue;
		 case DEF_VISBELLDELAY:
			screen->visbelldelay = atoi(strind);
			continue;
		 case DEF_AUDIBLEBELL:
			if (strcmp (strind, "off") == 0) 
				screen->audiblebell = FALSE;
			continue;
		 case DEF_DROPMENU:
			if (strcmp (strind, "on") == 0) 
				dropmenu = TRUE;
			continue;
		 case DEF_UNIQUESUFFIX:
			if (strcmp (strind, "off") == 0) 
				douniquesuffix = FALSE;
			continue;
#ifdef ALLOWUNSHIFTEDSELECTION
		 case DEF_UNSHIFTEDSELECTION:
			if (strcmp (strind, "on") == 0) 
				UnshiftedSelectionInit();
			continue;
#endif

		}
	}

	/* parse command line */
	
	for (argc--, argv++ ; argc > 0 ; argc--, argv++) {
	    if (**argv == '=') {
		geo_metry = *argv;
		continue;
	    }

	    if (**argv == '%') {
		T_geometry = *argv;
		*T_geometry = '=';
		continue;
	    }

	    if (**argv == '#') {
		icon_geom = *argv;
		*icon_geom = '=';
		continue;
	    }

	    if((strind = index (*argv, ':')) != NULL) {
		strncpy(display, *argv, sizeof(display));
		continue;
	    }

	    if(!(i = (**argv == '-')) && **argv != '+') Syntax ();

	    switch(argument(&(*argv)[1])) {
	     case ARG_132:
		screen->c132 = i;
		continue;
#ifdef TIOCCONS
	     case ARG__C:
		Console = i;
		continue;
#endif TIOCCONS
	     case ARG__L:
		{
		char tt[32];

		L_flag = 1;
		get_ty = argv[--argc];
		strcpy(tt,"/dev/");
		strcat(tt, get_ty);
		tt[5] = 'p';
		loginpty = open( tt, O_RDWR, 0 );
		dup2( loginpty, 4 );
		close( loginpty );
		loginpty = 4;
		tt[5] = 't';
		chown(tt, 0, 0);
		chmod(tt, 0622);
		if (open(tt, O_RDWR) < 0) {
			consolepr("open failed\n");
		}
		signal(SIGHUP, SIG_IGN);
		vhangup();
		setpgrp(0,0);
		signal(SIGHUP, SIG_DFL);
		(void) close(0);
		open(tt, O_RDWR, 0);
		dup2(0, 1);
		dup2(0, 2);
		continue;
		}
	     case ARG__S:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    sscanf(*++argv, "%c%c%d", passedPty, passedPty+1,
		     &am_slave);
		    if (am_slave <= 0) Syntax();
		} else
		    am_slave = 0;
		continue;
	     case ARG_AI:
	        screen->active_icon = i;
		continue;
	     case ARG_AR:
		screen->autoraise = i;
		continue;
	     case ARG_B:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    screen->border = atoi (*++argv);
		} else
		    screen->border = DEFBORDER;
		continue;
	     case ARG_BD:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    brdr_color = *++argv;
		} else
		    brdr_color = NULL;
		continue;
	     case ARG_BG:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    back_color = *++argv;
		} else
		    back_color = NULL;
		continue;
	     case ARG_BW:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    screen->borderwidth = atoi (*++argv);
		} else
		    screen->borderwidth = DEFBORDERWIDTH;
		continue;
	     case ARG_CR:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    curs_color = *++argv;
		} else
		    curs_color = NULL;
		continue;
	     case ARG_CU:
		screen->curses = i;
		continue;
#ifdef DEBUG
	     case ARG_D:
		debug = i;
		continue;
#endif DEBUG
	     case ARG_DW:
		screen->deiconwarp = i;
		continue;
	     case ARG_E:
	 	if(!i) Syntax();
		if (argc <= 1) Syntax ();
		command_to_exec = ++argv;
		break;
	     case ARG_FB:
		if(fbflag = i) {
		    if (--argc <= 0) Syntax ();
		    f_b = *++argv;
		    fbflag = TRUE;
		} else {
		    f_b = def_bold_font;
		    fbflag = FALSE;
		}
		continue;
	     case ARG_FG:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    fore_color = *++argv;
		} else
		    fore_color = NULL;
		continue;
	     case ARG_FI:
	        if (i) {
		    if (--argc <= 0) Syntax();
		    f_i = *++argv;
		} else
		    f_i = def_icon_font;
		continue;
	     case ARG_FN:
		if(fnflag = i) {
		    if (--argc <= 0) Syntax ();
		    f_n = *++argv;
		    fnflag = TRUE;
		} else {
		    f_n = def_font;
		    fnflag = FALSE;
		}
		continue;
	     case ARG_FT:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    f_t = *++argv;
		} else
		    f_t = def_title_font;
		continue;
	     case ARG_I:
		screen->icon_show = i ? -1 : 0;
		continue;
	     case ARG_IB:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    iconbitmap = *++argv;
		} else
		    iconbitmap = NULL;
		continue;
	     case ARG_IT:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    tekiconbitmap = *++argv;
		} else
		    tekiconbitmap = NULL;
		continue;
	     case ARG_J:
		if(screen->jumpscroll = i)
			term.flags &= ~SMOOTHSCROLL;
		else
			term.flags |= SMOOTHSCROLL;
		continue;
#ifdef KEYBD
	     case ARG_K:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    keyboardtype = *++argv;
		} else
		    keyboardtype = NULL;
		continue;
#endif KEYBD
	     case ARG_L:
		log_on = i;
		continue;
	     case ARG_LF:
		if(screen->logfile)
			free(screen->logfile);
		if(i) {
		    if (--argc <= 0) Syntax ();
		    if(screen->logfile = malloc(strlen(*++argv) + 1))
			    strcpy(screen->logfile, *argv);
		} else
		    screen->logfile = NULL;
		continue;
	     case ARG_LS:
		login_shell = i;
		continue;
	     case ARG_MB:
		screen->marginbell = i;
		continue;
	     case ARG_MS:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    mous_color = *++argv;
		} else
		    mous_color = NULL;
		continue;
	     case ARG_N:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    win_name = *++argv;
		} else
		    win_name = NULL;
		continue;
	     case ARG_NB:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    n_marginbell = atoi (*++argv);
		} else
		    n_marginbell = N_MARGINBELL;
		continue;
	     case ARG_PO:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    if((screen->pageoverlap = atoi (*++argv) - 1) < 0)
			screen->pageoverlap = -1;
		} else
		    screen->pageoverlap = 0;
		continue;
	     case ARG_PS:
		screen->pagemode = i;
		continue;
	     case ARG_RV:
		re_verse = i;
		continue;
	     case ARG_RW:
		if(i)
		    term.flags |= REVERSEWRAP;
		else
		    term.flags &= ~REVERSEWRAP;
		continue;
	     case ARG_S:
		screen->multiscroll = i;
		continue;
	     case ARG_SB:
		screen->scrollbar = i ? SCROLLBARWIDTH : 0;
		continue;
	     case ARG_SI:
		screen->scrollinput = i;
		continue;
	     case ARG_SK:
		screen->scrollkey = i;
		continue;
	     case ARG_SL:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    save_lines = atoi (*++argv);
		} else
		    save_lines = SAVELINES;
		continue;
	     case ARG_SN:
		screen->reversestatus = !i;
		continue;
	     case ARG_ST:
		screen->statusline = i;
		continue;
	     case ARG_T:
		screen->TekEmu = i;
		continue;
	     case ARG_TB:
		screen->fullVwin.titlebar = i;
		continue;
	     case ARG_TI:
		screen->textundericon = i;
		continue;
	     case ARG_AB:
		screen->audiblebell = i;
		continue;
	     case ARG_VB:
		screen->visualbell = i;
		continue;
	     case ARG_VD:
		if(i) {
		    if (--argc <= 0) Syntax ();
		    screen->visbelldelay = atoi (*++argv);
		} else
		    screen->visbelldelay = VISBELLDELAY;
		continue;
	     default:
		Syntax ();
	    }
	    break;
	}

	term.initflags = term.flags;

	if (fnflag && !fbflag) f_b = NULL;
	if (!fnflag && fbflag) f_n = f_b;
	if(!win_name) {
		if(get_ty) {
			char b[256];

			gethostname(b, sizeof(b) - 1);
			b[sizeof(b) - 1] = 0;
			if(strind = index(b, '.')) /* remove domain */
				*strind = 0;
			win_name = malloc(strlen(b) + 8);
			strcpy(win_name, "login(");
			strcat(win_name, b);
			strcat(win_name, ")");
		} else
			win_name = (am_slave ? "xterm slave" :
			 (command_to_exec ? basename(command_to_exec[0]) :
			 xterm_name));
	}
	if(inhibit & I_TEK)
		screen->TekEmu = FALSE;

	/* set up stderr properly */
	i = -1;
#ifdef DEBUG
	if(debug)
		i = open ("xterm.debug.log", O_WRONLY | O_CREAT | O_TRUNC,
		 0666);
	else
#endif DEBUG
	if(get_ty)
		i = open("/dev/console", O_WRONLY);
	if(i >= 0)
		fileno(stderr) = i;
	if(fileno(stderr) != (NOFILE - 1)) {
		dup2(fileno(stderr), (NOFILE - 1));
		if(fileno(stderr) >= 3)
			close(fileno(stderr));
		fileno(stderr) = (NOFILE - 1);
	}

	signal (SIGCHLD, reapchild);
	signal (SIGHUP, SIG_IGN);
	signal(SIGALRM, onalarm);

	/* open a terminal for client */
	get_terminal ();
	spawn ();

	Xsocket = screen->display->fd;
	pty = screen->respond;

	if (am_slave) { /* Write window id so master end can read and use */
	    write(pty, screen->TekEmu ? (char *)&TWindow(screen) :
	     (char *)&VWindow(screen), sizeof(Window));
	    write(pty, "\n", 1);
	}

	if(log_on) {
		log_on = FALSE;
		StartLog(screen);
	}
	screen->inhibit = inhibit;
	mode = 1;
	if (ioctl (pty, FIONBIO, &mode) == -1) SysError (ERROR_FIONBIO);
	
	pty_mask = 1 << pty;
	X_mask = 1 << Xsocket;
	Select_mask = pty_mask | X_mask;
	max_plus1 = (pty < Xsocket) ? (1 + Xsocket) : (1 + pty);

#ifdef DEBUG
	if (debug) printf ("debugging on\n");
#endif DEBUG
	XErrorHandler(xerror);
	XIOErrorHandler(xioerror);
	for( ; ; )
		if(screen->TekEmu)
			TekRun();
		else
			VTRun();
}

char *basename(name)
char *name;
{
	register char *cp;
	char *rindex();

	return((cp = rindex(name, '/')) ? cp + 1 : name);
}

static struct argstr {
	char *arg;
	int val;
} arg[] = {
	{"132",	ARG_132},
#ifdef TIOCCONS
	{"C",	ARG__C},
#endif TIOCCONS
	{"L",	ARG__L},
	{"S",	ARG__S},
	{"ab",	ARG_AB},
	{"ai",	ARG_AI},
	{"ar",	ARG_AR},
	{"b",	ARG_B},
	{"bd",	ARG_BD},
	{"bg",	ARG_BG},
	{"bw",	ARG_BW},
	{"cr",	ARG_CR},
	{"cu",	ARG_CU},
#ifdef DEBUG
	{"d",	ARG_D},
#endif DEBUG
	{"dw",	ARG_DW},
	{"e",	ARG_E},
	{"fb",	ARG_FB},
	{"fg",	ARG_FG},
	{"fi",	ARG_FI},
	{"fn",	ARG_FN},
	{"ft",	ARG_FT},
	{"i",	ARG_I},
	{"ib",	ARG_IB},
	{"it",	ARG_IT},
	{"j",	ARG_J},
#ifdef KEYBD
	{"k",	ARG_K},
#endif KEYBD
	{"l",	ARG_L},
	{"lf",	ARG_LF},
	{"ls",	ARG_LS},
	{"mb",	ARG_MB},
	{"ms",	ARG_MS},
	{"n",	ARG_N},
	{"nb",	ARG_NB},
	{"po",	ARG_PO},
	{"ps",	ARG_PS},
	{"r",	ARG_RV},
	{"rv",	ARG_RV},
	{"rw",	ARG_RW},
	{"s",	ARG_S},
	{"sb",	ARG_SB},
	{"si",	ARG_SI},
	{"sk",	ARG_SK},
	{"sl",	ARG_SL},
	{"sn",	ARG_SN},
	{"st",	ARG_ST},
	{"t",	ARG_T},
	{"tb",	ARG_TB},
	{"ti",	ARG_TI},
	{"vb",	ARG_VB},
	{"vd",	ARG_VD},
	{"w",	ARG_BW},
};

argument(s)
register char *s;
{
	register int i, low, high, com;

	low = 0;
	high = sizeof(arg) / sizeof(struct argstr) - 1;
	while(low <= high) {/* use binary search, arg in lexigraphic order */
		i = (low + high) / 2;
		if ((com = strcmp(s, arg[i].arg)) == 0)
			return(arg[i].val);
		if(com > 0)
			low = i + 1;
		else
			high = i - 1;
	}
	return(-1);
}

static char *ustring[] = {
"Usage: xterm [-132] [-ab] [-ai] [-ar] [-b margin_width] [-bd border_color] \\\n",
#ifdef ARG__C
" [-bg backgrnd_color] [-bw border_width] [-C] [-cr cursor_color] [-cu] \\\n",
#else ARG__C
" [-bg backgrnd_color] [-bw border_width] [-cr cursor_color] [-cu] \\\n",
#endif ARG__C
" [-dw] [-fb bold_font] [-fg foregrnd_color] [-fi icon_font] [-fn norm_font] \\\n",
" [-ft title_font] [-i] [-ib iconbitmap] [-it tekiconbitmap] [-j] \\\n",
#ifdef ARG_K
" [-k keybd] [-l] [-lf logfile] [-ls] [-mb] [-ms mouse_color] \\\n",
#else ARG_K
" [-l] [-lf logfile] [-ls] [-mb] [-ms mouse_color] \\\n",
#endif ARG_K
" [-n name] [-nb bell_margin] [-po] [-ps] [-rv] [-rw] [-s] \\\n",
" [-sb] [-si] [-sk] [-sl save_lines] [-sn] [-st] [-t] [-tb] \\\n",
" [-ti] [-vb] [-vd visbelldelay] [=[width]x[height][[+-]xoff[[+-]yoff]]] \\\n",
" [%[width]x[height][[+-]xoff[[+-]yoff]]] [#[+-]xoff[[+-]yoff]] \\\n",
" [-e command_to_exec]\n\n",
"Fonts must be of fixed width and of same size;\n",
"If only one font is specified, it will be used for normal and bold text\n",
"The -132 option allows 80 <-> 132 column escape sequences\n",
"The -ab option enables audible bell\n",
"The -ai option turns on miniature (active) icons\n",
"The -ar option turns auto raise window mode on\n",
#ifdef ARG__C
"The -C option forces output to /dev/console to appear in this window\n",
#endif ARG__C
"The -cu option turns a curses bug fix on\n",
"The -dw option warps the mouse on deiconify\n",
"The -i option enables icon startup\n",
"The -j option enables jump scroll\n",
"The -l option enables logging\n",
"The -ls option makes the shell a login shell\n",
"The -mb option turns the margin bell on\n",
"The -ps option turns page scroll on\n",
"The -rv option turns reverse video on\n",
"The -rw option turns reverse wraparound on\n",
"The -s option enables asynchronous scrolling\n",
"The -sb option enables the scrollbar\n",
"The -si option enables re-positioning the scrollbar at the bottom on input\n",
"The -sk option causes the scrollbar to position at the bottom on a key\n",
"The -sn option makes the status line normal video \n",
"The -st option enables the status line\n",
"The -t option starts Tektronix mode\n",
"The -tb option enables the titlebar\n",
"The -ti option places the window name under the icon\n",
"The -vb option enables visual bell\n",
0
};

Syntax ()
{
	register char **us = ustring;

	while (*us) fputs(*us++, stderr);
	exit (1);
}

get_pty (pty, tty)
/*
   opens a pty, storing fildes in pty and tty.
 */
int *pty, *tty;
{
	int devindex, letter = 0;

	while (letter < 4) {
	    ttydev [8] = ptydev [8] = "pqrs" [letter++];
	    devindex = 0;

	    while (devindex < 16) {
		ttydev [9] = ptydev [9] = "0123456789abcdef" [devindex++];
		if ((*pty = open (ptydev, O_RDWR)) < 0)
			continue;
		if ((*tty = open (ttydev, O_RDWR)) < 0) {
			close(*pty);
			continue;
		}
		return;
	    }
	}
	
	fprintf (stderr, "%s: Not enough available pty's\n", xterm_name);
	exit (ERROR_PTYS);
}

#if BSD >= 43
#define TTYGRPNAME	"tty"		/* name of group to own ttys */
#define TTYGID(gid)	tty_gid(gid)	/* gid that owns all ttys */

tty_gid(default_gid)
	int default_gid;
{
	struct group *getgrnam(), *gr;
	int gid = default_gid;

	gr = getgrnam(TTYGRPNAME);
	if (gr != (struct group *) 0)
		gid = gr->gr_gid;

	endgrent();

	return (gid);
}
#endif

get_terminal ()
/* 
 * sets up X and initializes the terminal structure except for term.buf.fildes.
 */
{
	register Screen *screen = &term.screen;
	register int try;
	int on = 1;
	Color cdef;
	char *malloc();
	
	for (try = 10 ; ; ) {
	    if (screen->display = XOpenDisplay(display))
		break;
	    if (!get_ty) {
#ifdef TIOCCONS
		/*
		 * Hack: if console is set, this is probably
		 * the login window from xinit.
		 */
		if (Console)
			continue;
#endif TIOCCONS
		fprintf(stderr, "%s: No such display server %s\n", xterm_name,
		 XDisplayName(display));
		exit(ERROR_NOX);
	    }
	    if (--try <= 0)  {
		fprintf (stderr, "%s: Can't connect to display server %s\n",
		 xterm_name, XDisplayName(display));
		exit (ERROR_NOX2);
	    }	    
	    sleep (5);
	}
	(void) setsockopt(screen->display->fd, SOL_SOCKET, SO_KEEPALIVE,
		&on, sizeof(on));

	if(re_verse) {
		B_Pixel = WhitePixel;
		B_Pixmap = WhitePixmap;
		W_Pixel = BlackPixel;
		W_Pixmap = BlackPixmap;
	} else {
		B_Pixel = BlackPixel;
		B_Pixmap = BlackPixmap;
		W_Pixel = WhitePixel;
		W_Pixmap = WhitePixmap;
	}

	if (brdr_color && DisplayCells() > 2 &&
	 XParseColor(brdr_color, &cdef) && XGetHardwareColor(&cdef)) {
	    if(!(screen->bordertile = XMakeTile(cdef.pixel)))
		Error(ERROR_BORDER);
	} else
	    screen->bordertile = B_Pixmap;
	screen->graybordertile = make_gray();

	screen->foreground = B_Pixel;
	screen->background = W_Pixel;
	screen->cursorcolor = B_Pixel;
	screen->mousecolor = B_Pixel;

	if (DisplayCells() > 2 && (fore_color || back_color ||
	 curs_color)) {
		if (fore_color && XParseColor(fore_color, &cdef) &&
		 XGetHardwareColor(&cdef)) {
			screen->foreground = cdef.pixel;
			screen->color |= C_FOREGROUND;
		}
		if (back_color && XParseColor(back_color, &cdef) &&
		 XGetHardwareColor(&cdef)) {
			screen->background = cdef.pixel;
			screen->color |= C_BACKGROUND;
		}
		if (curs_color && XParseColor(curs_color, &cdef) &&
		 XGetHardwareColor(&cdef)) {
			screen->cursorcolor = cdef.pixel;
			screen->color |= C_CURSOR;
		} else
			screen->cursorcolor = screen->foreground;
	}

	if (mous_color && DisplayCells() > 2 &&
	 XParseColor(mous_color, &cdef) && XGetHardwareColor(&cdef)) {
	    screen->mousecolor = cdef.pixel;
	    screen->color |= C_MOUSE;
	} else
	    screen->mousecolor = screen->cursorcolor;

	if(screen->color & C_BACKGROUND) {
	    if(!(screen->bgndtile = XMakeTile(screen->background)))
		Error(ERROR_BACK);
	} else
		screen->bgndtile = W_Pixmap;
	screen->arrow = make_arrow(screen->mousecolor, screen->background,
	 GXcopy);

	XAutoRepeatOn();
	if((screen->titlefont = XOpenFont(f_t)) == NULL) {
		fprintf(stderr, "%s: Can't get title font %s\n", xterm_name,
		 f_t);
		exit(ERROR_TITLEFONT);
	}
	screen->title_n_size= XQueryWidth("m", screen->titlefont->id);
	screen->titleheight = screen->titlefont->height + 2 * TITLEPAD + 1;
	if(screen->fullVwin.titlebar)
		screen->fullVwin.titlebar =
		    screen->fullTwin.titlebar = screen->titleheight;
	IconInit(screen, iconbitmap, tekiconbitmap);
}

static char *tekterm[] = {
	"tek4015",
	"tek4014",
	"tek4013",
	"tek4010",
	"dumb",
	0
};

static char *vtterm[] = {
	"xterms",
	"xterm",
	"vt102",
	"vt100",
	"ansi",
	"dumb",
	0
};

spawn ()
/* 
 *  Inits pty and tty and forks a login process.
 *  Does not close fd Xsocket.
 *  If getty,  execs getty rather than csh and uses std fd's rather
 *  than opening a pty/tty pair.
 *  If slave, the pty named in passedPty is already open for use
 */
{
	register Screen *screen = &term.screen;
	int Xsocket = screen->display->fd;
	int index1, tty = -1;
	int discipline;
	unsigned lmode;
	struct tchars tc;
	struct ltchars ltc;
	struct sgttyb sg;

	char termcap [1024];
	char newtc [1024];
	char *ptr, *shname;
	int i, no_dev_tty = FALSE;
	char **envnew;		/* new environment */
	char buf[32];
	char *TermName = NULL;
	int ldisc = 0;
#ifdef sun
#ifdef TIOCSSIZE
	struct ttysize ts;
#endif TIOCSSIZE
#else sun
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif TIOCSWINSZ
#endif sun
	struct passwd *pw = NULL;
#ifdef UTMP
	struct utmp utmp;
#endif UTMP
	extern int Exit();
	struct passwd *getpwuid();
	char *getenv();
	char *index (), *rindex (), *strindex ();

	screen->uid = getuid();
	screen->gid = getgid();

#ifdef UTMP
	added_utmp_entry = FALSE;
#endif UTMP
	/* so that TIOCSWINSZ || TIOCSIZE doesn't block */
	signal(SIGTTOU,SIG_IGN);
	if(!(screen->TekEmu ? TekInit() : VTInit()))
		exit(ERROR_INIT);

	if(screen->TekEmu) {
		envnew = tekterm;
		ptr = newtc;
	} else {
		/*
		 * Special case of a 80x24 window, use "xterms"
		 */
		envnew = (screen->max_col == 79 && screen->max_row ==
		 23) ? vtterm : &vtterm[1];
		ptr = termcap;
	}
	while(*envnew) {
		if(tgetent(ptr, *envnew) == 1) {
			TermName = *envnew;
			if(!screen->TekEmu)
			    resize(screen, TermName, termcap, newtc);
			break;
		}
		envnew++;
	}

	if (get_ty) {
		screen->respond = loginpty;
		if((tslot = ttyslot()) <= 0)
			SysError(ERROR_TSLOT);
#ifdef TIOCCONS
		if (Console) {
			int on = 1;
			if (ioctl (tty, TIOCCONS, &on) == -1) {
				perror("xterm: ioctl TIOCCONS");
				Console = 0;
			}
		}
#endif TIOCCONS
	} else if (am_slave) {
		screen->respond = am_slave;
		ptydev[8] = ttydev[8] = passedPty[0];
		ptydev[9] = ttydev[9] = passedPty[1];
		if((tslot = ttyslot()) <= 0)
			SysError(ERROR_TSLOT2);
		setgid (screen->gid);
		setuid (screen->uid);
	} else {
		if ((tty = open ("/dev/tty", O_RDWR, 0)) < 0) {
			if (errno != ENXIO) SysError(ERROR_OPDEVTTY);
			else {
				no_dev_tty = TRUE;
				sg = d_sg;
				tc = d_tc;
				discipline = d_disipline;
				ltc = d_ltc;
				lmode = d_lmode;
			}
		} else {
			/* get a copy of the current terminal's state */

			if(ioctl(tty, TIOCGETP, &sg) == -1)
				SysError (ERROR_TIOCGETP);
			if(ioctl(tty, TIOCGETC, &tc) == -1)
				SysError (ERROR_TIOCGETC);
			if(ioctl(tty, TIOCGETD, &discipline) == -1)
				SysError (ERROR_TIOCGETD);
			if(ioctl(tty, TIOCGLTC, &ltc) == -1)
				SysError (ERROR_TIOCGLTC);
			if(ioctl(tty, TIOCLGET, &lmode) == -1)
				SysError (ERROR_TIOCLGET);
			close (tty);

			/* close all std file descriptors */
			for (index1 = 0; index1 < 3; index1++)
				close (index1);
			if ((tty = open ("/dev/tty", O_RDWR, 0)) < 0)
				SysError (ERROR_OPDEVTTY2);

			if (ioctl (tty, TIOCNOTTY, 0) == -1)
				SysError (ERROR_NOTTY);
			close (tty);
		}

		get_pty (&screen->respond, &tty);

		if (screen->respond != Xsocket + 1) {
			dup2 (screen->respond, Xsocket + 1);
			close (screen->respond);
			screen->respond = Xsocket + 1;
		}

#if BSD >= 43
		/* change ownership of tty to real user id and tty gid */
		chown (ttydev, screen->uid, TTYGID(screen->gid));

		/* change protection of tty */
		chmod (ttydev, 0620);
#else
		/* change ownership of tty to real group and user id */
		chown (ttydev, screen->uid, screen->gid);

		/* change protection of tty */
		chmod (ttydev, 0622);
#endif

		if (tty != Xsocket + 2)	{
			dup2 (tty, Xsocket + 2);
			close (tty);
			tty = Xsocket + 2;
		}

		/* set the new terminal's state to be the old one's 
		   with minor modifications for efficiency */

		sg.sg_flags &= ~(ALLDELAY | XTABS | CBREAK | RAW);
		sg.sg_flags |= ECHO | CRMOD;
		/* make sure speed is set on pty so that editors work right*/
		sg.sg_ispeed = B9600;
		sg.sg_ospeed = B9600;
		/* reset t_brkc to default value */
		tc.t_brkc = -1;

		if (ioctl (tty, TIOCSETP, &sg) == -1)
			SysError (ERROR_TIOCSETP);
		if (ioctl (tty, TIOCSETC, &tc) == -1)
			SysError (ERROR_TIOCSETC);
		if (ioctl (tty, TIOCSETD, &discipline) == -1)
			SysError (ERROR_TIOCSETD);
		if (ioctl (tty, TIOCSLTC, &ltc) == -1)
			SysError (ERROR_TIOCSLTC);
		if (ioctl (tty, TIOCLSET, &lmode) == -1)
			SysError (ERROR_TIOCLSET);
#ifdef TIOCCONS
		if (Console) {
			int on = 1;
			if (ioctl (tty, TIOCCONS, &on) == -1) {
				perror("xterm: ioctl TIOCCONS");
				Console = 0;
			}
		}
#endif TIOCCONS

		close (open ("/dev/null", O_RDWR, 0));

		for (index1 = 0; index1 < 3; index1++)
			dup2 (tty, index1);
		if((tslot = ttyslot()) <= 0)
			SysError(ERROR_TSLOT3);
#ifdef UTMP
		if(login_shell && (pw = getpwuid(screen->uid)) &&
		 (i = open(etc_utmp, O_WRONLY)) >= 0) {
			bzero((char *)&utmp, sizeof(struct utmp));
			(void) strcpy(utmp.ut_line, &ttydev[5]);
			(void) strcpy(utmp.ut_name, pw->pw_name);
			if (strncmp(DisplayName(), "unix:", 5) != 0)
				(void) strcpy(utmp.ut_host, DisplayName());
			time(&utmp.ut_time);
			lseek(i, (long)(tslot * sizeof(struct utmp)), 0);
			write(i, (char *)&utmp, sizeof(struct utmp));
			close(i);
			added_utmp_entry = TRUE;
		}
#endif UTMP
	}

#ifdef sun
#ifdef TIOCSSIZE
	/* tell tty how big window is */
	if(screen->TekEmu) {
		ts.ts_lines = 38;
		ts.ts_cols = 81;
	} else {
		ts.ts_lines = screen->max_row + 1;
		ts.ts_cols = screen->max_col + 1;
	}
	ioctl (screen->respond, TIOCSSIZE, &ts);
#endif TIOCSSIZE
#else sun
#ifdef TIOCSWINSZ
	/* tell tty how big window is */
	if(screen->TekEmu) {
		ws.ws_row = 38;
		ws.ws_col = 81;
		ws.ws_xpixel = TFullWidth(screen);
		ws.ws_ypixel = TFullHeight(screen);
	} else {
		ws.ws_row = screen->max_row + 1;
		ws.ws_col = screen->max_col + 1;
		ws.ws_xpixel = FullWidth(screen);
		ws.ws_ypixel = FullHeight(screen);
	}
	ioctl (screen->respond, TIOCSWINSZ, &ws);
#endif TIOCSWINSZ
#endif sun

	if (!am_slave) {
	    if ((screen->pid = fork ()) == -1)
		SysError (ERROR_FORK);
		
	    if (screen->pid == 0) {
		extern char **environ;
		int pgrp = getpid();
		char shell_name[64];

		close (Xsocket);
		close (screen->respond);
		if(fileno(stderr) >= 3)
			close (fileno(stderr));

		if (tty >= 0) close (tty);

		signal (SIGCHLD, SIG_DFL);
		signal (SIGHUP, SIG_IGN);

		/* copy the environment before Setenving */
		for (i = 0 ; environ [i] != NULL ; i++) ;
		/*
		 * The `4' is the number of Setenv() calls which may add
		 * a new entry to the environment.  The `1' is for the
		 * NULL terminating entry.
		 */
		envnew = (char **) calloc (i + (4 + 1), sizeof(char *));
		bcopy((char *)environ, (char *)envnew, i * sizeof(char *));
		environ = envnew;
		Setenv ("TERM=", TermName);
		if(!TermName)
			*newtc = 0;
		Setenv ("TERMCAP=", newtc);
		sprintf(buf, "%d", screen->TekEmu ? (int)TWindow(screen) :
		 (int)VWindow(screen));
		Setenv ("WINDOWID=", buf);
		/* put the display into the environment of the shell*/
		if (display[0] != '\0') 
			Setenv ("DISPLAY=", screen->display->displayname);

		signal(SIGTERM, SIG_DFL);
		ioctl(0, TIOCSPGRP, &pgrp);
		setpgrp (0, 0);
		close(open(ttyname(0), O_WRONLY, 0));
		setpgrp (0, pgrp);

		setgid (screen->gid);
		setuid (screen->uid);

		if (command_to_exec) {
			execvp(*command_to_exec, command_to_exec);
			/* print error message on screen */
			fprintf(stderr, "%s: Can't execvp %s\n", xterm_name,
			 *command_to_exec);
		}
		signal(SIGHUP, SIG_IGN);
		if (get_ty) {
			ioctl (0, TIOCNOTTY, 0);
			execl ("/etc/getty", "+", "Xwindow", get_ty, 0);
		}
		signal(SIGHUP, SIG_DFL);

#ifdef UTMP
		if(((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		 ((pw == NULL && (pw = getpwuid(screen->uid)) == NULL) ||
		 *(ptr = pw->pw_shell) == 0))
#else UTMP
		if(((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		 ((pw = getpwuid(screen->uid)) == NULL ||
		 *(ptr = pw->pw_shell) == 0))
#endif UTMP
			ptr = "/bin/sh";
		if(shname = rindex(ptr, '/'))
			shname++;
		else
			shname = ptr;
		i = strlen(shname) - 3;
		ldisc = (strcmp("csh", shname + i) == 0 ||
		 strcmp("ksh", shname + i) == 0) ? NTTYDISC : 0;
		ioctl(0, TIOCSETD, &ldisc);
		if (login_shell) {
			strcpy(shell_name, "-");
			strcat(shell_name, shname);
		} else
			strcpy(shell_name, shname);
		execl (ptr, shell_name, 0);
		fprintf (stderr, "%s: Could not exec %s!\n", xterm_name, ptr);
		sleep(5);
		exit(ERROR_EXEC);
	    }
	}

	if(tty >= 0) close (tty);
	signal(SIGHUP,SIG_IGN);

	if (!no_dev_tty) {
		if ((tty = open ("/dev/tty", O_RDWR, 0)) < 0)
			SysError(ERROR_OPDEVTTY3);
		for (index1 = 0; index1 < 3; index1++)
			dup2 (tty, index1);
		if (tty > 2) close (tty);
	}

	signal(SIGINT, Exit);
	signal(SIGQUIT, Exit);
	signal(SIGTERM, Exit);
}

Exit(n)
int n;
{
	register Screen *screen = &term.screen;
#ifdef UTMP
	register int i;
	struct utmp utmp;

	if(added_utmp_entry && (i = open(etc_utmp, O_WRONLY)) >= 0) {
		bzero((char *)&utmp, sizeof(struct utmp));
		lseek(i, (long)(tslot * sizeof(struct utmp)), 0);
		write(i, (char *)&utmp, sizeof(struct utmp));
		close(i);
	}
#endif UTMP
	if(screen->logging)
		CloseLog(screen);

	if(!get_ty && !am_slave) {
		/* restore ownership of tty */
		chown (ttydev, 0, 0);

		/* restore modes of tty */
		chmod (ttydev, 0666);
	}
	(void) close(screen->respond);
	exit(n);
}

resize(screen, TermName, oldtc, newtc)
Screen *screen;
char *TermName;
register char *oldtc, *newtc;
{
	register char *ptr1, *ptr2;
	register int i;
	register int li_first = 0;
	register char *temp;
	char *index(), *strindex();

	if ((ptr1 = strindex (oldtc, "co#")) == NULL){
		fprintf(stderr, "%s: Can't find co# in termcap string %s\n",
			xterm_name, TermName);
		exit (ERROR_NOCO);
	}
	if ((ptr2 = strindex (oldtc, "li#")) == NULL){
		fprintf(stderr, "%s: Can't find li# in termcap string %s\n",
			xterm_name, TermName);
		exit (ERROR_NOLI);
	}
	if(ptr1 > ptr2) {
		li_first++;
		temp = ptr1;
		ptr1 = ptr2;
		ptr2 = temp;
	}
	ptr1 += 3;
	ptr2 += 3;
	strncpy (newtc, oldtc, i = ptr1 - oldtc);
	newtc += i;
	sprintf (newtc, "%d", li_first ? screen->max_row + 1 :
	 screen->max_col + 1);
	newtc += strlen(newtc);
	ptr1 = index (ptr1, ':');
	strncpy (newtc, ptr1, i = ptr2 - ptr1);
	newtc += i;
	sprintf (newtc, "%d", li_first ? screen->max_col + 1 :
	 screen->max_row + 1);
	ptr2 = index (ptr2, ':');
	strcat (newtc, ptr2);
}

static reapchild ()
{
	union wait status;
	register int pid;
	
#ifdef DEBUG
	if (debug) fputs ("Exiting\n", stderr);
#endif DEBUG
	pid  = wait3 (&status, WNOHANG, NULL);
	if (!pid) return;
	if (pid != term.screen.pid) return;
	
	Cleanup(0);
}

consolepr(string)
char *string;
{
	extern int errno;
	extern char *sys_errlist[];
	int oerrno;
	int f;

	oerrno = errno;
	f = open("/dev/console",O_WRONLY);
	write(f, "xterm: ", 7);
	write(f, string, strlen(string));
	write(f, ": ", 2);
	write(f, sys_errlist[oerrno],strlen(sys_errlist[oerrno]));
	write(f, "\n", 1);
	close(f);
	if ((f = open("/dev/tty", 2)) >= 0) {
		ioctl(f, TIOCNOTTY, 0);
		close(f);
	}
}

checklogin()
{
	register int i, j;
	register struct passwd *pw;
	struct utmp utmp;

	if((i = open(etc_utmp, O_RDONLY)) < 0)
		return(FALSE);
	lseek(i, (long)(tslot * sizeof(struct utmp)), 0);
	j = read(i, (char *)&utmp, sizeof(utmp));
	close(i);
	if(j != sizeof(utmp) || strcmp(get_ty, utmp.ut_line) != 0 ||
	 !*utmp.ut_name || (pw = getpwnam(utmp.ut_name)) == NULL)
		return(FALSE);
	chdir(pw->pw_dir);
	setgid(pw->pw_gid);
	setuid(pw->pw_uid);
	L_flag = 0;
	return(TRUE);
}
