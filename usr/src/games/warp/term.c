/* $Header: term.c,v 7.0.1.2 86/12/12 17:04:09 lwall Exp $ */

/* $Log:	term.c,v $
 * Revision 7.0.1.2  86/12/12  17:04:09  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.1  86/10/16  10:53:20  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:14:02  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "bang.h"
#include "intrp.h"
#include "object.h"
#include "play.h"
#include "score.h"
#include "sig.h"
#include "us.h"
#include "util.h"
#include "weapon.h"
#include "INTERN.h"
#include "term.h"

int typeahead = FALSE;

char tcarea[TCSIZE];	/* area for "compiled" termcap strings */

/* guarantee capability pointer != Nullch */
/* (I believe terminfo will ignore the &tmpaddr argument.) */

#define Tgetstr(key) ((tstr = tgetstr(key,&tmpaddr)) ? tstr : nullstr)

#ifdef PUSHBACK
struct keymap {
    char km_type[128];
    union km_union {
	struct keymap *km_km;
	char *km_str;
    } km_ptr[128];
};

#define KM_NOTHIN 0
#define KM_STRING 1
#define KM_KEYMAP 2
#define KM_BOGUS 3

#define KM_TMASK 3
#define KM_GSHIFT 4
#define KM_GMASK 7

typedef struct keymap KEYMAP;

KEYMAP *topmap INIT(Null(KEYMAP*));

void mac_init();
KEYMAP *newkeymap();
void pushstring();
#endif

/* terminal initialization */

void
term_init()
{
    savetty();				/* remember current tty state */

#ifdef TERMIO
    ospeed = _tty.c_cflag & CBAUD;	/* for tputs() */
    ERASECH = _tty.c_cc[VERASE];	/* for finish_command() */
    KILLCH = _tty.c_cc[VKILL];		/* for finish_command() */
#else
    ospeed = _tty.sg_ospeed;		/* for tputs() */
    ERASECH = _tty.sg_erase;		/* for finish_command() */
    KILLCH = _tty.sg_kill;		/* for finish_command() */
#endif

    /* The following could be a table but I can't be sure that there isn't */
    /* some degree of sparsity out there in the world. */

    switch (ospeed) {			/* 1 second of padding */
#ifdef BEXTA
        case BEXTA:  just_a_sec = 1920; break;
#else
#ifdef B19200
        case B19200: just_a_sec = 1920; break;
#endif
#endif
        case B9600:  just_a_sec =  960; break;
        case B4800:  just_a_sec =  480; break;
        case B2400:  just_a_sec =  240; break;
        case B1800:  just_a_sec =  180; break;
        case B1200:  just_a_sec =  120; break;
        case B600:   just_a_sec =   60; break;
	case B300:   just_a_sec =   30; break;
	/* do I really have to type the rest of this??? */
        case B200:   just_a_sec =   20; break;
        case B150:   just_a_sec =   15; break;
        case B134:   just_a_sec =   13; break;
        case B110:   just_a_sec =   11; break;
        case B75:    just_a_sec =    8; break;
        case B50:    just_a_sec =    5; break;
        default:     just_a_sec =  960; break;
					/* if we are running detached I */
    }					/*  don't want to know about it! */
}

/* set terminal characteristics */

void
term_set(tcbuf)
char *tcbuf;		/* temp area for "uncompiled" termcap entry */
{
    char *tmpaddr;			/* must not be register */
    Reg1 char *tstr;
    char *tgetstr();
    char *s;
    int retval;

#ifdef PENDING
#ifndef FIONREAD
#ifndef RDCHK
    /* do no delay reads on something that always gets closed on exit */

    devtty = open("/dev/tty",0);
    if (devtty < 0) {
	printf(cantopen,"/dev/tty");
	finalize(1);
    }
    fcntl(devtty,F_SETFL,O_NDELAY);
#endif
#endif
#endif
    
    /* get all that good termcap stuff */

    retval = tgetent(tcbuf,getenv("TERM"));	/* get termcap entry */
    if (retval < 1) {
#ifdef VERBOSE
	printf("No termcap %s found.\n", retval ? "file" : "entry");
#else
	fputs("Termcap botch\n",stdout);
#endif
	finalize(1);
    }
    tmpaddr = tcarea;			/* set up strange tgetstr pointer */
    s = Tgetstr("pc");			/* get pad character */
    PC = *s;				/* get it where tputs wants it */
    if (!tgetflag("bs")) {		/* is backspace not used? */
	BC = Tgetstr("bc");		/* find out what is */
	if (BC == nullstr) 		/* terminfo grok's 'bs' but not 'bc' */
	    BC = Tgetstr("le");
    } else
	BC = "\b";			/* make a backspace handy */
    UP = Tgetstr("up");			/* move up a line */
    ND = Tgetstr("nd");			/* non-destructive move cursor right */
    DO = Tgetstr("do");			/* move cursor down */
    if (!*DO)
	DO = Tgetstr("nl");
    CL = Tgetstr("cl");			/* get clear string */
    CE = Tgetstr("ce");			/* clear to end of line string */
    CM = Tgetstr("cm");			/* cursor motion - PWP */
    HO = Tgetstr("ho");			/* home cursor if no CM - PWP */
    CD = Tgetstr("cd");			/* clear to end of display - PWP */
    SO = Tgetstr("so");			/* begin standout */
    SE = Tgetstr("se");			/* end standout */
    if ((SG = tgetnum("sg"))<0)
	SG = 0;				/* blanks left by SG, SE */
    US = Tgetstr("us");			/* start underline */
    UE = Tgetstr("ue");			/* end underline */
    if ((UG = tgetnum("ug"))<0)
	UG = 0;				/* blanks left by US, UE */
    if (*US)
	UC = nullstr;			/* UC must not be NULL */
    else
	UC = Tgetstr("uc");		/* underline a character */
    if (!*US && !*UC) {			/* no underline mode? */
	US = SO;			/* substitute standout mode */
	UE = SE;
	UG = SG;
    }
    LINES = tgetnum("li");		/* lines per page */
    COLS = tgetnum("co");		/* columns on page */
    AM = tgetflag("am");		/* terminal wraps automatically? */
    XN = tgetflag("xn");		/* then eats next newline? */
    VB = Tgetstr("vb");
    if (!*VB)
	VB = "\007";
    CR = Tgetstr("cr");
    if (!*CR) {
	if (tgetflag("nc") && *UP) {
	    CR = safemalloc((MEM_SIZE)strlen(UP)+2);
	    Sprintf(CR,"%s\r",UP);
	}
	else
	    CR = "\r";
    }
    if (LINES <= 0)
	LINES = 24;
    if (COLS <= 0)
	COLS = 80;

    BCsize = comp_tc(bsptr,BC,1);
    BC = bsptr;

    if (!*ND)				/* not defined? */
	NDsize = 1000;			/* force cursor addressing */
    else {
	NDsize = comp_tc(cmbuffer,ND,1);
	myND = malloc((unsigned)NDsize);
	movc3(NDsize,cmbuffer,myND);
	if (debugging) {
	    int scr;

	    printf("ND");
	    for (scr=0; scr<NDsize; scr++)
		printf(" %d",myND[scr]);
	    printf("\n");
	}
    }

    if (!*UP)				/* not defined? */
	UPsize = 1000;			/* force cursor addressing */
    else {
	UPsize = comp_tc(cmbuffer,UP,1);
	myUP = malloc((unsigned)UPsize);
	movc3(UPsize,cmbuffer,myUP);
	if (debugging) {
	    int scr;

	    printf("UP");
	    for (scr=0; scr<UPsize; scr++)
		printf(" %d",myUP[scr]);
	    printf("\n");
	}
    }

    if (!*DO) {				/* not defined? */
	myDO = DO = "\n";		/* assume a newline */
	DOsize = 1;
    }
    else {
	DOsize = comp_tc(cmbuffer,DO,1);
	myDO = malloc((unsigned)DOsize);
	movc3(DOsize,cmbuffer,myDO);
	if (debugging) {
	    int scr;

	    printf("DO");
	    for (scr=0; scr<DOsize; scr++)
		printf(" %d",myDO[scr]);
	    printf("\n");
	}
    }
    if (debugging)
	Fgets(cmbuffer,(sizeof cmbuffer),stdin);

    CMsize = comp_tc(cmbuffer,tgoto(CM,20,20),0);
    if (PC != '\0') {
	char *p;

	for (p=filler+(sizeof filler)-1;!*p;--p)
	    *p = PC;
    }
    charsperhalfsec = ospeed >= B9600 ? 480 :
		      ospeed == B4800 ? 240 :
		      ospeed == B2400 ? 120 :
		      ospeed == B1200 ? 60 :
		      ospeed == B600 ? 30 :
	      /* speed is 300 (?) */   15;

    gfillen = ospeed >= B9600 ? (sizeof filler) :
	      ospeed == B4800 ? 13 :
	      ospeed == B2400 ? 7 :
	      ospeed == B1200 ? 4 :
				1+BCsize;
    if (ospeed < B2400)
	lowspeed = TRUE;

    strcpy(term,ttyname(2));

    if (!*CM || !BCsize)
	no_can_do("dumb");
    if (!scorespec && (LINES < 24 || COLS < 80))
	no_can_do("puny");

    crmode();
    raw();
    noecho();				/* turn off echo */
    nonl();

#ifdef PUSHBACK
    mac_init(tcbuf);
#endif
}

#ifdef PUSHBACK
void
mac_init(tcbuf)
char *tcbuf;
{
    char tmpbuf[1024];

    tmpfp = fopen(filexp(getval("WARPMACRO",WARPMACRO)),"r");
    if (tmpfp != Nullfp) {
	while (fgets(tcbuf,1024,tmpfp) != Nullch) {
	    mac_line(tcbuf,tmpbuf,(sizeof tmpbuf));
	}
	Fclose(tmpfp);
    }
}

void
mac_line(line,tmpbuf,tbsize)
char *line;
char *tmpbuf;
int tbsize;
{
    Reg1 char *s;
    Reg2 char *m;
    Reg3 KEYMAP *curmap;
    Reg4 int ch;
    Reg5 int garbage = 0;
    static char override[] = "\r\nkeymap overrides string\r\n";

    if (topmap == Null(KEYMAP*))
	topmap = newkeymap();
    if (*line == '#' || *line == '\n')
	return;
    if (line[ch = strlen(line)-1] == '\n')
	line[ch] = '\0';
    m = dointerp(tmpbuf,tbsize,line," \t");
    if (!*m)
	return;
    while (*m == ' ' || *m == '\t') m++;
    for (s=tmpbuf,curmap=topmap; *s; s++) {
	ch = *s & 0177;
	if (s[1] == '+' && isdigit(s[2])) {
	    s += 2;
	    garbage = (*s & KM_GMASK) << KM_GSHIFT;
	}
	else
	    garbage = 0;
	if (s[1]) {
	    if ((curmap->km_type[ch] & KM_TMASK) == KM_STRING) {
		puts(override);
		free(curmap->km_ptr[ch].km_str);
		curmap->km_ptr[ch].km_str = Nullch;
	    }
	    curmap->km_type[ch] = KM_KEYMAP + garbage;
	    if (curmap->km_ptr[ch].km_km == Null(KEYMAP*))
		curmap->km_ptr[ch].km_km = newkeymap();
	    curmap = curmap->km_ptr[ch].km_km;
	}
	else {
	    if ((curmap->km_type[ch] & KM_TMASK) == KM_KEYMAP)
		puts(override);
	    else {
		curmap->km_type[ch] = KM_STRING + garbage;
		curmap->km_ptr[ch].km_str = savestr(m);
	    }
	}
    }
}

KEYMAP*
newkeymap()
{
    Reg1 int i;
    Reg2 KEYMAP *map;

#ifndef lint
    map = (KEYMAP*)safemalloc(sizeof(KEYMAP));
#else
    map = Null(KEYMAP*);
#endif /* lint */
    for (i=127; i>=0; --i) {
	map->km_ptr[i].km_km = Null(KEYMAP*);
	map->km_type[i] = KM_NOTHIN;
    }
    return map;
}

#endif

/* print out a file, stopping at form feeds */

void
page(filename,num)
char *filename;
bool num;
{
    int linenum = 1;

    tmpfp = fopen(filename,"r");
    if (tmpfp != NULL) {
	while (fgets(spbuf,(sizeof spbuf),tmpfp) != NULL) {
	    if (*spbuf == '\f') {
		printf("[Type anything to continue] ");
		Fflush(stdout);
		getcmd(spbuf);
		printf("\r\n");
		if (*spbuf == INTRCH)
		    finalize(0);
		if (*spbuf == 'q' || *spbuf == 'Q')
		    break;
	    }
	    else {
		if (num)
		    printf("%3d   %s\r",linenum++,spbuf);
		else
		    printf("%s\r",spbuf);
	    }
	}
	Fclose(tmpfp);
    }
}

void
move(y, x, chadd)
int y, x;
int chadd;
{
    Reg1 int ydist;
    Reg2 int xdist;
    Reg3 int i;
    Reg4 char *s;

    ydist = y - real_y;
    xdist = x - real_x;
    i = ydist * (ydist < 0 ? -UPsize : DOsize) +
        xdist * (xdist < 0 ? -BCsize : NDsize);
    beg_qwrite();
    if (i <= CMsize) {
	if (ydist < 0)
	    for (; ydist; ydist++)
		for (i=UPsize,s=myUP; i; i--)
		    qaddch(*s++);
	else
	    for (; ydist; ydist--)
		for (i=DOsize,s=myDO; i; i--)
		    qaddch(*s++);
	if (xdist < 0)
	    for (; xdist; xdist++)
		for (i=BCsize,s=BC; i; i--)
		    qaddch(*s++);
	else
	    for (; xdist; xdist--)
		for (i=NDsize,s=myND; i; i--)
		    qaddch(*s++);
    }
    else {
	tputs(tgoto(CM,x,y),0,cmstore);
    }
    real_y = y;
    real_x = x;
    if (chadd) {
	qaddch(chadd);
    }
    if (maxcmstring != cmbuffer)
	end_qwrite();
}

void
do_tc(s,l)
char *s;
int l;
{
    beg_qwrite();
    tputs(s,l,cmstore);
    end_qwrite();
}

int
comp_tc(dest,s,l)
char *dest;
char *s;
int l;
{
    maxcmstring = dest;
    tputs(s,l,cmstore);
    return(maxcmstring-dest);
}

void
helper()
{
    clear();
    mvaddstr(0,4,"h or 4          left");
    mvaddstr(1,4,"j or 2          down                Use with SHIFT to fire torpedoes.");
    mvaddstr(2,4,"k or 8          up                  Use with CTRL or FUNCT to fire");
    mvaddstr(3,4,"l or 6          right                   phasers or turbolasers.");
    mvaddstr(4,4,"b or 1          down and left       Use preceded by 'a' or 'r' for");
    mvaddstr(5,4,"n or 3          down and right          attractors or repulsors.");
    mvaddstr(6,4,"y or 7          up and left         Use normally for E or B motion.");
    mvaddstr(7,4,"u or 9          up and right");
    mvaddstr(8,4,"");
    mvaddstr(9,4,"del or %        fire photon torpedoes in every (reasonable) direction.");
    mvaddstr(10,4,"s               stop all torpedoes.");
    mvaddstr(11,4,"S or 0          stop the Enterprise when in warp mode.");
    mvaddstr(12,4,"d/D             destruct all torpedoes/current vessel.");
    mvaddstr(13,4,"i/w             switch to Enterprise & put into impulse/warp mode.");
    mvaddstr(14,4,"c/v             switch to Enterprise & make cloaked/visible.");
    mvaddstr(15,4,"p               switch to Base.");
    mvaddstr(16,4,"o               toggle to other vessel (from E to B, or vice versa.)");
    mvaddstr(17,4,"z               zap (suppress) blasts near Enterprise next cycle");
    mvaddstr(18,4,"");
    mvaddstr(19,4,"^R      refresh the screen.              ^Z      suspend the game.");
    mvaddstr(20,4,"q       exit this round (if you haven't typed q within 10 cycles).");
    mvaddstr(21,4,"Q       exit this game.");
    mvaddstr(22,4,"");
    mvaddstr(23,4,"                   [Hit space to continue]");
    Fflush(stdout);
    do {
	getcmd(spbuf);
    } while (*spbuf != ' ');
    rewrite();
    
}

void
rewrite()
{
    Reg1 int x;
    Reg2 int y;
    Reg3 OBJECT *obj;

    clear();
    for (y=0; y<YSIZE; y++) {
	for (x=0; x<XSIZE; x++) {
	    if (numamoebas && amb[y][x] != ' ')
		mvaddc(y+1,x*2,amb[y][x]);
	    if (obj=occupant[y][x]) {
		if (obj->image != ' ')
		    mvaddc(y+1,x*2,obj->image);
	    }
	}
    }
    Sprintf(spbuf,
    "%-4s E: %4d %2d B: %5d %3d Enemies: %-3d Stars: %-3d Stardate%5d.%1d %9ld",
	"   ", 0, 0, 0, 0, 0, 0, timer/10+smarts*100, timer%10, 0L);
    mvaddstr(0,0,spbuf);
    oldeenergy = oldbenergy = oldcurscore =
    oldstatus = oldetorp = oldbtorp = oldstrs = oldenemies = -1;
					/* force everything to fill in */
    if (damage)
	olddamage = 0;
    if (!ent)
	etorp = 0;
    if (!base)
	btorp = 0;
    display_status();
}

char
cmstore(ch)
Reg1 char ch;
{
    *maxcmstring++ = ch;
}

/* discard any characters typed ahead */

void
eat_typeahead()
{
#ifdef PUSHBACK
    if (!typeahead && nextin==nextout)	/* cancel only keyboard stuff */
#else
    if (!typeahead)
#endif
    {
#ifdef PENDING
	while (input_pending())
	    Read_tty(buf,sizeof(buf));
#else /* this is probably v7, with no rdchk() */
	ioctl(_tty_ch,TIOCSETP,&_tty);
#endif
    }
}

void
settle_down()
{
    dingaling();
    Fflush(stdout);
    sleep(1);
#ifdef PUSHBACK
    nextout = nextin;			/* empty circlebuf */
#endif
    eat_typeahead();
}

#ifdef PUSHBACK
/* read a character from the terminal, with multi-character pushback */

int
read_tty(addr,size)
char *addr;
int size;	/* ignored for now */
{
#ifdef lint
    size = size;
#endif
    if (nextout != nextin) {
	*addr = circlebuf[nextout++];
	nextout %= PUSHSIZE;
	return 1;
    }
    else {
	size = read(0,addr,1);
	if (size < 0)
	    sig_catcher(SIGHUP);
	if (metakey) {
	    if (*addr & 0200) {
		pushchar(*addr & 0177);
		*addr = '\001';
	    }
	}
	else
	    *addr &= 0177;
	return 1;
    }
}

#ifdef PENDING
#ifndef FIONREAD
#ifndef RDCHK
int
circfill()
{
    Reg1 int howmany;
    Reg2 int i;

    assert (nextin == nextout);
    howmany = read(devtty,circlebuf+nextin,metakey?1:PUSHSIZE-nextin);
    if (howmany > 0) {
	if (metakey) {
	    if (circlebuf[nextin] & 0200) {
		circlebuf[nextin] &= 0177;
		pushchar('\001');
	    }
	}
	else
	    for (i = howmany+nextin-1; i >= nextin; i--)
		circlebuf[i] &= 0177;
	nextin += howmany;
	nextin %= PUSHSIZE;	/* may end up 1 if metakey */
    }
    return howmany;
}
#endif /* RDCHK */
#endif /* FIONREAD */
#endif /* PENDING */

void
pushchar(ch)
char ch;
{
    nextout--;
    if (nextout < 0)
	nextout = PUSHSIZE - 1;
    if (nextout == nextin) {
	fputs("\r\npushback buffer overflow\r\n",stdout);
	sig_catcher(0);
    }
    circlebuf[nextout] = ch;
}

#else /* PUSHBACK */
#ifndef read_tty
/* read a character from the terminal, with hacks for O_NDELAY reads */

int
read_tty(addr,size)
char *addr;
int size;
{
    if (is_input) {
	*addr = pending_ch;
	is_input = FALSE;
	return 1;
    }
    else {
	size = read(0,addr,size);
	if (size < 0)
	    sig_catcher(SIGHUP);
	if (metakey) {
	    if (*addr & 0200) {
		pending_ch = *addr & 0177;
		is_input = TRUE;
		*addr = '\001';
	    }
	}
	else
	    *addr &= 0177;
	return size;
    }
}
#endif /* read_tty */
#endif /* PUSHBACK */

int
read_nd(buff, siz)
char *buff;
int siz;
{
    if (!input_pending())
	return 0;

    getcmd(buff);
    return 1;
}

/* get a character into a buffer */

void
getcmd(whatbuf)
Reg3 char *whatbuf;
{
#ifdef PUSHBACK
    Reg1 KEYMAP *curmap;
    Reg2 int i;
    bool no_macros; 
    int times = 0;			/* loop detector */
    char scrchar;

tryagain:
    curmap = topmap;
/*    no_macros = (whatbuf != buf && nextin == nextout);  */
    no_macros = FALSE;
#endif
    for (;;) {
	errno = 0;
	if (read_tty(whatbuf,1) < 0 && !errno)
	    errno = EINTR;
#ifdef read_tty
	if (metakey) {
	    if (*whatbuf & 0200) {
		*what_buf &= 037;	/* punt and hope they don't notice */
	    }
	}
	else
	    *whatbuf &= 0177;
#endif /* read_tty */
	if (errno && errno != EINTR) {
	    perror(readerr);
	    sig_catcher(0);
	}
#ifdef PUSHBACK
	if (*whatbuf & 0200 || no_macros) {
	    *whatbuf &= 0177;
	    goto got_canonical;
	}
	if (curmap == Null(KEYMAP*))
	    goto got_canonical;
	for (i = (curmap->km_type[*whatbuf] >> KM_GSHIFT) & KM_GMASK; i; --i){
	    Read_tty(&scrchar,1);
	}
	switch (curmap->km_type[*whatbuf] & KM_TMASK) {
	case KM_NOTHIN:			/* no entry? */
	    if (curmap == topmap)	/* unmapped canonical */
		goto got_canonical;
	    settle_down();
	    goto tryagain;
	case KM_KEYMAP:			/* another keymap? */
	    curmap = curmap->km_ptr[*whatbuf].km_km;
	    assert(curmap != Null(KEYMAP*));
	    break;
	case KM_STRING:			/* a string? */
	    pushstring(curmap->km_ptr[*whatbuf].km_str);
	    if (++times > 20) {		/* loop? */
		fputs("\r\nmacro loop?\r\n",stdout);
		settle_down();
	    }
	    no_macros = FALSE;
	    goto tryagain;
	}
#else
	*whatbuf &= 0177;
	break;
#endif
    }

got_canonical:
#ifndef TERMIO
    if (*whatbuf == '\r')
	*whatbuf = '\n';
#endif
    if (whatbuf == buf)
	whatbuf[1] = FINISHCMD;		/* tell finish_command to work */
}

#ifdef PUSHBACK
void
pushstring(str)
char *str;
{
    Reg1 int i;
    char tmpbuf[PUSHSIZE];
    Reg2 char *s = tmpbuf;

    assert(str != Nullch);
    interp(s,PUSHSIZE,str);
    for (i = strlen(s)-1; i >= 0; --i) {
	s[i] ^= 0200; 
	pushchar(s[i]);
    }
}
#endif
