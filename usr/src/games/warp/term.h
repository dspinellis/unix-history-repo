/* $Header: term.h,v 7.0.1.2 86/12/12 17:05:15 lwall Exp $ */

/* $Log:	term.h,v $
 * Revision 7.0.1.2  86/12/12  17:05:15  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.1  86/10/16  10:53:33  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:14:07  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

/* warp will still work without the following, but may get ahead at low speed */
#ifdef TIOCOUTQ		/* chars left in output queue */
#define output_pending() (ioctl(1, TIOCOUTQ, &iocount),iocount)
#endif

/* If some of the following look something like curses calls, it is because
 * warp used to use curses but doesn't now.  Warp was neither as efficient nor
 * as portable with curses, and since the program had to cheat on curses all
 * over the place anyway, we ripped it out.
 */
#define setimage(of,to) (mvaddch(of->posy+1,of->posx*2,of->image=(to)))

#define mvaddch(y,x,ch) move((y),(x),(ch))
/* #define addch(ch) (tmpchr=(ch), write(1,&tmpchr,1), real_x++) */
#define mvaddc(y,x,ch) move((y),(x),(ch))
#define addc(ch) (write(1,&(ch),1), real_x++)
#define addspace() (write(1," ",1), real_x++)
#define mvaddstr(y,x,s) (move((y),(x),0), tmpstr = (s), \
     tmplen = strlen(tmpstr), write(1, tmpstr, tmplen), real_x += tmplen)

EXT int tmplen;
EXT char *tmpstr;
/* EXT char tmpchr; */

/* The following macros are like the pseudo-curses macros above, but do
 * certain amount of controlled output buffering.
 *
 * NOTE: a beg_qwrite()..end_qwrite() sequence must NOT contain a cursor
 * movement (move), because the move() routine uses beg_qwrite()..end_qwrite()
 * itself.
 */

#define beg_qwrite() (maxcmstring = cmbuffer)
#ifdef vax
#define qwrite() asm("movc3 _gfillen,_filler,*_maxcmstring"); maxcmstring += gfillen
#else
#define qwrite() (movc3(gfillen,filler,maxcmstring), maxcmstring += gfillen)
#endif
#define qaddc(ch) (*maxcmstring++ = (ch), real_x++)
#define qaddch(ch) (*maxcmstring++ = (ch), real_x++)
#define qaddspace() (*maxcmstring++ = ' ', real_x++)
#define end_qwrite() (write(1,cmbuffer,maxcmstring-cmbuffer))

/* setting a ??size to infinity forces cursor addressing in that direction */

EXT int CMsize;
EXT int BCsize INIT(1);
EXT int DOsize INIT(1000);
EXT int UPsize INIT(1000);
EXT int NDsize INIT(1000);

EXT int charsperhalfsec;

EXT int real_y INIT(-100);
EXT int real_x INIT(-100);

#ifdef DOINIT
char filler[] = {0,'\b',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
#else
EXT char filler[];
#endif

EXT char *bsptr INIT(filler+1);

EXT char term[12];

EXT char gfillen INIT(25);

EXT char *maxcmstring;
EXT char cmbuffer[512];

#define BREAKCH '\0'

EXT char INTRCH INIT('\03');

#ifdef PUSHBACK
    EXT char circlebuf[PUSHSIZE];
    EXT int nextin INIT(0);
    EXT int nextout INIT(0);
#   ifdef PENDING
#	ifdef FIONREAD
	    EXT long iocount INIT(0);
#	    ifndef lint
#		define input_pending() (nextin!=nextout || \
(ioctl(0, FIONREAD, &iocount),(int)iocount))
#	    else
#		define input_pending() bizarre
#	    endif /* lint */
#	else /* FIONREAD */
	    int circfill();
#	    ifdef RDCHK
#		ifndef lint
#		    define input_pending() rdchk(0)
#		else /* lint */
#		    define input_pending() bizarre
#		endif /* lint */
#	    else /* RDCHK */
#		ifndef O_NDELAY	/* assert O_NDELAY */
		    ??? PENDING isn't defined correctly in warp.h
#		endif
		EXT int devtty INIT(0);
#		ifndef lint
#		    define input_pending() (nextin!=nextout || circfill())
#		else
#		    define input_pending() bizarre
#		endif /* lint */
#	    endif /* RDCHK */
#	endif /* FIONREAD */
#   else /* PENDING */
	??? warp won't work without PENDING
#	ifndef lint
#	    define input_pending() (nextin!=nextout)
#	else
#	    define input_pending() bizarre
#	endif /* lint */
#   endif /* PENDING */
#else /* PUSHBACK */
#   ifdef PENDING
#	ifdef FIONREAD /* must have FIONREAD or O_NDELAY for input_pending() */
#	    define read_tty(addr,size) read(0,addr,size)
#	    ifndef lint
#		define input_pending() (ioctl(0, FIONREAD, &iocount), \
(int)iocount)
#	    else
#		define input_pending() bizarre
#	    endif /* lint */
	    EXT long iocount INIT(0);
#	else /* FIONREAD */
#	    ifdef RDCHK		/* actually, they can have rdchk() too */
#	    define read_tty(addr,size) read(0,addr,size)
#		ifndef lint
#		    define input_pending() rdchk(0)
#		else /* lint */
#		    define input_pending() bizarre
#		endif /* lint */
#	    else /* RDCHK */
#		ifndef O_NDELAY	/* assert O_NDELAY */
		    ??? PENDING isn't defined correctly in warp.h
#		endif
		EXT int devtty INIT(0);
		EXT bool is_input INIT(FALSE);
		EXT char pending_ch INIT(0);
#		ifndef lint
#		    define input_pending() (is_input || \
(is_input=read(devtty,&pending_ch,1)))
#		else
#		    define input_pending() bizarre
#		endif /* lint */
#	    endif /* RDCHK */
#	endif /* FIONREAD */
#   else /* PENDING */
	??? warp won't work without PENDING
#	define read_tty(addr,size) read(0,addr,size)
#	define input_pending() (FALSE)
#   endif /* PENDING */
#endif /* PUSHBACK */

/* stuff wanted by terminal mode diddling routines */

#ifdef TERMIO
EXT struct termio _tty, _oldtty;
#else
EXT struct sgttyb _tty;
EXT int _res_flg INIT(0);
#endif

EXT int _tty_ch INIT(2);
EXT bool bizarre INIT(FALSE);			/* do we need to restore terminal? */

/* terminal mode diddling routines */

#ifdef TERMIO
  
#define raw() ((bizarre=1),_tty.c_lflag &=~ISIG,_tty.c_cc[VMIN] = 1,ioctl(_tty_ch,TCSETAF,&_tty))
#define noraw() ((bizarre=1),_tty.c_lflag |= ISIG,_tty.c_cc[VEOF] = CEOF,ioctl(_tty_ch,TCSETAF,&_tty))
#define crmode() ((bizarre=1),_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN] = 1,ioctl(_tty_ch,TCSETAF,&_tty))
#define nocrmode() ((bizarre=1),_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,ioctl(_tty_ch,TCSETAF,&_tty))
#define echo()	 ((bizarre=1),_tty.c_lflag |= ECHO, ioctl(_tty_ch, TCSETAW, &_tty))
#define noecho() ((bizarre=1),_tty.c_lflag &=~ECHO, ioctl(_tty_ch, TCSETAW, &_tty))
#define nl()	 ((bizarre=1),_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
#define nonl()	 ((bizarre=1),_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
#define	savetty() (ioctl(_tty_ch, TCGETA, &_oldtty),ioctl(_tty_ch, TCGETA, &_tty))
#define	resetty() ((bizarre=0),ioctl(_tty_ch, TCSETAF, &_oldtty))
#define unflush_output()

#else

#define raw()	 ((bizarre=1),_tty.sg_flags|=RAW, stty(_tty_ch,&_tty))
#define noraw()	 ((bizarre=1),_tty.sg_flags&=~RAW,stty(_tty_ch,&_tty))
#define crmode() ((bizarre=1),_tty.sg_flags |= CBREAK, stty(_tty_ch,&_tty))
#define nocrmode() ((bizarre=1),_tty.sg_flags &= ~CBREAK,stty(_tty_ch,&_tty))
#define echo()	 ((bizarre=1),_tty.sg_flags |= ECHO, stty(_tty_ch, &_tty))
#define noecho() ((bizarre=1),_tty.sg_flags &= ~ECHO, stty(_tty_ch, &_tty))
#define nl()	 ((bizarre=1),_tty.sg_flags |= CRMOD,stty(_tty_ch, &_tty))
#define nonl()	 ((bizarre=1),_tty.sg_flags &= ~CRMOD, stty(_tty_ch, &_tty))
#define	savetty() (gtty(_tty_ch, &_tty), _res_flg = _tty.sg_flags)
#define	resetty() ((bizarre=0),_tty.sg_flags = _res_flg, stty(_tty_ch, &_tty))
#endif /* TERMIO */

#ifdef TIOCSTI
#ifdef lint
#define forceme(c) ioctl(_tty_ch,TIOCSTI,Null(long*))	/* ghad! */
#else
#define forceme(c) ioctl(_tty_ch,TIOCSTI,c) /* pass character in " " */
#endif /* lint */
#else
#define forceme(c)
#endif

/* termcap stuff */

/*
 * NOTE: if you don't have termlib you'll have to define these strings,
 *    the tputs routine, and the tgoto routine.
 * The tgoto routine simply produces a cursor addressing string for a given
 * x and y.  The 1st argument is a generic string to be interpreted.
 * If you are hardwiring it you might just ignore the 1st argument.
 * The tputs routine interprets any leading number as a padding factor, possibly
 * scaled by the number of lines (2nd argument), puts out the string (1st arg)
 * and the padding using the routine specified as the 3rd argument.
 */

#ifdef HAVETERMLIB
EXT char *BC INIT(Nullch);		/* backspace character */
EXT char *UP INIT(Nullch);		/* move cursor up one line */
EXT char *myUP;
EXT char *ND INIT(Nullch);		/* non-destructive cursor right */
EXT char *myND;
EXT char *DO INIT(Nullch);		/* move cursor down one line */
EXT char *myDO;
EXT char *CR INIT(Nullch);		/* get to left margin, somehow */
EXT char *VB INIT(Nullch);		/* visible bell */
EXT char *CL INIT(Nullch);		/* home and clear screen */
EXT char *CE INIT(Nullch);		/* clear to end of line */
EXT char *CM INIT(Nullch);		/* cursor motion -- PWP */
EXT char *HO INIT(Nullch);		/* home cursor -- PWP */
EXT char *CD INIT(Nullch);		/* clear to end of display -- PWP */
EXT char *SO INIT(Nullch);		/* begin standout mode */
EXT char *SE INIT(Nullch);		/* end standout mode */
EXT int SG INIT(0);		/* blanks left by SO and SE */
EXT char *US INIT(Nullch);		/* start underline mode */
EXT char *UE INIT(Nullch);		/* end underline mode */
EXT char *UC INIT(Nullch);		/* underline a character, if that's how it's done */
EXT int UG INIT(0);		/* blanks left by US and UE */
EXT bool AM INIT(FALSE);		/* does terminal have automatic margins? */
EXT bool XN INIT(FALSE);		/* does it eat 1st newline after automatic wrap? */
EXT char PC INIT(0);		/* pad character for use by tputs() */
EXT short ospeed INIT(0);	/* terminal output speed, for use by tputs() */
EXT int LINES INIT(0), COLS INIT(0);	/* size of screen */
EXT int just_a_sec INIT(960);			/* 1 sec at current baud rate */
					/* (number of nulls) */
EXT char ERASECH;		/* rubout character */
EXT char KILLCH;		/* line delete character */

/* define a few handy macros */

#define clear() (do_tc(CL,LINES),real_y=real_x=0)
#define erase_eol() do_tc(CE,1)
#define backspace() (do_tc(BC,0),real_x--)
#define clear_rest() do_tc(CD,LINES)
#define underline() do_tc(US,1)
#define un_underline() do_tc(UE,1)
#define underchar() do_tc(UC,0)
#define standout() do_tc(SO,1)
#define un_standout() do_tc(SE,1)
#define up_line() do_tc(UP,1)
#define carriage_return() do_tc(CR,1)
#define dingaling() do_tc(VB,1)
#else
  ????????		/* up to you */
#endif

void	term_init();
void	term_set();
#ifdef PUSHBACK
void	pushchar();
void	mac_init();
void	mac_line();
#endif
void	eat_typeahead();
void	settle_down();
#ifndef read_tty
    int		read_tty();
#endif
void	getcmd();

int read_nd();
void page();
void move();
void do_tc();
int comp_tc();
void helper();
void rewrite();
char cmstore();

