/* $Header: term.h,v 4.3.1.2 85/05/13 15:52:05 lwall Exp $
 *
 * $Log:	term.h,v $
 * Revision 4.3.1.2  85/05/13  15:52:05  lwall
 * Declared devtty on TERMIO system.
 * 
 * Revision 4.3.1.1  85/05/10  11:41:24  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:51:36  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef PUSHBACK
EXT char circlebuf[PUSHSIZE];
EXT int nextin INIT(0);
EXT int nextout INIT(0);
#ifdef PENDING
#ifdef FIONREAD
EXT long iocount INIT(0);
#ifndef lint
#define input_pending() (nextin!=nextout || (ioctl(0, FIONREAD, &iocount),(int)iocount))
#else
#define input_pending() bizarre
#endif lint
#else FIONREAD
int circfill();
EXT int devtty INIT(0);
#ifndef lint
#define input_pending() (nextin!=nextout || circfill())
#else
#define input_pending() bizarre
#endif lint
#endif FIONREAD
#else PENDING
#ifndef lint
#define input_pending() (nextin!=nextout)
#else
#define input_pending() bizarre
#endif lint
#endif PENDING
#else PUSHBACK
#ifdef PENDING
#ifdef FIONREAD	/* must have FIONREAD or O_NDELAY for input_pending() */
#define read_tty(addr,size) read(0,addr,size)
#ifndef lint
#define input_pending() (ioctl(0, FIONREAD, &iocount),(int)iocount)
#else
#define input_pending() bizarre
#endif lint
EXT long iocount INIT(0);

#else FIONREAD

EXT int devtty INIT(0);
EXT bool is_input INIT(FALSE);
EXT char pending_ch INIT(0);
#ifndef lint
#define input_pending() (is_input || (is_input=read(devtty,&pending_ch,1)))
#else
#define input_pending() bizarre
#endif lint
#endif FIONREAD
#else PENDING
#define read_tty(addr,size) read(0,addr,size)
#define input_pending() (FALSE)
#endif PENDING
#endif PUSHBACK

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

#define crmode() ((bizarre=1),_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN] = 1,ioctl(_tty_ch,TCSETAF,&_tty))
#define nocrmode() ((bizarre=1),_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,stty(_tty_ch,&_tty))
#define echo()	 ((bizarre=1),_tty.c_lflag |= ECHO, ioctl(_tty_ch, TCSETA, &_tty))
#define noecho() ((bizarre=1),_tty.c_lflag &=~ECHO, ioctl(_tty_ch, TCSETA, &_tty))
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
#ifdef LFLUSHO
#ifndef lint
EXT int lflusho INIT(LFLUSHO);
#else
EXT long lflusho INIT(LFLUSHO);
#endif lint
#define unflush_output() (ioctl(_tty_ch,TIOCLBIC,&lflusho))
#else
#define unflush_output()
#endif LFLUSHO
#endif TERMIO

#ifdef TIOCSTI
#ifdef lint
#define forceme(c) ioctl(_tty_ch,TIOCSTI,Null(long*))	/* ghad! */
#else
#define forceme(c) ioctl(_tty_ch,TIOCSTI,c) /* pass character in " " */
#endif lint
#else
#define forceme(c)
#endif

/* termcap stuff */

/*
 * NOTE: if you don't have termlib you'll either have to define these strings
 *    and the tputs routine, or you'll have to redefine the macros below
 */

#ifdef HAVETERMLIB
EXT char *BC INIT(Nullch);		/* backspace character */
EXT char *UP INIT(Nullch);		/* move cursor up one line */
EXT char *CR INIT(Nullch);		/* get to left margin, somehow */
EXT char *VB INIT(Nullch);		/* visible bell */
EXT char *CL INIT(Nullch);		/* home and clear screen */
EXT char *CE INIT(Nullch);		/* clear to end of line */
#ifdef CLEAREOL
EXT char *CM INIT(Nullch);		/* cursor motion -- PWP */
EXT char *HO INIT(Nullch);		/* home cursor -- PWP */
EXT char *CD INIT(Nullch);		/* clear to end of display -- PWP */
#endif CLEAREOL
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

/* define a few handy macros */

#define backspace() tputs(BC,0,putchr) FLUSH
#define clear() tputs(CL,LINES,putchr) FLUSH
#define erase_eol() tputs(CE,1,putchr) FLUSH
#ifdef CLEAREOL
#define clear_rest() tputs(CD,LINES,putchr) FLUSH	/* PWP */
#define maybe_eol() if(erase_screen&&can_home_clear)tputs(CE,1,putchr) FLUSH
#endif CLEAREOL
#define underline() tputs(US,1,putchr) FLUSH
#define un_underline() tputs(UE,1,putchr) FLUSH
#define underchar() tputs(UC,0,putchr) FLUSH
#define standout() tputs(SO,1,putchr) FLUSH
#define un_standout() tputs(SE,1,putchr) FLUSH
#define up_line() tputs(UP,1,putchr) FLUSH
#define carriage_return() tputs(CR,1,putchr) FLUSH
#define dingaling() tputs(VB,1,putchr) FLUSH
#else
  ????????		/* up to you */
#endif

EXT int page_line INIT(1);	/* line number for paging in print_line (origin 1) */

void	term_init();
void	term_set();
#ifdef PUSHBACK
void	pushchar();
void	mac_init();
void	mac_line();
void	show_macros();
#endif
char	putchr();	/* routine for tputs to call */
bool	finish_command();
void	eat_typeahead();
void	settle_down();
#ifndef read_tty
    int		read_tty();
#endif
void	underprint();
#ifdef NOFIREWORKS
    void	no_sofire();
    void	no_ulfire();
#endif
void	getcmd();
int	get_anything();
void	in_char();
int	print_lines();
void	page_init();
void	pad();
void	printcmd();
void	rubout();
void	reprint();
#ifdef CLEAREOL
void	home_cursor();
#endif CLEAREOL
