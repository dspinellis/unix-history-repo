/* $Id: term.h,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

EXT char circlebuf[PUSHSIZE];
EXT int nextin INIT(0);
EXT int nextout INIT(0);
EXT char lastchar;
#ifdef PENDING
# ifdef FIONREAD
EXT long iocount INIT(0);
#  ifndef lint
#define input_pending() (nextin!=nextout || (ioctl(0, FIONREAD, &iocount),(int)iocount))
#  else
#define input_pending() bizarre
#  endif /* lint */
# else /* FIONREAD */
#  ifdef HAS_RDCHK
#define input_pending() (rdchk(0) > 0)		/* boolean only */
#  else /*  HAS_RDCHK */
int circfill();
EXT int devtty INIT(0);
#   ifndef lint
#define input_pending() (nextin!=nextout || circfill())
#   else
#define input_pending() bizarre
#   endif /* lint */
#  endif /* HAS_RDCHK */
# endif /* FIONREAD */
#else /* PENDING */
# ifndef lint
#define input_pending() (nextin!=nextout)
# else
#define input_pending() bizarre
# endif /* lint */
#endif /* PENDING */

/* stuff wanted by terminal mode diddling routines */

#ifdef I_TERMIO
EXT struct termio _tty, _oldtty;
#else
# ifdef I_TERMIOS
EXT struct termios _tty, _oldtty;
# else
EXT struct sgttyb _tty;
EXT int _res_flg INIT(0);
# endif
#endif

EXT int _tty_ch INIT(2);
EXT bool bizarre INIT(FALSE);			/* do we need to restore terminal? */

/* terminal mode diddling routines */

#ifdef I_TERMIO

#define crmode() ((bizarre=1),_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN] = 1,ioctl(_tty_ch,TCSETAF,&_tty))
#define nocrmode() ((bizarre=1),_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,stty(_tty_ch,&_tty))
#define echo()	 ((bizarre=1),_tty.c_lflag |= ECHO, ioctl(_tty_ch, TCSETA, &_tty))
#define noecho() ((bizarre=1),_tty.c_lflag &=~ECHO, ioctl(_tty_ch, TCSETA, &_tty))
#define nl()	 ((bizarre=1),_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
#define nonl()	 ((bizarre=1),_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
#define	savetty() (ioctl(_tty_ch, TCGETA, &_oldtty),ioctl(_tty_ch, TCGETA, &_tty))
#define	resetty() ((bizarre=0),ioctl(_tty_ch, TCSETAF, &_oldtty))
#define unflush_output()

#else /* !I_TERMIO */
# ifdef I_TERMIOS

#define crmode() ((bizarre=1), _tty.c_lflag &= ~ICANON,_tty.c_cc[VMIN]=1,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
#define nocrmode() ((bizarre=1),_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,tcsetattr(_tty_ch, TCSAFLUSH,&_tty))
#define echo()	 ((bizarre=1),_tty.c_lflag |= ECHO, tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
#define noecho() ((bizarre=1),_tty.c_lflag &=~ECHO, tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
#define nl()	 ((bizarre=1),_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
#define nonl()	 ((bizarre=1),_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
#define	savetty() (tcgetattr(_tty_ch, &_oldtty),tcgetattr(_tty_ch, &_tty))
#define	resetty() ((bizarre=0),tcsetattr(_tty_ch, TCSAFLUSH, &_oldtty))
#define unflush_output()

# else /* !I_TERMIOS */

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
#  ifdef LFLUSHO
#   ifndef lint
EXT int lflusho INIT(LFLUSHO);
#   else
EXT long lflusho INIT(LFLUSHO);
#   endif /* lint */
#define unflush_output() (ioctl(_tty_ch,TIOCLBIC,&lflusho))
#  else
#define unflush_output()
#  endif /* LFLUSHO */
# endif /* I_TERMIOS */

#endif /* I_TERMIO */

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
 * NOTE: if you don't have termlib you'll either have to define these strings
 *    and the tputs routine, or you'll have to redefine the macros below
 */

#ifdef HAS_TERMLIB
EXT int GT;				/* hardware tabs */
EXT char *BC INIT(Nullch);		/* backspace character */
EXT char *UP INIT(Nullch);		/* move cursor up one line */
EXT char *CR INIT(Nullch);		/* get to left margin, somehow */
EXT char *VB INIT(Nullch);		/* visible bell */
EXT char *CL INIT(Nullch);		/* home and clear screen */
EXT char *CE INIT(Nullch);		/* clear to end of line */
EXT char *TI INIT(Nullch);		/* initialize terminal */
EXT char *TE INIT(Nullch);		/* reset terminal */
EXT char *CM INIT(Nullch);		/* cursor motion */
EXT char *HO INIT(Nullch);		/* home cursor */
#ifdef CLEAREOL
EXT char *CD INIT(Nullch);		/* clear to end of display */
#endif /* CLEAREOL */
EXT char *SO INIT(Nullch);		/* begin standout mode */
EXT char *SE INIT(Nullch);		/* end standout mode */
EXT int SG INIT(0);			/* blanks left by SO and SE */
EXT char *US INIT(Nullch);		/* start underline mode */
EXT char *UE INIT(Nullch);		/* end underline mode */
EXT char *UC INIT(Nullch);		/* underline a character,
						 if that's how it's done */
EXT int UG INIT(0);			/* blanks left by US and UE */
EXT bool AM INIT(FALSE);		/* does terminal have automatic
								 margins? */
EXT bool XN INIT(FALSE);		/* does it eat 1st newline after
							 automatic wrap? */
EXT char PC INIT(0);			/* pad character for use by tputs() */

#ifdef _POSIX_SOURCE
EXT speed_t outspeed INIT(0);		/* terminal output speed, */
#else
EXT long outspeed INIT(0);		/* 	for use by tputs() */
#endif

EXT int LINES INIT(0), COLS INIT(0);	/* size of screen */
EXT int just_a_sec INIT(960);		/* 1 sec at current baud rate */
					/* (number of nulls) */

/* define a few handy macros */

#define backspace() tputs(BC,0,putchr) FLUSH
#define clear() tputs(CL,LINES,putchr) FLUSH
#define erase_eol() tputs(CE,1,putchr) FLUSH
#ifdef CLEAREOL
#define clear_rest() tputs(CD,LINES,putchr) FLUSH
#define maybe_eol() if(erase_screen&&can_home_clear)tputs(CE,1,putchr) FLUSH
#endif /* CLEAREOL */
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

EXT int page_line INIT(1);	/* line number for paging in
						 print_line (origin 1) */

void	term_init _((void));
void	term_set _((char*));
void	pushchar _((char_int));
void	pushstring _((char*,char_int));
void	mac_init _((char*));
void	mac_line _((char*,char*,int));
void	show_macros _((void));
char	putchr _((char_int));	/* routine for tputs to call */
bool	finish_command _((int));
bool	finish_dblchar _((void));
void	eat_typeahead _((void));
void	save_typeahead _((char*, int));
void	settle_down _((void));
#ifdef HAS_TERMLIB
void	termlib_init _((void));
void	termlib_reset _((void));
#endif
#ifndef read_tty
int	read_tty _((char*,int));
#endif
void	underprint _((char*));
#ifdef NOFIREWORKS
void	no_sofire _((void));
void	no_ulfire _((void));
#endif
void	getcmd _((char*));
int	get_anything _((void));
int	pause_getcmd _((void));
void	in_char _((char*,char_int));
void	in_answer _((char*,char_int));
int	print_lines _((char*,int));
void	page_init _((void));
void	pad _((int));
void	printcmd _((void));
void	rubout _((void));
void	reprint _((void));
void	home_cursor _((void));
void	goto_line _((int,int));
#ifdef SIGWINCH
Signal_t winch_catcher _((int));
#endif /* SIGWINCH */
