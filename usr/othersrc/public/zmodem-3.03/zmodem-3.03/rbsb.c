/*
 *
 *  Rev 5-09-89
 *  This file contains Unix specific code for setting terminal modes,
 *  very little is specific to ZMODEM or YMODEM per se (that code is in
 *  sz.c and rz.c).  The CRC-16 routines used by XMODEM, YMODEM, and ZMODEM
 *  are also in this file, a fast table driven macro version
 *
 *	V7/BSD HACKERS:  SEE NOTES UNDER mode(2) !!!
 *
 *   This file is #included so the main file can set parameters such as HOWMANY.
 *   See the main files (rz.c/sz.c) for compile instructions.
 */

#ifdef V7
#include <sys/types.h>
#include <sys/stat.h>
#define STAT
#include <sgtty.h>
#define OS "V7/BSD"
#define ROPMODE "r"
#ifdef LLITOUT
long Locmode;		/* Saved "local mode" for 4.x BSD "new driver" */
long Locbit = LLITOUT;	/* Bit SUPPOSED to disable output translations */
#include <strings.h>
#endif
#endif

#ifndef OS
#ifndef USG
#define USG
#endif
#endif

#ifdef USG
#include <sys/types.h>
#include <sys/stat.h>
#define STAT
#include <termio.h>
#define OS "SYS III/V"
#define ROPMODE "r"
#define MODE2OK
#include <string.h>
#endif

#if HOWMANY  > 255
Howmany must be 255 or less
#endif

/*
 * return 1 iff stdout and stderr are different devices
 *  indicating this program operating with a modem on a
 *  different line
 */
int Fromcu;		/* Were called from cu or yam */
from_cu()
{
	struct stat a, b;

	fstat(1, &a); fstat(2, &b);
	Fromcu = a.st_rdev != b.st_rdev;
	return;
}
cucheck()
{
	if (Fromcu)
		fprintf(stderr,"Please read the manual page BUGS chapter!\r\n");
}


struct {
	unsigned baudr;
	int speedcode;
} speeds[] = {
	110,	B110,
	300,	B300,
	600,	B600,
	1200,	B1200,
	2400,	B2400,
	4800,	B4800,
	9600,	B9600,
	19200,	EXTA,
	38400,	EXTB,
	0,
};

int Twostop;		/* Use two stop bits */


/*
 *  The following uses an external rdchk() routine if available,
 *  otherwise defines the function for BSD or fakes it for SYSV.
 */

#ifndef READCHECK
#ifdef FIONREAD
#define READCHECK
/*
 *  Return non 0 iff something to read from io descriptor f
 */
rdchk(f)
{
	static long lf;

	ioctl(f, FIONREAD, &lf);
	return ((int) lf);
}

#else		/* FIONREAD */

#ifdef SV
#define READCHECK
#include <fcntl.h>

int checked = 0;
/*
 * Nonblocking I/O is a bit different in System V, Release 2
 *  Note: this rdchk vsn throws away a byte, OK for ZMODEM
 *  sender because protocol design anticipates this problem.
 */
#define EATSIT
rdchk(f)
{
	int lf, savestat;
	static char bchecked;

	savestat = fcntl(f, F_GETFL) ;
	fcntl(f, F_SETFL, savestat | O_NDELAY) ;
	lf = read(f, &bchecked, 1) ;
	fcntl(f, F_SETFL, savestat) ;
	checked = bchecked & 0377;	/* force unsigned byte */
	return(lf) ;
}
#endif
#endif
#endif


static unsigned
getspeed(code)
{
	register n;

	for (n=0; speeds[n].baudr; ++n)
		if (speeds[n].speedcode == code)
			return speeds[n].baudr;
	return 38400;	/* Assume fifo if ioctl failed */
}



#ifdef ICANON
struct termio oldtty, tty;
#else
struct sgttyb oldtty, tty;

struct tchars oldtch, tch;
#endif

SIGTYPE bibi (int);

/*
 * mode(n)
 *  3: save old tty stat, set raw mode with flow control
 *  2: set XON/XOFF for sb/sz with ZMODEM or YMODEM-g
 *  1: save old tty stat, set raw mode 
 *  0: restore original tty mode
 */
mode(n)
{
	static did0 = FALSE;

	vfile("mode:%d", n);
	switch(n) {
#ifdef USG
	case 2:		/* Un-raw mode used by sz, sb when -g detected */
		if(!did0)
			(void) ioctl(0, TCGETA, &oldtty);
		tty = oldtty;

		tty.c_iflag = BRKINT|IXON;

		tty.c_oflag = 0;	/* Transparent output */

		tty.c_cflag &= ~PARENB;	/* Disable parity */
		tty.c_cflag |= CS8;	/* Set character size = 8 */
		if (Twostop)
			tty.c_cflag |= CSTOPB;	/* Set two stop bits */


#ifdef READCHECK
		tty.c_lflag = Zmodem ? 0 : ISIG;
		tty.c_cc[VINTR] = Zmodem ? -1:030;	/* Interrupt char */
#else
		tty.c_lflag = ISIG;
		tty.c_cc[VINTR] = Zmodem ? 03:030;	/* Interrupt char */
#endif
		tty.c_cc[VQUIT] = -1;			/* Quit char */
#ifdef NFGVMIN
		tty.c_cc[VMIN] = 1;
#else
		tty.c_cc[VMIN] = 3;	 /* This many chars satisfies reads */
#endif
		tty.c_cc[VTIME] = 1;	/* or in this many tenths of seconds */

		(void) ioctl(0, TCSETAW, &tty);
		did0 = TRUE;
		return OK;
	case 1:
	case 3:
		if(!did0)
			(void) ioctl(0, TCGETA, &oldtty);
		tty = oldtty;

		tty.c_iflag = n==3 ? (IGNBRK|IXOFF) : IGNBRK;

		 /* No echo, crlf mapping, INTR, QUIT, delays, no erase/kill */
		tty.c_lflag &= ~(ECHO | ICANON | ISIG);

		tty.c_oflag = 0;	/* Transparent output */

		tty.c_cflag &= ~PARENB;	/* Same baud rate, disable parity */
		tty.c_cflag |= CS8;	/* Set character size = 8 */
		if (Twostop)
			tty.c_cflag |= CSTOPB;	/* Set two stop bits */
#ifdef NFGVMIN
		tty.c_cc[VMIN] = 1; /* This many chars satisfies reads */
#else
		tty.c_cc[VMIN] = HOWMANY; /* This many chars satisfies reads */
#endif
		tty.c_cc[VTIME] = 1;	/* or in this many tenths of seconds */
		(void) ioctl(0, TCSETAW, &tty);
		did0 = TRUE;
		Effbaud = Baudrate = getspeed(tty.c_cflag & CBAUD);
		return OK;
#endif
#ifdef V7
	/*
	 *  NOTE: this should transmit all 8 bits and at the same time
	 *   respond to XOFF/XON flow control.  If no FIONREAD or other
	 *   rdchk() alternative, also must respond to INTRRUPT char
	 *   This doesn't work with V7.  It should work with LLITOUT,
	 *   but LLITOUT was broken on the machine I tried it on.
	 */
	case 2:		/* Un-raw mode used by sz, sb when -g detected */
		if(!did0) {
			ioctl(0, TIOCEXCL, 0);
			ioctl(0, TIOCGETP, &oldtty);
			ioctl(0, TIOCGETC, &oldtch);
#ifdef LLITOUT
			ioctl(0, TIOCLGET, &Locmode);
#endif
		}
		tty = oldtty;
		tch = oldtch;
#ifdef READCHECK
		tch.t_intrc = Zmodem ? -1:030;	/* Interrupt char */
#else
		tch.t_intrc = Zmodem ? 03:030;	/* Interrupt char */
#endif
		tty.sg_flags |= (ODDP|EVENP|CBREAK);
		tty.sg_flags &= ~(ALLDELAY|CRMOD|ECHO|LCASE);
		ioctl(0, TIOCSETP, &tty);
		ioctl(0, TIOCSETC, &tch);
#ifdef LLITOUT
		ioctl(0, TIOCLBIS, &Locbit);
#endif
		bibi(99);	/* un-raw doesn't work w/o lit out */
		did0 = TRUE;
		return OK;
	case 1:
	case 3:
		if(!did0) {
			ioctl(0, TIOCEXCL, 0);
			ioctl(0, TIOCGETP, &oldtty);
			ioctl(0, TIOCGETC, &oldtch);
#ifdef LLITOUT
			ioctl(0, TIOCLGET, &Locmode);
#endif
		}
		tty = oldtty;
		tty.sg_flags |= (RAW|TANDEM);
		tty.sg_flags &= ~ECHO;
		ioctl(0, TIOCSETP, &tty);
		did0 = TRUE;
		Effbaud = Baudrate = getspeed(tty.sg_ospeed);
		return OK;
#endif
	case 0:
		if(!did0)
			return ERROR;
#ifdef USG
		(void) ioctl(0, TCSBRK, 1);	/* Wait for output to drain */
		(void) ioctl(0, TCFLSH, 1);	/* Flush input queue */
		(void) ioctl(0, TCSETAW, &oldtty);	/* Restore modes */
		(void) ioctl(0, TCXONC,1);	/* Restart output */
#endif
#ifdef V7
		ioctl(0, TIOCSETP, &oldtty);
		ioctl(0, TIOCSETC, &oldtch);
		ioctl(0, TIOCNXCL, 0);
#ifdef LLITOUT
		ioctl(0, TIOCLSET, &Locmode);
#endif
#endif

		return OK;
	default:
		return ERROR;
	}
}

sendbrk()
{
#ifdef V7
#ifdef TIOCSBRK
#define CANBREAK
	sleep(1);
	ioctl(0, TIOCSBRK, 0);
	sleep(1);
	ioctl(0, TIOCCBRK, 0);
#endif
#endif
#ifdef USG
#define CANBREAK
	ioctl(0, TCSBRK, 0);
#endif
}

/* End of rbsb.c */
