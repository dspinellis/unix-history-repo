#include "vmodem.h"
#include ssdef
#include tt2def
#include ttdef
#define SS_NORMAL SS$_NORMAL

/*  VMS structures  */
/*
 *	TT_INFO structures are used for passing information about
 *	the terminal.  Used in GTTY and STTY calls.
 */
struct	tt_info	ttys, ttysnew, ttystemp;

/*
 *
 */

/*
 * return 1 iff stdout and stderr are different devices
 *  indicating this program operating with a modem on a
 *  different line
 */
int Fromcu;		/* Were called from cu or yam */
from_cu()
{
}
cucheck()
{
}



/*
 * mode(n)
 *  3: save old tty stat, set raw mode with flow control
 *  2: set XON/XOFF for sb/sz with ZMODEM or YMODEM-g
 *  1: save old tty stat, set raw mode 
 *  0: restore original tty mode
 */
mode(n)
{
	int	*iptr, parameters;
	static savedmodes = FALSE;

	vfile("mode:%d", n);

	if (!savedmodes) {
		if (gtty(&ttys) != SS$_NORMAL)
			death("SETMODES:  error return from GTTY (1)");
		if (gtty(&ttysnew) != SS$_NORMAL)
			death("SETMODES:  error return from GTTY (2)");
		savedmodes = TRUE;
	}

	/*
	 * Set new terminal parameters.
	 *  Note:  there are three bytes of terminal characteristics,
	 *  so we should make sure the fourth byte of the integer is unchanged.
	 */
	switch (n) {
	case 1:
	case 2:
	case 3:
		iptr	= &(ttysnew.dev_characteristics.bcharacteristics);
		parameters	= *iptr;

		parameters	&= ~TT$M_ESCAPE;	/*  ESCAPE   OFF  */
		parameters	&= ~TT$M_HOSTSYNC;	/*  HOSTSYNC OFF  */
		parameters	|=  TT$M_NOECHO;	/*  NOECHO   ON   */
		parameters	|=  TT$M_PASSALL;	/*  PASSALL  ON   */
		parameters	&= ~TT$M_READSYNC;	/*  READSYNC OFF  */
		parameters	&= ~TT$M_TTSYNC;	/*  TTSYNC   OFF  */
		parameters	&= ~TT$M_WRAP;		/*  WRAP     OFF  */
		parameters	|= TT$M_EIGHTBIT;	/*  EIGHTBIT ON   */
		if (n == 3) {
			parameters |= TT$M_HOSTSYNC;	/*  HOSTSYNC On  */
		}
		if (n == 2) {
			parameters |= TT$M_TTSYNC;	/*  TTSYNC On  */
		}

		*iptr		= parameters;

		if (stty(&ttysnew) != SS_NORMAL)
			fatal("SETMODES:  error return from STTY");
		break;
	case 0:
		stty(&ttys);		/*  Restore original modes  */
					/* error return to /dev/null */
	break;
	}
}



/* set tty modes for vrzsz transfers */
setmodes()
{
/*  Device characteristics for VMS  */
}

fatal(msg)
char *msg;
{
	mode(0);  		/* put back normal tty modes */
	printf("vrzsz:  %s\n", msg);
	exit(SS_NORMAL);
}

/* Call this instead if funny modes haven't been set yet */
death(msg)
char *msg;
{
	printf("vrzsz:  %s\n", msg);
	exit(SS_NORMAL);
}

#define LSIZE 64	/* Size of send & receive buffers */
#ifdef BUFREAD

char Rxlbuf[LSIZE+1];
int Rxleft=0;		/* number of characters in Rxlbuf */
char *Rxcdq = Rxlbuf;	/* pointer for removing chars from Rxlbuf */

/*
 * This version of readline is reasoably well suited for
 * reading many characters.
 *
 * timeout is in tenths of seconds
 */

readline(timeout)
int timeout;
{
	register int c;
	extern errno;

	if (--Rxleft>=0)
		return (*Rxcdq++ & 0377);
#ifdef DEBUGG
	eprintf("Calling read: ");
#endif
	if ((c = timeout/10)<2)
		c=2;

	do {
		Rxleft = raw_read(LSIZE, Rxcdq=Rxlbuf, 1);
	} while (Rxleft == SS$_TIMEOUT   &&   --c >= 0);
#ifdef DEBUGG
	eprintf("Read returned %d bytes\n", Rxleft);
#endif
	if (Rxleft == SS$_TIMEOUT || --Rxleft < 0) {
		Rxleft = 0;
		return TIMEOUT;
	}
	return (*Rxcdq++ & 0377);
}

purgeline()
{
	Rxleft=0;
}


#else	/* BUFREAD */


/* get a byte from data stream -- timeout if "dseconds" elapses */
/*	NOTE, however, that this function returns an INT, not a BYTE!!!  */
readline(dseconds)
{
	int seconds;
	int ret, c;

	seconds = dseconds/10;
	if (seconds < 2)
		seconds = 2;
	ret	= raw_read(1, &c, seconds);

	if (ret == SS$_TIMEOUT)
		return(TIMEOUT);

	return(c & 0377);  /* return the char */
}

purgeline()
{
	int c;

	do {
		c = readline(1);
	} while (c != TIMEOUT);
}
#endif


#ifdef BUFWRITE
char Txlbuf[LSIZE+1];
int Txleft=LSIZE;		/* number of characters in Txlbuf */
char *Txcq = Txlbuf;	/* pointer for removing chars from Rxlbuf */

sendline(c)
{
	if (--Txleft >= 0)
		*Txcq++ = c;
	else {
		Txleft = 0;
		flushmoc();
		--Txleft;
		*Txcq++ = c;
	}
}

flushmoc()
{
	register int n;

	n = LSIZE - Txleft;
	Txcq=Txlbuf;  Txleft = LSIZE;
	raw_wbuf(n, Txlbuf);
}

/*
 *  Wait for the modem line outbuffer to drain
 */
flushmo()
{
	fflush(stdout);
	flushmoc();
}

#else	/* BUFWRITE */

/* send a byte to data stream */
sendline(data)
{
	char	dataout;

	dataout	= data;
	raw_write(dataout);

}

flushmo() {}
flushmoc() {}
#endif

sendbrk()
{
}


/* End of vrzsz.c */
