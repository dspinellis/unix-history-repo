/*
 *	     DZ-11 driver
 *	     ------------
 *
 *		Written to handle single dz - `carrier'|`ring' support non-existent
 *
 *					Piers Lauder
 *					SYDNEY UNIVERSITY
 *					July 1977
 *
 *		Re-written to handle multiple dz's and `carrier'|`ring'.
 *
 *					Ian Johnstone
 *					UNSW
 *					December 1977
 *					January  1978
 *
 *		General tidy up, new comments etc. Removal of ifdefs
 *		for CARRIER and RING. If you don't want them, tough.
 *
 *					Chris Maltby
 *					Basser October 1979
 */ 


/* ( no messages )
#define	MESSAGES
	/*
	 * Define this if you want parity and framing errors
	 * to be logged (via printf). It can be very wordy.
	 */

#define	INTR_ON_BREAK
	/*
	 * Define this to translate framing errors (breaks)
	 * to CINTR for terminals which lack a DEL key.
	 * Can be nasty if you get lots of line errors.
	 */

/* (not	DEBUG)
#define	DEBUG
	/*
	 * Define to debug info for dialup lines.
	 * costs approximately 128 bytes 
	 */

#include	"../defines.h"
#include	"../param.h"
#ifdef	DEBUG
#ifdef	AUSAML
#include	"../lnode.h"
#endif	AUSAML
#include	"../systm.h"
#endif	DEBUG
#include	"../conf.h"
#include	"../user.h"
#include	"../tty.h"
#include	"../proc.h"


#define	NDZ		2		/* number of DZ-11s */
#define NLINES		8*NDZ		/* total number of lines */
#define	TSCANRATE	2		/* dzscan called every 2 tics */
					/* Must be >= 2 always	      */

#define	RESPONDTIME	(25*HZ)		/* Carrier must be raised inside this */
#define	CARRIERTIME	(1*HZ)		/* Carrier must drop for this before hangup */

#define FLUSHTIME	5		/* time required to allow hardware buffered
					 * characters to flush before setting speed */

#define SSPEED		12		/* standard speed 4800 bd */

struct	dz	/* one for each dz-11 */ 
{
	int	*dzaddr;	/* dz device address */
	char	sopen;		/* bit set for single open lines */
	char	carrier;	/* bits set for carrier controlled lines */
	char	ring;		/* bits set for lines with dial in modems */ 
	char	active;		/* bits set for active dialup lines */ 
	char	openl;		/* bits set for open lines */ 
	unsigned pyerrors;	/* count of of parity errors on input */ 
	unsigned overrors;	/* count of of overrun errors on input */ 
}
dz[NDZ]
{
	/* dzaddr  sopen  carr  ring */ 
	{ 0160040,  0002, 0077, 0074 },
	{ 0160050,  0000, 0000, 0000 }
};
int	dzscanning;	/* set when scanning for input and modem change */

/*
 *	DZ11 register layout
 */ 

struct dzr_read
{
	int	dzcsr;	/* r/w */ 
	int	dzrbuf;	/* no bit, byte, or tst ops */ 
	char	dztcr;	/* r/w */ 
	char	dzdtr;	/* r/w */ 
	char	dzring;
	char	dzcarr;
};

struct dzr_write
{
	int	dzcsr;	/* r/w */
	int	dzlpr;	/* no bit or byte ops */ 
	char	dztcr;	/* r/w */
	char	dzdtr;	/* r/w */
	char	dztbuf;	/* no bit ops */ 
	char	dzbrk;	/* no bit ops */ 
};

/*
 *	register control bits
 */ 
#define TRDY		0100000 	/* dzcsr */
#define TIE		040000
#define SA		020000
#define SAE		010000
#define TLINE		03400
#define RDONE		0200
#define RIE		0100
#define MSE		040
#define CLR		020

#define RCVR_ON 	010000		/* dzlpr */
#define S9600		07000
#define S300		02400
#define S134_5		01400
#define S110		01000
#define ODD_PAR 	0300
#define EVN_PAR 	0100
#define TWOSBIT		040
#define C8BIT		030
#define C7BIT		020
#define	C6BIT		010
/*
#define IBM2741		RCVR_ON|S134_5|ODD_PAR|C6BIT	/* if you must */ 

#define	RERROR		070000		/* dzrbuf */
#define OVR_RUN 	040000	
#define FRAME		020000
#define PARITY		010000
#define LINE_NO 	03400

/*
 *	table to map UNIX standard speeds to DZ11 speeds
 *	illegal speeds are ignored.
 */ 
char	dzspeedmap[16]
{
	    0 /*  0 - zero */ 
	, 020 /*  1 -   50 */ 
	, 021 /*  2 -   75 */ 
	, 022 /*  3 -  110 */ 
	, 023 /*  4 -  134.5 */ 
	, 024 /*  5 -  150 */ 
	,0200 /*  6 -  200 -- ## ILLEGAL ## */ 
	, 025 /*  7 -  300 */ 
	, 026 /*  8 -  600 */ 
	, 027 /*  9 - 1200 */ 
	, 030 /* 10 - 1800 */ 
	, 032 /* 11 - 2400 */ 
	, 034 /* 12 - 4800 */ 
	, 036 /* 13 - 9600 */ 
	, 031 /* 14 - ext A - maps to 2000 */ 
	, 037 /* 15 - ext B - maps to 19200 */ 
};



struct tty	dz11[NLINES];

char		dzdelays[NLINES];	/* Count of clock ticks for per-line
					 * delays. Count of <= 0 means no delay.
					 * Reduces requirement of timeouts.
					 */

int		dzringt[NLINES];	/* Delay counts for modem control */

/*
 *	open a DZ11 line
 */ 
dzopen(dev, flag)
{
	register struct tty	*tp;
	register struct dz	*dzp;
	register int		t_bit;
	extern	dzstart(), dzscan();


	if(dev.d_minor >= NLINES)
	{
		u.u_error = ENXIO;
		return;
	}
	dzp = &dz[dev.d_minor>>3];
	t_bit = (1<<(dev.d_minor&07));
	if((dzp->sopen&t_bit) && (dzp->openl&t_bit))
	{
		u.u_error = EOPENFAIL;
		return;
	}
	tp = &dz11[dev.d_minor];
	if(u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = tp;
	if((tp->t_state&ISOPEN) == 0)
	{
		tp->t_dev = dev;
		tp->t_addr = &dzstart;
		tp->t_speeds = SSPEED|(SSPEED<<8);
		tp->t_flags = ODDP|EVENP|XTABS|RAW;
		tp->t_erase = CERASE;
		tp->t_kill = CKILL;
		if(dzp->openl == 0)
			dzp->dzaddr->dzcsr =| (TIE|RIE|SAE|MSE);	/* reciever interrupt every 16 chars */ 
		dzp->openl =| t_bit;
		spl5();
		if(dzscanning == 0)
			dzscan();	/* start scanning */ 
		if(!(dzp->ring&t_bit) && !(dzp->carrier&t_bit))
			dzp->dzaddr->dzdtr =| t_bit;	/* turn on DTR for non-dialup lines */ 
		else
		{
#			ifdef	DEBUG
				printf("%d wo%d\n", time.loint, dev.d_minor);
#			endif	DEBUG
			while(!(dzp->dzaddr->dzcarr&t_bit))
			{
				tp->t_state =| WOPEN;
				sleep(&tp->t_rawq, TTIPRI);
			}
#			ifdef	DEBUG
				printf("%d op%d\n", time.loint, dev.d_minor);
#			endif	DEBUG
		}
		spl0();
		tp->t_state = (ISOPEN|CARR_ON|SSTART);
		dzparam(tp, 1);
	}
}


/*
 *	scan open lines for:
 *			1.  modem status changes
 *			2.  process timeouts as dictated by dzdelays
 *			3.  input by calling dzrint
 */ 
dzscan()	/* at spl5 */ 
{
	register struct dz	*dzp;
	struct tty		*tp;
	int			*p;
	extern dzstart(), dzrint(), dzscan();
#	define	openring	(dzp->openl & dzp->ring)
#	define	opencarrier	(dzp->openl & dzp->carrier)

	/*
	 *	scan open dialup/carrier lines.
	 */ 
	tp = &dz11[0];
	p = &dzringt[0];
	for(dzp = dz; dzp < &dz[NDZ]; dzp++)
	{
	    register int	*dzaddr;
	    char	scanl, scanc;

	    dzaddr = dzp->dzaddr;
	    if((scanl = openring) | (scanc = opencarrier))
	    {
		register int	t_bit;

		for(t_bit = 1; t_bit&0377; t_bit =<< 1, tp++, p++)
		{
		    if(scanl&t_bit)
		    {
			/* this is an open `dialup' line */ 
			if(dzp->active&t_bit)
			{
			    if(!(dzaddr->dzcarr&t_bit))
			    {
				if(*p == 0)
				    *p = CARRIERTIME;
				else if((*p =- TSCANRATE) <= 0)
				{
				    *p = 0;	/* disable */ 
#				    ifdef	DEBUG
					printf("%d hu%d\n", time.loint, tp->t_dev.d_minor);
#				    endif	DEBUG
				    dzaddr->dzdtr =& ~t_bit;	/* hang up the phone */ 
				    signal(tp, SIGHUP);
				    flushtty(tp);
				    dzaddr->dztcr =& ~t_bit;	/* disable transmit */ 
				    dzp->active =& ~t_bit;
				}
			    }
			    else
			    {
				if(tp->t_state&WOPEN)
				    wakeup(&tp->t_rawq);
				*p = 0;
				if(!(tp->t_state&TIMEOUT) && (tp->t_outq.c_cc))
				    dzaddr->dztcr =| t_bit;
			    }
			}
			else
			{
			    if(!(dzaddr->dzdtr&t_bit) && (dzaddr->dzring&t_bit))
			    {
				dzaddr->dzdtr =| t_bit;	/* answer the phone */ 
				*p = RESPONDTIME;
				dzp->active =| t_bit;
#				ifdef	DEBUG
				    printf("%d ap%d\n", time.loint, tp->t_dev.d_minor);
#				endif	DEBUG
			    }
			}
		    }
		    else if((scanc&t_bit) && (dzaddr->dzcarr&t_bit))
		    {	/* carrier only line */
			if(tp->t_state&WOPEN)
			{
			    dzaddr->dzdtr =| t_bit;
			    wakeup(&tp->t_rawq);
			}
			else if((!(tp->t_state&TIMEOUT)) && (tp->t_outq.c_cc))
			    dzaddr->dztcr =| t_bit;
		    }
		}
	    }
	    else
	    {
		tp =+ 8; p =+ 8;	/* in the case where no dialup/carrier lines on current dz */ 
	    }
	}

	/*
	 *	process timeouts for each line
	 */ 

	{
		register i;

		for(i = 0; i < NLINES; i++)
			if((dzdelays[i] > 0) && (--dzdelays[i] <= 0))
			{
				dz11[i].t_state =& ~TIMEOUT;
				dzstart(&dz11[i]);
			}
	}

	/*
	 *	scan each dz for input
	 */ 

	dzrint(0);

	/*
	 *	restart scanning if necessary
	 */ 

	dzp = dz;
	do
	{
		if(dzp->openl)
		{
			dzscanning = timeout(&dzscan, 0, TSCANRATE);
			return;
		}
		dzp++;
	} while(dzp < &dz[NDZ]);
	dzscanning = 0;
}

/*
 *	close a DZ11 line
 */ 
dzclose(dev)
{
	register struct tty *tp;
	register t_bit;
	register struct dz *dzp;

	tp = &dz11[dev.d_minor];
	wflushtty(tp);
	tp->t_state = SSTART;
	dzp = &dz[dev.d_minor>>3];
	t_bit = 1<<(dev.d_minor&07);
	if(dzp->ring&t_bit)
	{
		if(tp->t_flags&HUPCL)
			dzp->dzaddr->dzdtr =& ~t_bit;	/* hang up the phone */ 
	}
	else
	{
		dzp->dzaddr->dzdtr =& ~t_bit;	/* turn off dtr for non-dialup lines */ 
	}
	if((dzp->openl =& ~t_bit) == 0)
		dzp->dzaddr->dzcsr = 0;	/* disable receive on final close */ 
}



/*
 *	read from a DZ11 line
 */ 
dzread(dev)
{
	ttread(&dz11[dev.d_minor]);
}



/*
 *	write on a DZ11 line
 */ 
dzwrite(dev)
{
	ttwrite(&dz11[dev.d_minor]);
}



/*
 *	stty/gtty for DZ11
 */ 
dzsgtty(dev, av)
int *av;
{
	register struct tty *tp;

	if((av == 0) && (dzspeedmap[u.u_arg[0]&017] < 0))
	{
		u.u_error = ENXIO;	/* illegal speed */ 
		return;
	}
	tp = &dz11[dev.d_minor];
	if(ttystty(tp, av))
		return;
	dzparam(tp, 1);
}



/*
 *	set parameters from open or stty into DZ hardware registers
 */ 
dzparam(tp, dflag)
register struct tty *tp;
{
	register lpr, x;
	extern wakeup();


	lpr = dzspeedmap[tp->t_speeds&017]<<8;

#ifdef	IBM2741
	if(lpr == (RCVR_ON|S134_5))
		lpr = IBM2741;
	else
	{
#endif	IBM2741
		if(lpr == (RCVR_ON|S110))
			lpr =| TWOSBIT;

		if((x = tp->t_flags)&EVENP)
			if((x&ODDP) == 0)
				lpr =| (EVN_PAR|C7BIT);
			else
				lpr =| C8BIT;
		else if(x&ODDP)
			lpr =| (ODD_PAR|C7BIT);
		else
			lpr =| C8BIT;
#ifdef	IBM2741
	}
#endif	IBM2741

	if(dflag)
	{
		/* delay only if it is permissible */ 
#ifndef	DELAY
		timeout(&wakeup, tp, FLUSHTIME);	/* wakeup in 5 tics */ 
		sleep(tp, TTOPRI);	/* delay while controller flushes */ 
#else
		delay(FLUSHTIME);	/* hang 5 */ 
#endif	DELAY
	}

	dz[tp->t_dev.d_minor>>3].dzaddr->dzlpr = lpr|(tp->t_dev.d_minor&07);
}



/*
 *	start (restart) transmission on a DZ11 line
 */ 
dzstart(tp)
register struct tty *tp;
{
	register t_bit;
	register struct dz *dzp;


	t_bit = 1<<(tp->t_dev.d_minor&07);
	dzp = &dz[tp->t_dev.d_minor>>3];
	if((!(dzp->carrier&t_bit)) || (dzp->dzaddr->dzcarr&t_bit))
		dzp->dzaddr->dztcr =| t_bit;
}


/*
 *	DZ11 transmitter interrupt.
 *
 *	Scan every line on each dz.
 *	Commencing with the device that caused
 *	dzxint to be called.
 */ 
dzxint(dev)
{
	register struct tty *tp;
	register c;
	register struct dzr_read *dzaddr;
	struct dz *dzp;
	struct tty *dzbase;
	int t_bit, lino, i, n;

	n = dev.d_minor;
	for(i = 0; i < NDZ; i++)
	{
		dzaddr = (dzp = &dz[n])->dzaddr;
		dzbase = &dz11[n*8];
		while((c = dzaddr->dzcsr) < 0)	/* xmit line ready */ 
		{
			t_bit = 1<<(lino = (c>>8)&07);

			tp = &dzbase[lino];

			if((!(dzp->carrier&t_bit) || (dzaddr->dzcarr&t_bit)) && (c = getc(&tp->t_outq)) >= 0)
				if(c <= 0177 || tp->t_flags == RAW)
					dzaddr->dztbuf = c;
				else
				{
					dzaddr->dztcr =& ~t_bit;
					tp->t_state =| TIMEOUT;
					dzdelays[tp-dz11] = ((c&0177)+(TSCANRATE-1))/TSCANRATE+1;	/* set up timeout */ 
					continue;
				}
			else
				dzaddr->dztcr =& ~t_bit;

#ifdef	TTY_HISPEED
			if(tp->t_outq.c_cc <= ((tp->t_speeds&017) > B1200?TTHSLOWAT:TTLOWAT) && (tp->t_state&ASLEEP))
#else
			if(tp->t_outq.c_cc <= TTLOWAT && (tp->t_state&ASLEEP))
#endif	TTY_HISPEED
			{
				tp->t_state =& ~ASLEEP;
				wakeup(&tp->t_outq);
			}
		}
		if(++n >= NDZ)
			n = 0;
	}
}



/*
 *	DZ11 receiver interrupt
 *
 *	Scan each dz commencing with the
 *	particular device that caused this call.
 *	Storing each charater as it comes.
 */ 
dzrint(dev)
{
	register struct tty *tp;
	register c;
	register struct dzr_read *dzaddr;
	struct dz *dzp;
	struct tty *dzbase;
	int i, n, lino, t_bit;

	n = dev.d_minor;
	for(i = 0; i < NDZ; i++)
	{
		dzp = &dz[n];
		if(dzp->openl)
		{
			dzbase = &dz11[n*8];
			while((c = dzp->dzaddr->dzrbuf) < 0)	/* char present in silo */ 
			{
				tp = &dzbase[lino = ((c>>8)&07)];
				t_bit = 1<<lino;
				if(c&RERROR)
				{
					if(c&OVR_RUN)
					{
						dzp->overrors++;
#						ifdef	MESSAGES
							printf("over run on dz %d/%d\n", n, lino);
#						endif	MESSAGES
					}
					if(c&FRAME)	/* break */ 
						if(tp->t_flags&RAW)
							c = 0;	/* null ( for getty ) */ 
						else
#						    ifdef	INTR_ON_BREAK
							c = CINTR;	/* del for NCRs. */ 
#						    else
							continue;	/* ignore framing errors if not raw */ 
#						    endif	INTR_ON_BREAK
					else if(c&PARITY)
					{
						dzp->pyerrors++;
#						ifdef	MESSAGES
							printf("parity on dz %d/%d\n", n, lino);
#						endif	MESSAGES
						continue;	/* throw away bad chars */ 
					}
				}
				if((!(dzp->carrier&t_bit)) || (dzp->dzaddr->dzcarr&t_bit))
					ttyinput(c, tp);
			}
		}
		if(++n >= NDZ)
			n = 0;
	}
}
