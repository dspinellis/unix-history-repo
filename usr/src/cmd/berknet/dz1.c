/*
 *	DZ-11 driver
 *	------------
 *
 *	OPTIMIZED dz driver to handle multiple dzs as efficiently
 *	as possible.  The efficiency is gained by disabling all
 *	dz transmitter interrupts and using a KW11-P to generate
 *	suitable interrupts.  Carrier is supported but not Ring.
 *
 *				Ian Johnstone	UNSW
 *				May 1979
 */ 

#include	"../defines.h"
#include	"../param.h"
#include	"../conf.h"
#include	"../user.h"
#include	"../tty.h"
#include	"../proc.h"

#define	NDZ		 7		/* no. of dz-11s */

#define NDZLIN		 8		/* no. of lines per dz DO NOT ALTER */
#define NLINES	(NDZLIN*NDZ)		/* total no. of lines available */

#define SSPEED		11		/* standard speed 2400 bd */

#define CLOCK 		 0172540	/* kw11-p lives here */

struct
{
	int csr;			/* control and status */
#define GO	0101			/* down, single, 100K, run */
	unsigned counter;		/* counter */
};

struct tty dz11[NLINES];		/* tty structures for this dz */

struct dz				/* one for each dz-11 */ 
{
	int *dzaddr;			/* control registers for this dz */ 
	char nocarr;			/* set for lines WITHOUT carrier */ 
	char sopen;			/* set for lines with exclusive use */ 
	struct tty *ttys[NDZLIN];	/* address of tty structs this dz */
					/* that is only ONE `open' allowed */ 
	char openl;			/* flags for open lines */ 
	char closl;			/* flags to indicate closing lines */
	char xmit;			/* set for lines to transmit on */
	unsigned pyerrors;		/* number of parity errors on input */ 
	unsigned overrors;		/* number of overrun errors on input */ 
	int closet[NDZLIN];		/* handle closing via this field */
}
dz[NDZ]
{
	{
	  0160100, 0377, 0000,
	  &dz11[000],&dz11[001],&dz11[002],&dz11[003],
	  &dz11[004],&dz11[005],&dz11[006],&dz11[007]
	},
	{
	  0160110, 0007, 0000,
	  &dz11[010],&dz11[011],&dz11[012],&dz11[013],
	  &dz11[014],&dz11[015],&dz11[016],&dz11[017]
	},
	{
	  0160120, 0320, 0020,
	  &dz11[020],&dz11[021],&dz11[022],&dz11[023],
	  &dz11[024],&dz11[025],&dz11[026],&dz11[027]
	},
	{
	  0160130, 0000, 0000,
	  &dz11[030],&dz11[031],&dz11[032],&dz11[033],
	  &dz11[034],&dz11[035],&dz11[036],&dz11[037]
	},
	{
	  0160140, 0000, 0000,
	  &dz11[040],&dz11[041],&dz11[042],&dz11[043],
	  &dz11[044],&dz11[045],&dz11[046],&dz11[047]
	},
	{
	  0160150, 0360, 0100,
	  &dz11[050],&dz11[051],&dz11[052],&dz11[053],
	  &dz11[054],&dz11[055],&dz11[056],&dz11[057]
	},
	{
	  0160160, 0110, 0013,
	  &dz11[060],&dz11[061],&dz11[062],&dz11[063],
	  &dz11[064],&dz11[065],&dz11[066],&dz11[067]
	},
/*
	{
	  0160170, 0377, 0000,
	  &dz11[070],&dz11[071],&dz11[072],&dz11[073],
	  &dz11[074],&dz11[075],&dz11[076],&dz11[077]
	}
*/
};

int dzopenc;			/* equal to total number of 'open' lines */

int dzrcvscan;			/* when <= 0 scan receiver silos */

char dzbitab[NDZLIN]		/* convert line numbers to bit pattern */
{
	0001, 0002, 0004, 0010, 0020, 0040, 0100, 0200
};

#define	SPLDZ	spl5		/* dz interrupts at this priority */

/*
 *	DZ11 register layout
 */ 
struct dzr_read
{
	int dzcsr;	/* r/w */ 
	int dzrbuf;	/* no bit, byte, or tst ops */ 
	char dztcr;	/* r/w */ 
	char dzdtr;	/* r/w */ 
	char dzring;
	char dzcarr;
};
struct dzr_write
{
	int dzcsr;
	int dzlpr;	/* no bit or byte ops */ 
	char dztcr;
	char dzdtr;
	char dztbuf;	/* no bit ops */ 
	char dzbrk;	/* no bit ops */ 
};
/*
 *	register control bits
 */ 
#define SAE		010000		/* dzcsr */
#define RIE		0100
#define MSE		040
#define RCVR_ON 	010000		/* dzlpr */
#define ODD_PAR 	0300
#define EVN_PAR 	0100
#define TWOSBIT		040
#define C8BIT		030
#define C7BIT		020
#define	RERROR		070000		/* dzrbuf */
#define OVR_RUN 	040000	
#define FRAME		020000
#define PARITY		010000

/*
 *	Table to map UNIX standard speeds to DZ11 speeds.
 *	Illegal speeds are ignored, and are indicated by 0200 bit.
 */ 
char dzspeedmap[16]
{
	  0200	/* 0 - zero */ 
	, 0220	/* 1 - 50 */ 
	, 0221	/* 2 - 75 */ 
	, 0222	/* 3 - 110 */ 
	, 0223	/* 4 - 134.5 */ 
	, 0224	/* 5 - 150 */ 
	, 0200	/* 6 - ILLEGAL */ 
#define	LOWSPEED 7	/* lowest speed allowed on dz */
	,  025	/* 7 - 300 */ 
	,  026	/* 8 - 600 */ 
	,  027	/* 9 - 1200 */ 
	, 0230	/* 10 - 1800 */ 
	,  032	/* 11 - 2400 */ 
	,  034	/* 12 - 4800 */ 
	,  036	/* 13 - 9600 */ 
	, 0231	/* 14 - ext A - maps to 2000 */ 
	, 0237	/* 15 - ext B - maps to 19200 */ 
};

/*
 *	Table to map UNIX standard speeds to time between interrupts for
 *	a line running at that speed.  The value in the table is multiplied
 *	by 10 to get a value in microseconds.  A nominal 20 microseconds
 *	is subtracted to make up for interrupt overhead.
 */ 

unsigned dzmicmap[16]
{
	       0		/* 0 - zero */ 
	,  19998		/* 1 - 50 */ 
	,  13331		/* 2 - 75 */ 
	,   9088		/* 3 - 110 */ 
	,   7433		/* 4 - 134.5 */ 
	,   6665		/* 5 - 150 */ 
	,      0		/* 6 - ILLEGAL */ 
	,   3331		/* 7 - 300 */ 
	,   1665		/* 8 - 600 */ 
	,    831		/* 9 - 1200 */ 
	,    554		/* 10 - 1800 */ 
	,    415		/* 11 - 2400 */ 
	,    206		/* 12 - 4800 */ 
	,    102		/* 13 - 9600 */ 
	,    498		/* 14 - ext A - maps to 2000 */ 
	,     50		/* 15 - ext B - maps to 19200 */ 
};

/*
 *	open a DZ11 line
 */ 
dzopen(dev, flag)
{
	extern dzstart();
	register struct tty *tp;
	register struct dz *dzp;
	register lino;

	lino =  dev.d_minor;
	if(lino >= NLINES)
	{
		u.u_error = ENXIO;
		return;
	}
	dzp   =  &dz[lino>>3];
	if(!fkword(dzp->dzaddr))		/* fix036 */
	{
		u.u_error = ENXIO;
		return;
	}					/* fix036 */
	tp    =  &dz11[lino];
	lino =& 07;

	if( (dzp->sopen&dzbitab[lino]) && (dzp->openl&dzbitab[lino]) )
	{
		u.u_error = EOPENFAIL;
		return;
	}

	if(u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = tp;

	SPLDZ();

	if( (dzp->openl&dzbitab[lino]) == 0 )
	{
		tp->t_dev    = dev;
		tp->t_state  = (ISOPEN|CARR_ON|SSTART);
		tp->t_addr   = &dzstart;
		tp->t_speeds = SSPEED|(SSPEED<<8);
		tp->t_flags  = ODDP|EVENP|XTABS|RAW;
		tp->t_erase  = CERASE;
		tp->t_kill   = CKILL;

		dzparam(tp);

		if(dzp->openl == 0)
			dzp->dzaddr->dzcsr =| (RIE|SAE|MSE); /* init */

		dzp->openl =| dzbitab[lino];

		if(dzopenc++ == 0)
			dzxint();	/* start transmitting */

	}
	else
		dzp->closl =& ~dzbitab[lino];
	spl0();
}

/*
 *	close a DZ11 line
 */ 
dzclose(dev)
{
	register struct tty *tp;
	register struct dz *dzp;
	register lino;

	lino  =  dev.d_minor;
	dzp   =  &dz[lino>>3];
	tp    =  &dz11[lino];
	lino  =& 07;

	dzp->closet[lino] =  tp->t_outq.c_cc << 1;	/* time for close */
	dzp->closl  =| dzbitab[lino];
	dzp->xmit   =| dzbitab[lino];			/* start transmitting */
	dzp->dzaddr->dztcr =| dzbitab[lino];		/* start transmitting */
}

/*
 *	read from a DZ11 line
 */ 
dzread(dev)
{
	ttread( &dz11[dev.d_minor] );
}

/*
 *	write on a DZ11 line
 */ 
dzwrite(dev)
{
	ttwrite( &dz11[dev.d_minor] );
}

/*
 *	stty/gtty for DZ11
 */ 
dzsgtty(dev, av)
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
	dzparam(tp);
}

/*
 *	set parameters from open or stty into DZ hardware registers
 */ 
dzparam(tp)
register struct tty *tp;
{
	register lpr, x;
	extern wakeup();

	lpr = dzspeedmap[tp->t_speeds&017]<<8;

	if((x = tp->t_flags)&EVENP)
		if((x&ODDP) == 0)
			lpr =| (EVN_PAR|C7BIT);
		else
			lpr =| C8BIT;
	else if(x&ODDP)
		lpr =| (ODD_PAR|C7BIT);
	else
		lpr =| C8BIT;

	/* set new speed, char currently in uart may be screwed */ 

	dz[tp->t_dev.d_minor>>3].dzaddr->dzlpr = lpr|(tp->t_dev.d_minor&07);
}
/*
 *	dz start routine
 */
dzstart(tp)	/* at SPLDZ */
struct tty *tp;
{
	register lino = tp->t_dev.d_minor;
	register struct dz *dzp;

	dzp  =  &dz[lino>>3];
	lino =& 07;
	dzp->xmit =| dzbitab[lino];		/* start transmitting */
	dzp->dzaddr->dztcr =| dzbitab[lino];	/* start transmitting */
}

/*
 *	DZ11 transmitter interrupt.
 *
 *	Scan every line on each dz.  Internal dz limitations
 *	force this scan to take an unusual form.  One line
 *	from each dz is serviced each scan until no dz requires
 *	service.  This is less efficient than servicing
 *	entirely a dz prior to scanning the next dz but it
 *	it is necessary.
 *
 *	dzxint is not actually invoked by a dz interrupt
 *	rather it is invoked by a clock interrupt.
 *	to drive multiple dz's efficiently utilizing dz
 *	transmitter interrupts is just NOT possible.
 */ 
int dzxc;
dzxint()	 /* at SPLDZ */ 
{
	extern ttrstrt();
	register struct dz *dzp;
	int hspeed = LOWSPEED;	/* to determine clock speed */
	int flag;		/* control dz scanning */

dzxc++;	/* count */
	if( dzopenc == 0 ) return;	/* stop if inactive */

	/*	scan every dz for characters to transmit	*/

   do
   {
	for(dzp = &dz[0], flag=0; dzp < &dz[NDZ]; dzp++ )
	{
		register struct tty *tp;
		register struct dzr_read *dza = dzp->dzaddr;
		int lino, t_bit;

		if((lino = dza->dzcsr.hibyte)  < 0) /* xmit ?? */ 
		{
			lino =& 07;		/* isolate line number */ 
			tp = dzp->ttys[lino];
			t_bit = dzbitab[lino];	/* bit mask, not line number */
			flag++;			/* note service */
			if( (dzp->closl & t_bit)
			&& ((--(dzp->closet[lino]) <= 0) || (tp->t_outq.c_cc == 0)) )
			{
				/* line closed, no time or chars left */
				flushtty(tp);
				tp->t_state = SSTART;
				dzp->closl =& ~t_bit;
				dzp->openl =& ~t_bit;
				dzp->xmit  =& ~t_bit;
				dza->dztcr =& ~t_bit;
				dza->dzdtr =& ~t_bit;
				if( (dzp->closl==0) && (dzp->openl==0) )
					dza->dzcsr = 0;
				if( --dzopenc == 0 )
					return;
			}
			else if(tp->t_outq.c_cc == 0)
			{
				dzp->xmit =& ~t_bit;
				dza->dztcr =& ~t_bit;
			}
			else if((dzp->nocarr&t_bit)||(dza->dzcarr&t_bit))
			{
				int c = getc(&tp->t_outq);
				if( c <= 0177 || tp->t_flags == RAW )
				{
					/* transmit the char for this line */ 
					dza->dztbuf = c;
					if( tp->t_speeds.lobyte > hspeed )
						hspeed = tp->t_speeds.lobyte;
				}
				else
				{
					dzp->xmit =& ~t_bit;
					dza->dztcr =& ~t_bit;
					timeout( &ttrstrt, tp, c&0177 );
					tp->t_state =| TIMEOUT;
				}
				/* if low water mark then want more */ 
				if( tp->t_state&ASLEEP
				&&  tp->t_outq.c_cc <= TTLOWAT )
				{
					tp->t_state =& ~ASLEEP;
					wakeup(&tp->t_outq);
				}
			}
			else
			{
				dza->dztcr =& ~t_bit;
			}
		}
	}
  } while( flag );

	/* finalize state of DZs prior to exitting */

	for(dzp = &dz[0]; dzp < &dz[NDZ]; dzp++ )
	{
		register struct dzr_read *dza = dzp->dzaddr;

		/* dtr to reflect state of carrier, for carrier lines */ 

		dza->dzdtr = (dza->dzcarr | dzp->nocarr) & dzp->openl;

		/* Enable all lines still with characters to send */

		dza->dztcr = dzp->xmit;
	}

	/*	setup for next interrupt 	*/

	CLOCK->counter = dzmicmap[hspeed];	/* count in 10microseconds */
	CLOCK->csr = GO;

	/*	call dzrint if needed	*/

	if( dzrcvscan <= 0 )
		dzrint(0);
	dzrcvscan =- dzmicmap[hspeed];
}

/*
 *	DZ11 receiver interrupt
 *
 *	Scan each dz commencing with the particular device that caused this call
 *	Scan at least every dzmicmap[LOWSPEED] microseconds.
 */ 
dzrint(dev)
{
	register struct tty *tp;
	register struct dz *dzp;
	register int lino;
	int i, c;

	for(dzp = &dz[dev], i = 0; i < NDZ; i++)
	{
		while((c = dzp->dzaddr->dzrbuf) < 0)	/* char present in silo */ 
		{
			lino =  c.hibyte; lino =& 07;
			if( ((dzp->nocarr&dzbitab[lino]) == 0 )
			&&  ((dzp->dzaddr->dzcarr&dzbitab[lino]) == 0 )) continue;
			if( (dzp->openl&dzbitab[lino]) == 0 ) continue;
			tp = dzp->ttys[lino];
			if(c&RERROR)
			{
				if( (c & FRAME) && (tp->t_flags & RAW ) )
					ttyinput(0, tp); /* break for getty */
				else if(c & OVR_RUN)
					dzp->overrors++;
				else if(c & PARITY)
					dzp->pyerrors++;
			}
			else
			{
				ttyinput(c, tp);
			}
		}
		if( ++dzp >= &dz[NDZ] ) dzp = &dz[0];
	}
	dzrcvscan = dzmicmap[LOWSPEED];
}
