/*
 * one dkmodule per hardware interface to datakit
 * a given hardware device == (major device, range of minor devices)
 * channel 0 == (dev, lo)
 * there are hi-lo channels
 */

struct	dkmodule {
	unsigned char	*dkstate;	/* open/closed status of channels */
	queue_t		*listnrq;	/* channel to controller */
	unsigned short	dev;		/* major device of datakit interface */
	unsigned short	lo, hi;		/* range of devs on this controller */
	unsigned short	type;		/* type of listener */
};

#ifdef KERNEL
struct dkmodule *dkmodall(), *getdkmod();
#endif

/*
 * channel states
 */
#define	DKCLOSED 0
#define	DKRCLOSE 1		/* remote hung up, local still around */
#define	DKLCLOSE 2		/* closed locally, CMC hasn't acked yet */
#define	DKOPEN	 3		/* in use */

/*
 * listener types
 */
#define	CMCLD	'c'
#define	UNIXPLD	'u'

/*
 * M_PRICTL messages contain
 * one byte of subtype
 * perhaps another byte of channel number
 */

#define	DKMCLOSE	0x00	/* this channel is closing */
#define	DKMXINIT	0x01	/* re-init URP because of splice */
#define DKMBUFFER	0x02	/* ok to buffer in driver */
