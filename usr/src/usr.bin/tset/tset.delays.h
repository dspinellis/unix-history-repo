/*
**  SYSTEM DEPENDENT TERMINAL DELAY TABLES
**
**	Evans Hall VAX
**
**	This file maintains the correspondence between the delays
**	defined in /etc/termcap and the delay algorithms on a
**	particular system.  For each type of delay, the bits used
**	for that delay must be specified (in XXbits) and a table
**	must be defined giving correspondences between delays and
**	algorithms.  Algorithms which are not fixed delays (such
**	as dependent on current column or line number) must be
**	cludged in some way at this time.
*/



/*
**  Carriage Return delays
*/

int	CRbits = CRDELAY;
struct delay	CRdelay[] =
{
	0,	CR0,
	9,	CR3,
	80,	CR1,
	160,	CR2,
	-1
};

/*
**  New Line delays
*/

int	NLbits = NLDELAY;
struct delay	NLdelay[] =
{
	0,	NL0,
	66,	NL1,		/* special M37 delay */
	100,	NL2,
	-1
};


/*
**  Back Space delays
*/

int	BSbits = BSDELAY;
struct delay	BSdelay[] =
{
	0,	BS0,
	-1
};


/*
**  TaB delays
*/

int	TBbits = TBDELAY;
struct delay	TBdelay[] =
{
	0,	TAB0,
	11,	TAB1,		/* special M37 delay */
	-1
};


/*
**  Form Feed delays
*/

int	FFbits = VTDELAY;
struct delay	FFdelay[] =
{
	0,	FF0,
	2000,	FF1,
	-1
};
