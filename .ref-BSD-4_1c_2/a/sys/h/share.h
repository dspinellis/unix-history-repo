/*	share.h	(Melb/Basser) 4.1	82/07/16	*/

/*
 *	 MUSH Share scheduling parameters
 */


struct sh_consts
{
	short	sc_fl;		/* scheduling flags */
	short	sc_maxnice;	/* maximum nice cut-off */
	float	sc_usagfactor;	/* fudge factor */
	float	sc_ratefactor;	/* fudge factor */
	long	sc_syscall;	/* cost of system call */
	long	sc_bio;		/*   "  "  logical block i/o */
	long	sc_tio;		/*   "  "  terminal i/o */
	long	sc_tic;		/*   "  "  cpu tick */
	long	sc_click;	/*   "  "  memory */
	long	sc_pgin;	/*   "  "  hard page fault */
	float	sc_usage;	/* current share per usage */
	float	sc_rate;	/* current share per rate */
	float	sc_usagk;	/* usage decay rate */
	long	sc_cost;	/* total costs last period */
	short	sc_users;	/* number of real users */
	short	sc_umin;	/* minimum users for min share calc */
	float	sc_ksmin;	/* constant for min share */
	short	sc_cpuwait;	/* maximum period to wait for cpu */
};

#ifdef	KERNEL
int	callshare;
extern struct sh_consts	shconsts;
#endif

#define	shareflags	shconsts.sc_fl
#define	Totusage	shconsts.sc_usage
#define	Avrate		shconsts.sc_rate
#define	maxnice		shconsts.sc_maxnice
#define	USAGEFACTOR	shconsts.sc_usagfactor
#define	RATEFACTOR	shconsts.sc_ratefactor
#define	Usagek		shconsts.sc_usagk
#define	Totusers	shconsts.sc_users
#define	Totcost		shconsts.sc_cost
#define	MINSHK		shconsts.sc_ksmin
#define	MINUSERS	shconsts.sc_umin
#define	CPUWAIT		shconsts.sc_cpuwait

#define	NOSHARE		1
#define	NONICE		2

#define	MINNICE		0
#define	BLOODYNICE	40

#define	MINUSAGE	(1.0e6)

#define	USAGEK		0.9999834	/* decay usage 40% per day */

#define	CHARGE(cost)	(u.u_quota->q_cost += shconsts.cost)
