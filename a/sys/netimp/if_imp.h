/*	if_imp.h	4.6	82/03/19	*/

/*
 * Structure of IMP 1822 long leader.
 */
struct control_leader {
	u_char	dl_format;	/* leader format */
	u_char	dl_network;	/* src/dest network */
	u_char	dl_flags;	/* leader flags */
	u_char	dl_mtype;	/* message type */
	u_char	dl_htype;	/* handling type */
	u_char	dl_host;	/* host number */
	u_short	dl_imp;		/* imp field */
	u_char	dl_link;	/* link number */
	u_char	dl_subtype;	/* message subtype */
};

struct imp_leader {
	struct	control_leader il_dl;
#define	il_format	il_dl.dl_format
#define	il_network	il_dl.dl_network
#define	il_flags	il_dl.dl_flags
#define	il_mtype	il_dl.dl_mtype
#define	il_htype	il_dl.dl_htype
#define	il_host		il_dl.dl_host
#define	il_imp		il_dl.dl_imp
#define	il_link		il_dl.dl_link
#define	il_subtype	il_dl.dl_subtype
	u_short	il_length;	/* message length */
};

#define	IMP_DROPCNT	2	/* # of noops from imp to ignore */
/* insure things are even... */
#define	IMPMTU		((8159 / NBBY) & ~01)

/*
 * IMP-host flags
 */
#define	IMP_NFF		0xf	/* 96-bit (new) format */
#define	IMP_TRACE	0x8	/* trace message route */

#define	IMP_DMASK	0x3	/* host going down mask */

/*
 * IMP-host message types.
 */
#define	IMPTYPE_DATA		0	/* data for protocol */
#define	IMPTYPE_BADLEADER	1	/* leader error */
#define	IMPTYPE_DOWN		2	/* imp going down */
#define	IMPTYPE_NOOP		4	/* noop seen during initialization */
#define	IMPTYPE_RFNM		5	/* request for new messages */
#define	IMPTYPE_HOSTDEAD	6	/* host doesn't respond */
#define	IMPTYPE_HOSTUNREACH	7	/* host unreachable */
#define	IMPTYPE_BADDATA		8	/* data error */
#define	IMPTYPE_INCOMPLETE	9	/* incomplete message, send rest */
#define	IMPTYPE_RESET		10	/* reset complete */
/* non-blocking IMP interface */
#define	IMPTYPE_RETRY		11	/* IMP refused, try again */
#define	IMPTYPE_NOTIFY		12	/* IMP refused, will notify */
#define	IMPTYPE_TRYING		13	/* IMP refused, still rexmt'ng */
#define	IMPTYPE_READY		14	/* ready for next message */

/*
 * IMPTYPE_DOWN subtypes.
 */
#define	IMPDOWN_GOING		0	/* 30 secs */
#define	IMPDOWN_PM		1	/* hardware PM */
#define	IMPDOWN_RELOAD		2	/* software reload */
#define	IMPDOWN_RESTART		3	/* emergency restart */

/*
 * IMPTYPE_BADLEADER subtypes.
 */
#define	IMPLEADER_ERR		0	/* error flip-flop set */
#define	IMPLEADER_SHORT		1	/* leader < 80 bits */
#define	IMPLEADER_TYPE		2	/* illegal type field */
#define	IMPLEADER_OPPOSITE	3	/* opposite leader type */

/*
 * IMPTYPE_HOSTDEAD subtypes.
 */
#define	IMPHOST_NORDY		1	/* ready-line negated */
#define	IMPHOST_TARDY		2	/* tardy receiving mesgs */
#define	IMPHOST_NOEXIST		3	/* NCC doesn't know host */
#define	IMPHOST_IMPSOFT		4	/* IMP software won't allow mesgs */
#define	IMPHOST_PM		5	/* host down for scheduled PM */
#define	IMPHOST_HARDSCHED	6	/* " " " " hardware work */
#define	IMPHOST_SOFTSCHED	7	/* " " " " software work */
#define	IMPHOST_RESTART		8	/* host down for emergency restart */
#define	IMPHOST_POWER		9	/* down because of power outage */
#define	IMPHOST_BREAKPOINT	10	/* host stopped at a breakpoint */
#define	IMPHOST_HARDWARE	11	/* hardware failure */
#define	IMPHOST_NOTUP		12	/* host not scheduled to be up */
/* 13-14 currently unused */
#define	IMPHOST_COMINGUP	15	/* host in process of coming up */

/*
 * IMPTYPE_HOSTUNREACH subtypes.
 */
#define	IMPREACH_IMP		0	/* destination IMP can't be reached */
#define	IMPREACH_HOSTUP		1	/* destination host isn't up */
#define	IMPREACH_LEADER		2	/* host doesn't support long leader */
#define	IMPREACH_PROHIBITED	3	/* communication is prohibited */

/*
 * IMPTYPE_INCOMPLETE subtypes.
 */
#define	IMPCOMPLETE_SLOW	0	/* host didn't take data fast enough */
#define	IMPCOMPLETE_TOOLONG	1	/* message was too long */
#define	IMPCOMPLETE_TIMEOUT	2	/* mesg transmission time > 15 sec. */
#define	IMPCOMPLETE_FAILURE	3	/* IMP/circuit failure */
#define	IMPCOMPLETE_NOSPACE	4	/* no resources within 15 sec. */
#define	IMPCOMPLETE_IMPIO	5	/* src IMP I/O failure during receipt */

/*
 * IMPTYPE_RETRY subtypes.
 */
#define	IMPRETRY_BUFFER		0	/* IMP buffer wasn't available */
#define	IMPRETRY_BLOCK		1	/* connection block unavailable */

/*
 * Data structure shared between IMP protocol module and hardware
 * interface driver.  Used to allow layering of IMP routines on top
 * of varying device drivers.  NOTE: there's a possible problem 
 * with ambiguity in the ``unit'' definition which is implicitly
 * shared by the both IMP and device code.  If we have two IMPs,
 * with each on top of a device of the same unit, things won't work.
 * The assumption is if you've got multiple IMPs, then they all run
 * on top of the same type of device, or they must have different units.
 */
struct impcb {
	char	ic_oactive;		/* output in progress */
	int	(*ic_init)();		/* hardware init routine */
	int	(*ic_start)();		/* hardware start output routine */
};

/*
 * State of an IMP.
 */
#define	IMPS_DOWN	0		/* unavailable, don't use */
#define	IMPS_GOINGDOWN	1		/* been told we go down soon */
#define	IMPS_INIT	2		/* coming up */
#define	IMPS_UP		3		/* ready to go */
#define	IMPS_RESET	4		/* reset in progress */

#define	IMPTV_DOWN	(30*60)		/* going down timer 30 secs */

#ifdef IMPLEADERS
char *impleaders[IMPTYPE_READY+1] = {
	"DATA", "BADLEADER", "DOWN", "bad", "NOOP", "RFNM", "HOSTDEAD",
	"HOSTUNREACH", "BADDATA", "INCOMPLETE", "RESET", "RETRY",
	"NOTIFY", "TRYING", "READY"
};
#endif
