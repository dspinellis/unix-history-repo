#define LOGFILE		"/usr/spool/log/nimd_log"
#define HELPFILE	"/usr/local/lib/nim/help"
#define NUIFILE		"/usr/local/lib/nim/nui"

#define MAXPSIZ		(256+1)		/* maximum packet size + 1 */
#define DEFAULT_PROFILE		0	/* default profile */

#define INVALID		-1	/* illegal parameter */
#define NPROFILES	6	/* number of standard profiles */

#define C_FORWARD	01
#define C_ERASE		02
#define C_KILL		04
#define C_DISPLAY	010
#define C_ESCAPE	020
#define C_TYPE(c)	(chartab[c&0177] & 0176)
#define ISFORWARD(c)	(chartab[c&0177] & C_FORWARD)

#define ST_COMMAND	01	/* command mode */
#define ST_DATA		02	/* data mode */
#define ST_ESCSEEN	04	/* received nim escape character from pty */
#define ST_ESCCOMM	010	/* escaped to command mode */
#define ST_UGLY_50_BAUD_BREAK_IN_PROGRESS	020

#define FROMPTY		0
#define FROMNET		1
#define FROMNIM		2

#define MAXQSIZ		256		/* maximum number size of any queue */

short	CurrentX29Parms[128];		/* current x.29 parameter values */
short	CurrentProfile;			/* current profile number (1 - 6) */
short	pnums[];			/* list of valid parameter numbers */
short	State;				/* NIM state (command or data) */
short	OutputBlocked;			/* remote host sent ^S */

extern	short NetFd, PtyFd;
extern	char Banner[];

#define CCITT1978	0
#define CCITT1980	1

struct	netinfo {
	short	n_type;		/* CCITT1978 or CCITT1980 */
	short	n_nparms;	/* number of parameters */
	short	n_psize;	/* current packet size */
} NetInfo;

char	*rindex(), *strcat(), *strcpy(), *strncpy(), *rindex();
