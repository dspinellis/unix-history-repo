/*	rwhod.h	4.5	83/05/05	*/

/*
 * rwho protocol packet format.
 */
struct	whod {
	char	wd_vers;		/* protocol version # */
	char	wd_type;		/* packet type, see below */
	char	wd_pad[2];
	int	wd_sendtime;		/* time stamp by sender */
	int	wd_recvtime;		/* time stamp applied by receiver */
	char	wd_hostname[32];	/* hosts's name */
	int	wd_loadav[3];		/* load average as in uptime */
	int	wd_boottime;		/* time system booted */
	struct	whoent {
		struct	utmp we_utmp;	/* active tty info */
		int	we_idle;	/* tty idle time */
	} wd_we[1024 / sizeof (struct whoent)];
};

#define	WHODVERSION	1
#define	WHODTYPE_STATUS	1		/* host status */
