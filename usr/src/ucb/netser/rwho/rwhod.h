/*	rwhod.h	4.3	82/12/24	*/

struct	whod {
	int	wd_sendtime;
	int	wd_recvtime;
	char	wd_hostname[32];
	int	wd_loadav[3];
	int	wd_boottime;
	struct	whoent {
		struct	utmp we_utmp;
		int	we_idle;
	} wd_we[1024 / sizeof (struct whoent)];
};

#define	RWHODIR	"/usr/spool/rwho"	/* where data is stored */
