/*	rwhod.h	4.2	82/11/14	*/

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
