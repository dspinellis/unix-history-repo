/*	lastlog.h	4.1	83/05/03	*/

struct lastlog {
	time_t	ll_time;
	char	ll_line[8];
};
