/*	@(#)getlpquota.c	4.2	(Melbourne)	82/02/04	*/

/*
 * Melb Uni Comp Sci
 *
 *
 *	Paper usage quotas
 *
 *	procedure 'getlpquota' examines the paper quota file.
 *	 The final quota setup is passed to the parameter ll.
 */

#include <stdio.h>
#include <sys/types.h>
#include <lpdquota.h>
#include <time.h>
#define	LPQSIZE (sizeof(struct lpquota))

getlpquota(uid, ll)
	struct lpquota *ll;
	int uid;
{
	int	qfd;

	qfd = open(QFILE, 0);
	if (qfd >= 0) {
		register struct tm *tp;
		extern struct tm *localtime();
		int year, yday, wday;
		register long temp;
		time_t	nowtime;

		time(&nowtime);
		tp = localtime(&nowtime);
		year = tp->tm_year;
		yday = tp->tm_yday;
		wday = tp->tm_wday;

		lseek(qfd, (long)uid * LPQSIZE, 0);
		if (read(qfd, ll, LPQSIZE) == LPQSIZE) {

			tp = localtime(&ll->lpq_last);

			if (ll->lpq_dmax) {
				temp = ll->lpq_today;
				if (tp->tm_yday!=yday || tp->tm_year!=year) {
					temp += ((year-tp->tm_year)*365 +
					  yday-tp->tm_yday) * ll->lpq_daily;
					if (temp > ll->lpq_dmax)
						temp = ll->lpq_dmax;
				}
				ll->lpq_today = temp;
			}
			if (ll->lpq_wmax) {
				temp = ll->lpq_week;
				if (wday < tp->tm_wday || yday-tp->tm_yday >= 7 
				    || tp->tm_year<year-1 || (tp->tm_year!=year
				      && tp->tm_yday-yday<360)) {
					temp += ((year-tp->tm_year)*365 +
					  yday-tp->tm_yday + 6) / 7 *
					  ll->lpq_allow;
					if (temp > ll->lpq_wmax)
						temp = ll->lpq_wmax;
				}
				ll->lpq_week = temp;
			}
		}
		close(qfd);
		return(1);
	}
	return(0);
}
