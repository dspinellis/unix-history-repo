#include <stdio.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/time.h>

/*
 * Output the date in right format gotten from
 * the passed ctime(3) format date.
 * If the passed date is NULL, output the current
 * date and time.
 */

putdate(timestr, out)
	char *timestr;
	register FILE *out;
{
	register char *t, *p, *cp;
	char *timezone();
	struct tm *tmp;
	struct tm *localtime();
	struct timeb tb;
	long now;
	int isdst;
	char *asctime();
	static char *str = "SunMonTueWedThuFriSat";
	static char *daytag[] = {
		"Sunday",
		"Monday",
		"Tuesday",
		"Wednesday",
		"Thursday",
		"Friday",
		"Saturday"
	};

	now = time((long *) 0);
	tmp = localtime(&now);
	isdst = tmp->tm_isdst;
	if (timestr == 0)
		timestr = asctime(tmp);
	cp = str;
	t = timestr;
	while (*cp) {
		if (strcmpn(cp, t, 3) == 0)
			break;
		cp += 3;
	}
	if (*cp)
		cp = daytag[(cp - str) / 3];
	else
		cp = 0;
	ftime(&tb);
	/*
	 * This call to timezone() may be wrong:
	 * really need the tm_isdst whoever generated timestr.
	 */
	p = timezone(tb.timezone, isdst);

	if (cp == 0)
		fprintf(out, "Date: %.2s %.3s %.4s %.2s%.2s-%.3s\n",
			     t+8, t+4, t+20, t+11, t+14, p);
	else
		fprintf(out, "Date: %.2s %.3s %.4s %.2s%.2s-%.3s (%s)\n",
			     t+8, t+4, t+20, t+11, t+14, p, cp);
}
