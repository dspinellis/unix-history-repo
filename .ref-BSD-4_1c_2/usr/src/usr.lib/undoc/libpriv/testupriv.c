/*	@(#)testupriv.c	4.2	(Melbourne)	82/01/28	*/

#include <sys/types.h>
#include <udata.h>

testupriv(unum, priv)
	int	unum;
	enum u_priv priv;
{
	static   int		cup_usr = -1;
	static   struct udata	cup_data;
	static   int		cup_fd = -1;
	register int		wd, bit;

	if (cup_usr != unum) {
		if (cup_fd == -1)
			if ((cup_fd = open(UPRIVFILE, 0)) == -1) {
				perror(UPRIVFILE);
				return(0);
			}
		lseek(cup_fd, (long)(cup_usr = unum) * sizeof(struct udata), 0);

		switch (read(cup_fd, &cup_data, sizeof(struct udata))) {

		case sizeof(struct udata):
			break;

		case 0:
			privzero(&cup_data, sizeof cup_data);
			break;

		case -1:
			perror(UPRIVFILE);
			cup_usr = -1;
			return(0);

		default:
			/*	HELP	*/
			write(2, "screwed upriv file - help!\n", 27);
			cup_usr = -1;
			return(0);
		}
	}
	if ((bit = (int)priv) < BITSIN(cup_data.ud_flags)) {

		wd = bit / BITSIN(cup_data.ud_flags[0]);
		bit %= BITSIN(cup_data.ud_flags[0]);

		return(cup_data.ud_flags[wd] & (1 << bit));
	}
	return(0);
}
