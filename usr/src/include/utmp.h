/*	utmp.h	4.2	83/05/22	*/

/*
 * Structure of utmp and wtmp files.
 *
 * Assuming the number 8 is unwise.
 */
struct utmp {
	char	ut_line[8];		/* tty name */
	char	ut_name[8];		/* user id */
	char	ut_host[16];		/* host name, if remote */
	long	ut_time;		/* time on */
};
