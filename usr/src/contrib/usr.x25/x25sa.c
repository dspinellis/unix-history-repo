/*
 * Print the kernel X.25 accounting file, in the following format:
 *  month
 *  day
 *  hour:min	- call start time
 *  duration	- call duration in seconds
 *  user	- name of user who placed the call
 *  in/out	- incoming or outgoing call flag
 *  address	- X.121 address
 *  host	- host name associated with address, if known
 *  rev/---	- reverse charge flag
 *  packet size	- packet size in bytes
 *  user data	- 4 bytes of user data (protocol),
 * 		  ITI calls appear as x29d, EAN 1&2 as ean1/ean2,
 * 		  others as 4 hex digits.
 *  packets sent
 *  packets received - number of packets sent and received.
 *
 * Not the nicest format for people, but it is assumed that the
 * output will be further processed before people see it.
 * The idea here is to convert the raw data to a form
 * convenient for shell or awk scripts.
 */
#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/time.h>
#include <pwd.h>
#include <netccitt/x25.h>
#include <netccitt/x25acct.h>
#include <netdb.h>

main(argc, argv) 
	char	**argv;
{
	struct passwd *pw;
	char *user, ubuf[20], *dayt, *filename;
	FILE *fp;
	struct x25acct ac;
	char addr[16+1];
	char *format_udata();
	struct hostent *hp, *getx25hostbyaddr();
	
	if (argc > 2) {
		fprintf(stderr, "usage: %s [file]\n", argv[0]);
		exit(1);
	}
	filename = argc == 2 ? argv[1] : X25ACCTF;
	if ((fp = fopen(filename, "r")) == NULL) {
		fprintf(stderr, "%s: ", argv[0]);
		perror(filename);
		exit(1);
	}
	setx25hostent(1);	/* don't close after each call */
	while (fread((char *)&ac, sizeof ac, 1, fp) == 1) {
		if (ac.x25acct_txcnt == 0 && ac.x25acct_rxcnt == 0)
			continue;
		if ((pw = getpwuid(ac.x25acct_uid)) == NULL) {
			fprintf(stderr, "%s: unknown uid: %d\n",
				argv[0], ac.x25acct_uid);
			sprintf(ubuf, "#%d", ac.x25acct_uid);
			user = ubuf;
		}
		else
			user = pw->pw_name;
		dayt = ctime(&ac.x25acct_stime);
		dayt[16] = '\0'; /* we probably know what year it is */
		dayt += 4;	/* skip the day */

		if (ac.x25acct_addrlen > 16) {
			fprintf(stderr, "%s: Invalid addrlen %d\n",
				argv[0], ac.x25acct_addrlen);
			continue;
		}
		/*
		 * Convert bcd address to ascii.
		 */
		bzero(addr, sizeof addr);
		from_bcd(addr, (u_char *)ac.x25acct_addr,
			(int)ac.x25acct_addrlen);
		hp = getx25hostbyaddr(addr);
		
		printf("%s %4d %-8s %s %-14s %-9s %s %3d %-4s %3ld %3ld\n",
			dayt,
			ac.x25acct_etime,
			user,
			ac.x25acct_callin ? "in " : "out",
			addr,
			hp ? hp->h_name : addr,
			/* REVERSE means collect (by default caller pays) */
			ac.x25acct_revcharge ? "rev" : "---",
			1 << ac.x25acct_psize,
			format_udata(ac.x25acct_udata),
			ac.x25acct_txcnt,
			ac.x25acct_rxcnt);
	}
	exit(0);
}

	char *
format_udata(udata)
	char *udata;
{
	static char buf[15];

	if (bcmp(udata, "\1\0\0\0", 4) == 0)
		return "x29d";
	if (bcmp(udata, "ean", 3) == 0)
		return udata;
	sprintf(buf, "%x.%x.%x.%x", 
		udata[0], udata[1], udata[2], udata[3]);
	return buf;
}

from_bcd (a, x, len)
	register char *a;
	register u_char *x;
	register int len;
{
	register int posn = 0;

	while (--len >= 0) {
		if (posn++ & 0x01)
			*a = *x++ & 0x0f;
		else
			*a = (*x >> 4) & 0x0f;
		*a++ |= 0x30;
	}
}
