/*	udp.h	4.1	81/11/14	*/

struct udpiphdr {
	struct	ipovly u_ip;

	struct	udpiphdr *u_x[2];	/* space holders */
	u_char	u_x1;			/* unused */
	u_char	u_pr;			/* protocol */

