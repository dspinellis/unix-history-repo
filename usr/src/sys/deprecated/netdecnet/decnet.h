/*	decnet.h	1.3	82/10/09	*/

/*
 * DECnet address (should be expanded for Phase 3E)
 */
struct dn_addr {
	u_short	s_host;		/* remote host address */
};

/*
 * Socket address, DECnet style.
 */
struct sockaddr_dn {
	short	sdn_family;		/* AF_DECNET */
	u_short	sdn_port;		/* local port number */
	struct	dn_addr sdn_addr;	/* remote host/port address */
	char	sdn_zero[];
};
