/*	pup.h	4.3	82/03/13	*/

/*
 * PUP port addressing.
 */
struct pupport {
	u_char	pp_net;
	u_char	pp_host;
	u_long	pp_socket;
};

/*
 * PUP header.
 */
struct pup_header {
	u_short	pup_length;
	u_char	pup_tcontrol;		/* transport control */
	u_char	pup_type;		/* protocol type */
	u_long	pup_id;			/* used by protocols */
	struct	pupport pup_dport, pup_sport;
#define	pup_dnet	pup_dport.pp_net
#define	pup_dhost	pup_dport.pp_dhost
#define	pup_dsocket	pup_dport.pp_socket
#define	pup_snet	pup_sport.pp_net
#define	pup_shost	pup_sport.pp_host
#define	pup_ssocket	pup_sport.pp_socket
};

#define	PUP_TRACE	01		/* trace pup in network */

/*
 * A sockaddr, as seen through the eyes of PUP.
 */
struct sockaddr_pup {
	short	spup_family;
	short	spup_zero1;
	struct	pupport spup_addr;
	char	spup_zero2[4];
};

#define	sp_net		spup_addr.pp_net
#define	sp_host		spup_addr.pp_host
#define	sp_socket	spup_addr.pp_port
