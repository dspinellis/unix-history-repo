/*	pup.h	1.1	82/02/15	*/

/*
 * PUP port addressing.
 */
struct pupport {
	u_char	pp_net;
	u_char	pp_host;
	u_long	pp_socket;
}

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
