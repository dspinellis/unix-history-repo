/*	pup.h	6.1	83/07/29	*/

struct	pupport {
	u_char	pup_net;
	u_char	pup_host;
	u_char	pup_socket[4];
};

/*
 * PUP header.
 */
struct pup_header {
	u_short	pup_length;
	u_char	pup_tcontrol;		/* transport control */
	u_char	pup_type;		/* protocol type */
	u_long	pup_id;			/* used by protocols */
	u_char	pup_dnet;		/* destination */
	u_char	pup_dhost;
	u_char	pup_dsock[4];
	u_char	pup_snet;		/* source */
	u_char	pup_shost;
	u_char	pup_ssock[4];
};

#define	PUP_TRACE	01		/* trace pup in network */

#define	MINPUPSIZ	(sizeof (struct pup_header) + sizeof (short))
#define	MAXPUPDATA	532
#define	MAXPUPSIZ	(MINPUPSIZ + MAXPUPDATA)

#define	PUP_NOCKSUM	0xffff		/* no checksum supplied */

/*
 * A sockaddr, as seen through the eyes of PUP.
 */
struct sockaddr_pup {
	short	spup_family;
	short	spup_zero1;
	u_char	spup_net;
	u_char	spup_host;
	u_char	spup_sock[4];
	char	spup_zero2[4];
};
