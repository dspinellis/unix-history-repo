/*      sp.h     6.1     85/05/30     */

/*
 * Definitions for Xerox NS style sequenced packet protocol
 */

struct sphdr {
	u_char	sp_cc;		/* connection control */
	u_char	sp_dt;		/* datastream type */
#define	SP_SP	0x80		/* system packet */
#define	SP_SA	0x40		/* send acknowledgement */
#define	SP_OB	0x20		/* attention (out of band data) */
#define	SP_EM	0x10		/* end of message */
	u_short	sp_sid;		/* source connection identifier */
	u_short	sp_did;		/* destination connection identifier */
	u_short	sp_seq;		/* sequence number */
	u_short	sp_ack;		/* acknowledge number */
	u_short	sp_alo;		/* allocation number */
};
