/**************************************************************************/
/*           structures internal to the HMP implementation                */
/**************************************************************************/

struct hmp_hdr		/* HMP packet header */
{
    u_char hh_systyp;	/* system type */
    u_char hh_msgtyp;	/* message type */
    u_char hh_port;	/* application port # */
    u_char hh_ctlflg;	/* control flag */
    u_short hh_seq;	/* sequence # */
    u_short hh_passwd;	/* remote/local passwd */
    u_short hh_cksum;	/* checksum */
};

#define HMPIPHDR (sizeof(struct ip)+sizeof(struct hmp_hdr))

struct hmpcb 		/* HMP pcb */
{
    struct inpcb *hp_inpcb;	/* pointer to inpcb */
    u_char hp_flags;		/* flags */
    u_char hp_lsystyp;		/* local system type */
    u_char hp_rsystyp;		/* remote system type */
    u_char hp_lmsgtyp;		/* local message type */
    u_char hp_rmsgtyp;		/* remote message type */
    u_char hp_ctlflg;		/* control flags */
    u_short hp_lseq;		/* local sequence number */
    u_short hp_rseq;		/* remote sequence number */
    u_short hp_lpasswd;		/* local password */
    u_short hp_rpasswd;		/* remote password */
};

struct hmp_stat
{
    struct in_stat h_in;
#define h_total		h_in.in_total
#define h_badsum	h_in.in_badsum
#define h_tooshort	h_in.in_tooshort
#define h_drops		h_in.in_drops
};

#define intohmpcb(i)	((struct hmpcb *) ((i)->inp_ppcb))

#ifdef KERNEL
extern struct inpcb hmp;
#endif
