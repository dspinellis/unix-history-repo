struct pack {
	short	p_state;	/* line state */
	short	p_bits;		/* mask for getepack */
	short	p_psize;	/* packet size */
	short	p_icount;	/* input byte count */
	char	p_ostate;	/* output state */
	char	p_istate;	/* input state */
	char	p_msgs;		/* # cntl msgs sent */
	char	p_window;	/* window size */
	char	p_iseq;		/* input SEQ byte */
	char	p_oseq;		/* output SEQ byte */
	char	p_msg;		/* control msg */
	char	p_msg2;		/* extra msg bits */
	char	p_ps,p_pr;	/* last packet sent, recv'd */
	char	p_rps,p_rpr;	/* remote (received) ps and pr */
	char	p_nxtps;	/* next output seq number */
	char	p_nxtpr;	/* expected input seq number */
	char	p_prcopy;	/* oldest received packet in kernel */
	char	p_pscopy;	/* newest output packet */
	char	*p_input;	/* points into input buffer */
	char	p_iobuf[6];	/* cntl packet buffer */
	char	*p_ob[8];	/* output buffers */
	char	*p_ib[8];	/* input buffers */
	char	p_bstate[8];	/* output buffer status */
	char	p_cstate[8];	/* input buffer status */
	short	p_checks[8];
	short	p_rcheck;
	struct tty *p_ttyp;
	int	p_timer;
	int	xcount,rcount;
};
#define	NPSTRUCT	(1+(sizeof(struct pack))/64)
#define	NPERBUF		(8/NPSTRUCT)
#define	CHECK	0125252

int	npbits;

/*
 * driver state
 */
#define	DEAD	0
#define	INITa	1
#define	INITb	2
#define	ISINIT	3
#define	LIVE	4
#define	RESYNC	010
#define	DOWN	020
#define	RREJ	040
#define	RXMIT	0100
#define PDEBUG	0200
#define	DRAINO	0400
#define	WAITO	01000

/*
 * input side states
 */
#define	P_IDLE	1
#define	P_SCAN	2
#define	P_SEQ1	3
#define	P_PACK	4
#define	P_CHK1	5
#define	P_CHK2	6
#define	P_SEQ2	7
#define	P_FLUSH	8

#define	SYN	026

/*
 * output side states
 */
#define	T_IDLE	0
#define	T_READY	1
#define	T_HDR	2
#define	T_PACK	3
#define	T_TAIL	4

/*
 * io buffer states
 */
#define	B_FREE	0
#define	B_READY	1
#define	B_SENT	2
#define	B_PACK	4
#define	B_COPY	010
#define	B_XMIT	020

/*
 * frame control messages
 */
#define	RR	1
#define	RWR	2
#define	REJ	3
#define	CLOSE	4
#define	RESET	6
#define	INIT	7
#define	UA	8




#define	PKOPRI	31
#define	PKIPRI	30

#define	NPLINES	8

/*
 * packet ioctl buf
 */
struct	piocb {
	unsigned t;
	short	psize;
	short	mode;
	short	state;
	char	window;
};
