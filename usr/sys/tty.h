struct clist {
	int	c_cc;
	int	c_cf;
	int	c_cl;
};

struct tty {
	struct	clist t_rawq;
	struct	clist t_canq;
	struct	clist t_outq;
	int	t_flags;
	int	*t_addr;
	char	t_delct;
	char	t_col;
	char	t_erase;
	char	t_kill;
	char	t_state;
	char	t_char;
	int	t_speeds;
};

char partab[];

#define	TTIPRI	10
#define	TTOPRI	20
#define	CERASE	'#'
#define	CEOT	004
#define	CKILL	'@'
#define	CQUIT	034	/* FS, cntl shift L */
#define	CINTR	0177	/* DEL */

/* limits */
#define	TTHIWAT	50
#define	TTLOWAT	30
#define	TTYHOG	256

/* modes */
#define	NODELAY	01
#define	XTABS	02
#define	LCASE	04
#define	ECHO	010
#define	CRMOD	020
#define	RAW	040
#define	ODDP	0100
#define	EVENP	0200
#define	HUPCL	0400
#define	NTDELAY	010000

/* Bits */
#define	DONE	0200
#define	IENABLE	0100
#define	TIMEOUT	01
#define	WOPEN	02
#define	ISOPEN	04
#define	SSTART	010
#define	CARR_ON	020
#define	BUSY	040
