struct buf {
	int	b_flags;
	struct	buf *b_forw;
	struct	buf *b_back;
	struct	buf *av_forw;
	struct	buf *av_back;
	int	b_dev;
	int	b_wcount;
	char	*b_addr;
	char	*b_blkno;
	char	b_error;
	char	*b_resid;
} buf[NBUF];

/*
 * forw and back are shared with "buf" struct.
 */
struct devtab {
	char	d_active;
	char	d_errcnt;
	struct	buf *b_forw;
	struct	buf *b_back;
	struct	buf *d_actf;
	struct 	buf *d_actl;
};

struct	buf bfreelist;

#define	B_WRITE	0
#define	B_READ	01
#define	B_DONE	02
#define	B_ERROR	04
#define	B_BUSY	010
#define	B_XMEM	060
#define	B_WANTED 0100
#define	B_RELOC	0200
#define	B_ASYNC	0400
#define	B_DELWRI 01000
