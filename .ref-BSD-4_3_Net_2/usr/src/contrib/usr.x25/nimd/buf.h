#define FLAG(bp)	(bp)->b_data[0]
#define ISEMPTY(bp)	((bp)->b_top == (bp)->b_bot)
#define RESET(bp)	(bp)->b_top = (bp)->b_bot = (bp)->b_data
#define QEMPTY(qp)	((qp)->b_next == (struct buf *)qp)
#define SIZE(bp)	((bp)->b_top - (bp)->b_bot)
#define GETCHAR(bp)	(*(bp)->b_bot++ & 0377)
#define PUTCHAR(c, bp)	*(bp)->b_top++ = c
#define UNGETC(c, bp)	*--(bp)->b_bot = c
#define BUFCOPY(f, t)	bcopy((f)->b_bot, (t)->b_top, SIZE(f)); (t)->b_top+=SIZE(f);
#define STRTOBUF(s, bp)	{ register char *sp=s; register int l=strlen(s); \
			bcopy(s, (bp)->b_top, l); (bp)->b_top += l;}

struct	bufhd {		/* buffer header; b_prev and b_next must be first */
	struct	buf *b_prev, *b_next;
	short	b_count;	/* total number of bytes of data queued */
};

struct	buf {
	struct	buf *b_prev, *b_next;	/* previous and next buffers */
	char	*b_bot;		/* start of useful data */
	char	*b_top;		/* current position in data */
	char	b_data[1];	/* usually more than 1 byte */
} ;

struct	buf *getbuf(), *FillBuf();
