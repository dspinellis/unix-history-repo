/*	cpmfio.h	1.5	83/05/13	*/

#define C_NFILE		5	/* max number of concurrently open cp/m files */

typedef struct	c_iobuf {
	int	c_cnt;		/* bytes left in buffer */
	int	c_blk;		/* block number within the current extent */
				/* (starting at 0) */
	int	c_seccnt;	/* number of physical sectors left in */
				/* the current extent */
	int	c_ext;		/* current extent's directory index */
	int	c_extno;	/* extent number within current file */
	char	*c_buf;		/* next character position */
	char	*c_base;	/* location of buffer */
	short	c_flag;		/* access mode (READ or WRITE) */
	struct directory *c_dirp;	/* pointer to the current */
					/* extent's directory entry */
}	C_FILE;
extern	C_FILE	c_iob[C_NFILE];

#define c_getc(p)	(--(p)->c_cnt>=0 ? *(p)->c_buf++&0377 : c_fillbuf(p))
#define c_putc(x,p)	(--(p)->c_cnt>=0 ? ((int)(*(p)->c_buf++=(unsigned)(x))) : c_flsbuf((unsigned)(x), p))

C_FILE	*c_open(), *c_creat();

#define READ	0x01
#define WRITE	0x02
#define RW	0x03
#define MODFLG	0x08
#define BINARY	0x10
