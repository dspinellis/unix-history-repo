/*  $Revision: 1.2 $
**
**  Quick I/O package -- optimized for reading through a file.
*/


/*
**  State for a quick open file.
*/
typedef struct _QIOSTATE {
    int		fd;
    int		Size;
    int		flag;
    int		Length;
    char	*Buffer;
    char	*End;
    char	*Start;
    long	Count;
} QIOSTATE;

    /* A reasonable buffersize to use. */
#define QIO_BUFFER	8192

    /* Values for QIOstate.flag */
#define QIO_ok		0
#define QIO_error	1
#define QIO_long	2

#define QIOerror(qp)		((qp)->flag > 0)
#define QIOtoolong(qp)		((qp)->flag == QIO_long)
#define QIOtell(qp)		((qp)->Count - ((qp)->End - (qp)->Start))
#define QIOlength(qp)		((qp)->Length)
#define QIOfileno(qp)		((qp)->fd)

extern char	*QIOread();
extern QIOSTATE	*QIOopen();
extern QIOSTATE	*QIOfdopen();
extern void	QIOclose();
extern int	QIOrewind();
