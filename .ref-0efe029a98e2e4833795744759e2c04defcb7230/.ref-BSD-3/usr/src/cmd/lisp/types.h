typedef	struct { int rrr[1]; } *	physadr;
typedef	long		daddr_t;
typedef char *		caddr_t;
typedef	unsigned short	ino_t;
typedef	long		time_t;
typedef	int		label_t[10];
typedef	short		dev_t;
typedef	long		off_t;
/* major part of a device */
#define	major(x)	(int)(((unsigned)x>>8)&0377)

/* minor part of a device */
#define	minor(x)	(int)(x&0377)

/* make a device number */
#define	makedev(x,y)	(dev_t)(((x)<<8) | (y))
