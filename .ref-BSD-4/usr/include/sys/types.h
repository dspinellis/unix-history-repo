typedef	struct _physadr { int r[1]; } *physadr;
typedef	long		daddr_t;
typedef	char *		caddr_t;
typedef	unsigned short	ino_t;
typedef	int		swblk_t;
typedef	int		size_t;
typedef	long		time_t;
typedef	long		label_t[14];
typedef	short		dev_t;
typedef	long		off_t;

typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;

/* major part of a device */
#define	major(x)	((int)(((unsigned)(x)>>8)&0377))

/* minor part of a device */
#define	minor(x)	((int)((x)&0377))

/* make a device number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))
