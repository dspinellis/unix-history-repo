/*	ttyent.h	4.1	84/04/27	*/

struct	ttyent { /* see getttyent(3) */
	char	*ty_name;	/* terminal device name */
	char	*ty_getty;	/* name for gettytab (5) */
	char	*ty_type;	/* terminal type for termcap (3X) */
	int	ty_status;	/* status flags (see below for defines) */
	char	*ty_comment;	/* unused - for comments */
};

#define TTY_ON		0x1	/* enable logins (startup getty) */
#define TTY_SECURE	0x2	/* allow root to login */

extern struct ttyent *getttyent();
extern struct ttyent *getttynam();
