/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ttyent.h	5.5 (Berkeley) %G%
 */

#define	_PATH_TTYS	"/etc/ttys"

#define	_TTYS_OFF	"off"
#define	_TTYS_ON	"on"
#define	_TTYS_SECURE	"secure"
#define	_TTYS_WINDOW	"window"

struct ttyent {
	char	*ty_name;	/* terminal device name */
	char	*ty_getty;	/* command to execute, usually getty */
	char	*ty_type;	/* terminal type for termcap */
#define	TTY_ON		0x01	/* enable logins (start ty_getty program) */
#define	TTY_SECURE	0x02	/* allow uid of 0 to login */
	int	ty_status;	/* status flags */
	char 	*ty_window;	/* command to start up window manager */
	char	*ty_comment;	/* comment field */
};

#if __STDC__ || c_plusplus
extern struct ttyent *getttyent(void);
extern struct ttyent *getttynam(const char *);
extern int setttyent(void);
extern int endttyent(void);
#else
extern struct ttyent *getttyent();
extern struct ttyent *getttynam();
extern int setttyent();
extern int endttyent();
#endif
