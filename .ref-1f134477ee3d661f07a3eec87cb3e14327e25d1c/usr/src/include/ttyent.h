/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ttyent.h	8.1 (Berkeley) %G%
 */

#ifndef	_TTYENT_H_
#define	_TTYENT_H_

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

#include <sys/cdefs.h>

__BEGIN_DECLS
struct ttyent *getttyent __P((void));
struct ttyent *getttynam __P((const char *));
int setttyent __P((void));
int endttyent __P((void));
__END_DECLS

#endif /* !_TTYENT_H_ */
