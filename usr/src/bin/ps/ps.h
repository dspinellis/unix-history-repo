/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ps.h	5.1 (Berkeley) %G%
 */

#define	UNLIMITED	0	/* unlimited terminal width */
enum type { CHAR, UCHAR, SHORT, USHORT, LONG, ULONG, KPTR };

struct usave {
	struct	proc *u_procp;
	struct	timeval u_start;
	struct	rusage u_ru;
	struct	rusage u_cru;
	char	u_acflag;
};

typedef struct _kinfo {
	struct proc *ki_p;	/* proc structure */
	struct eproc *ki_e;	/* extra stuff */
	struct usave *ki_u;	/* interesting parts of user */
	char *ki_args;		/* exec args (should be char **) */
	char *ki_env;		/* environment (should be char **) */
} KINFO;

/* Variables. */
typedef struct _var {
	char	*name;		/* name(s) of variable */
	char	*header;	/* default header */
	char	*alias;		/* aliases */
#define	COMM	0x01		/* needs exec arguments and environment (XXX) */
#define	LJUST	0x02		/* left adjust on output (trailing blanks) */
#define	USER	0x04		/* needs user structure */
	u_int	flag;
	int	(*oproc)();	/* output routine */
	short	width;		/* printing width */
	/*
	 * The following (optional) elements are hooks for passing information
	 * to the generic output routines: pvar, evar, uvar (those which print
	 * simple elements from well known structures: proc, eproc, usave)
	 */
	int	off;		/* offset in structure */
	enum	type type;	/* type of element */
	char	*fmt;		/* printf format */
	char	*time;		/* time format */
	/*
	 * glue to link selected fields together
	 */
	struct _var *next;
} VAR;

extern VAR var[], *vhead, *vtail;
