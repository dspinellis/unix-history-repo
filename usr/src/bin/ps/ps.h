/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ps.h	8.1 (Berkeley) %G%
 */

#define	UNLIMITED	0	/* unlimited terminal width */
enum type { CHAR, UCHAR, SHORT, USHORT, LONG, ULONG, KPTR };

struct usave {
	struct	timeval u_start;
	struct	rusage u_ru;
	struct	rusage u_cru;
	char	u_acflag;
	char	u_valid;
};

#define KI_PROC(ki) (&(ki)->ki_p->kp_proc)
#define KI_EPROC(ki) (&(ki)->ki_p->kp_eproc)

typedef struct kinfo {
	struct kinfo_proc *ki_p;	/* proc structure */
	struct usave ki_u;	/* interesting parts of user */
	char *ki_args;		/* exec args */
	char *ki_env;		/* environment */
} KINFO;

/* Variables. */
typedef struct varent {
	struct varent *next;
	struct var *var;
} VARENT;

typedef struct var {
	char	*name;		/* name(s) of variable */
	char	*header;	/* default header */
	char	*alias;		/* aliases */
#define	COMM	0x01		/* needs exec arguments and environment (XXX) */
#define	LJUST	0x02		/* left adjust on output (trailing blanks) */
#define	USER	0x04		/* needs user structure */
	u_int	flag;
				/* output routine */
	void	(*oproc) __P((struct kinfo *, struct varent *));
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
} VAR;

#include "extern.h"
