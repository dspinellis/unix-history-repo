/*-
 * Copyright (c) 1992 Diomidis Spinellis.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Diomidis Spinellis of Imperial College, University of London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)defs.h	5.5 (Berkeley) %G%
 */

/*
 * Types of address specifications
 */
enum e_atype {
	AT_RE,					/* Line that match RE */
	AT_LINE,				/* Specific line */
	AT_LAST,				/* Last line */
};

/*
 * Format of an address
 */
struct s_addr {
	enum e_atype type;			/* Address type */
	union {
		u_long l;			/* Line number */
		regex_t *r;			/* Regular expression */
	} u;
};

/*
 * Substitution command
 */
struct s_subst {
	int n;					/* Occurrence to subst. */
	int p;					/* True if p flag */
	char *wfile;				/* NULL if no wfile */
	int wfd;				/* Cached file descriptor */
	regex_t *re;				/* Regular expression */
	int maxbref;				/* Largest backreference. */
	u_long linenum;				/* Line number. */
	char *new;				/* Replacement text */
};


/*
 * An internally compiled command.
 * Initialy, label references are stored in t, on a second pass they
 * are updated to pointers.
 */
struct s_command {
	struct s_command *next;			/* Pointer to next command */
	struct s_addr *a1, *a2;			/* Start and end address */
	char *t;				/* Text for : a c i r w */
	union {
		struct s_command *c;		/* Command(s) for b t { */
		struct s_subst *s;		/* Substitute command */
		u_char *y;			/* Replace command array */
		int fd;				/* File descriptor for w */
	} u;
	char code;				/* Command code */
	u_int nonsel:1;				/* True if ! */
	u_int inrange:1;			/* True if in range */
};

/*
 * Types of command arguments recognised by the parser
 */
enum e_args {
	EMPTY,			/* d D g G h H l n N p P q x = \0 */
	TEXT,			/* a c i */
	NONSEL,			/* ! */
	GROUP,			/* { */
	COMMENT,		/* # */
	BRANCH,			/* b t */
	LABEL,			/* : */
	RFILE,			/* r */
	WFILE,			/* w */
	SUBST,			/* s */
	TR			/* y */
};

/*
 * Structure containing things to append before a line is read
 */
struct s_appends {
	enum {AP_STRING, AP_FILE} type;
	char *s;
	size_t len;
};

enum e_spflag {
	APPEND,					/* Append to the contents. */
	REPLACE,				/* Replace the contents. */
};

/*
 * Structure for a space (process, hold, otherwise).
 */
typedef struct {
	char *space;		/* Current space pointer. */
	size_t len;		/* Current length. */
	int deleted;		/* If deleted. */
	char *back;		/* Backing memory. */
	size_t blen;		/* Backing memory length. */
} SPACE;

/*
 * Error severity codes:
 */
#define	FATAL		0	/* Exit immediately with 1 */
#define	ERROR		1	/* Continue, but change exit value */
#define	WARNING		2	/* Just print the warning */
#define	COMPILE		3	/* Print error, count and finish script */
#define	COMPILE2	3	/* Print error, count and finish script */
