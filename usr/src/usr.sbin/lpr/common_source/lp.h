/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lp.h	5.6 (Berkeley) %G%
 */

/*
 * Global definitions for the line printer system.
 */

extern int	DU;		/* daeomon user-id */
extern int	MX;		/* maximum number of blocks to copy */
extern int	MC;		/* maximum number of copies allowed */
extern char	*LP;		/* line printer device name */
extern char	*RM;		/* remote machine name */
extern char	*RG;		/* restricted group */
extern char	*RP;		/* remote printer name */
extern char	*LO;		/* lock file name */
extern char	*ST;		/* status file name */
extern char	*SD;		/* spool directory */
extern char	*AF;		/* accounting file */
extern char	*LF;		/* log file for error messages */
extern char	*OF;		/* name of output filter (created once) */
extern char	*IF;		/* name of input filter (created per job) */
extern char	*RF;		/* name of fortran text filter (per job) */
extern char	*TF;		/* name of troff(1) filter (per job) */
extern char	*NF;		/* name of ditroff(1) filter (per job) */
extern char	*DF;		/* name of tex filter (per job) */
extern char	*GF;		/* name of graph(1G) filter (per job) */
extern char	*VF;		/* name of raster filter (per job) */
extern char	*CF;		/* name of cifplot filter (per job) */
extern char	*FF;		/* form feed string */
extern char	*TR;		/* trailer string to be output when Q empties */
extern short	SC;		/* suppress multiple copies */
extern short	SF;		/* suppress FF on each print job */
extern short	SH;		/* suppress header page */
extern short	SB;		/* short banner instead of normal header */
extern short	HL;		/* print header last */
extern short	RW;		/* open LP for reading and writing */
extern short	PW;		/* page width */
extern short	PX;		/* page width in pixels */
extern short	PY;		/* page length in pixels */
extern short	PL;		/* page length */
extern short	BR;		/* baud rate if lp is a tty */
extern int	FC;		/* flags to clear if lp is a tty */
extern int	FS;		/* flags to set if lp is a tty */
extern int	XC;		/* flags to clear for local mode */
extern int	XS;		/* flags to set for local mode */
extern short	RS;		/* restricted to those with local accounts */

extern char	line[BUFSIZ];
extern char	pbuf[];		/* buffer for printcap entry */
extern char	*bp;		/* pointer into ebuf for pgetent() */
extern char	*name;		/* program name */
extern char	*printer;	/* printer name */
				/* host machine name */
extern char	host[MAXHOSTNAMELEN];
extern char	*from;		/* client's machine name */
extern int	sendtorem;	/* are we sending to a remote? */

/*
 * Structure used for building a sorted list of control files.
 */
struct queue {
	time_t	q_time;			/* modification time */
	char	q_name[MAXNAMLEN+1];	/* control file name */
};

#include <sys/cdefs.h>
struct dirent;

void     blankfill __P((int));
char	*checkremote __P((void));
int      chk __P((char *));
void     displayq __P((int));
void     dump __P((char *, char *, int));
void     endprent __P((void));
void	 fatal __P((const char *, ...));
int	 getline __P((FILE *));
int	 getport __P((char *));
int      getprent __P((char *));
int	 getq __P((struct queue *(*[])));
void     header __P((void));
void     inform __P((char *));
int      inlist __P((char *, char *));
int      iscf __P((struct dirent *));
int      isowner __P((char *, char *));
void     ldump __P((char *, char *, int));
int      lockchk __P((char *));
void     prank __P((int));
void     process __P((char *));
void     rmjob __P((void));
void     rmremote __P((void));
void     show __P((char *, char *, int));
int      startdaemon __P((char *));
int      pgetent __P((char *, char *));
int      pgetflag __P((char *));
int      pgetnum __P((char *));
char    *pgetstr __P((char *, char **));
int      pnamatch __P((char *));
int      pnchktc __P((void));
void     warn __P((void));
