/* mshsbr.h - definitions for msh */
/* @(#)$Id: mshsbr.h,v 1.6 1993/08/25 17:16:28 jromine Exp $ */

struct Cmd {
    char    line[BUFSIZ];
    char   *args[MAXARGS];

    char   *redirect;

    int     direction;
#define	STDIO	0		/* regular stdoutput */
#define	CRTIO	1		/* create  re-direct */
#define	APPIO	2		/* append  re-direct */
#define	PIPIO	3		/* pipe    re-direct */

    FILE   *stream;
};
#define	NULLCMD	((struct Cmd *) 0)


struct Msg {
    struct drop m_drop;
#define	m_bboard_id	m_drop.d_id
#define	m_top		m_drop.d_size
#define	m_start		m_drop.d_start
#define	m_stop		m_drop.d_stop

    char   *m_scanl;
    
    struct tws  m_tb;

    short   m_flags;
#define	MHNCHK	0x0001	/* did nontext check */
#define	MHNYES	0x0002	/*   .. and known to be non-text */

    short   m_stats;
#define CUR (1 << (FFATTRSLOT + NATTRS))
#ifdef	BPOP
#define	VIRTUAL	SELECT_EMPTY
#endif /* BPOP */
};

/*  */

				/* FOLDER */
extern char  *fmsh;		/* folder instead of file */
extern int    modified;		/* command modified folder */
extern struct msgs *mp;		/* used a lot */
extern struct Msg  *Msgs;	/* Msgs[0] not used */

FILE  *msh_ready ();


				/* COMMAND */
extern int  interactive;	/* running from a /dev/tty */
extern int  redirected;		/* re-directing output */
extern  FILE  *sp;		/* original stdout */

extern char *cmd_name;		/* command being run */

extern char myfilter[];		/* path to mhl.forward */



extern char *BBoard_ID;		/* BBoard-ID constant */


				/* SIGNALS */
extern TYPESIG  (*istat) ();	/* original SIGINT */
extern TYPESIG  (*qstat) ();	/* original SIGQUIT */
extern int  interrupted;	/* SIGINT detected */
extern int  broken_pipe;	/* SIGPIPE detected */
extern int  told_to_quit;	/* SIGQUIT detected */

#ifdef	BSD42
extern int  should_intr;	/* signal handler should interrupt call */
extern jmp_buf sigenv;		/* the environment pointer */
#endif /* BSD42 */


off_t    lseek ();
