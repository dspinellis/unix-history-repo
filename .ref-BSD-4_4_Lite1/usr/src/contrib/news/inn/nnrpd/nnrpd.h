/*  $Revision: 1.15 $
**
**  Net News Reading Protocol server.
*/
#include "configdata.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include <sys/file.h>
#if	defined(VAR_VARARGS)
#include <varargs.h>
#endif	/* defined(VAR_VARARGS) */
#if	defined(VAR_STDARGS)
#include <stdarg.h>
#endif	/* defined(VAR_STDARGS) */
#include <syslog.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "paths.h"
#include "nntp.h"
#include "logging.h"
#include "libinn.h"
#include "clibrary.h"
#include "qio.h"
#include "macros.h"


/*
**  Maximum input line length, sigh.
*/
#define ART_LINE_LENGTH		1000
#define ART_LINE_MALLOC		1024


/*
**  Some convenient shorthands.
*/
typedef struct in_addr	INADDR;
#define Printf		(void)printf
#if	defined(VAR_NONE)
#define Reply		(void)printf
#endif	/* defined(VAR_NONE) */


/*
**  The XTHREAD command is too ugly to talk about, but if you want it
**  change DONT_DO_XTRHEAD to DO_DO_XTRHEAD, below.
*/
#define DONT_DO_XTHREAD
#define THREAD_NAMES_FLAT
#define THREAD_SUFFIX		""
#define THREAD_DB		"/usr/spool/news/trn.threads/db.init"
#define THREAD_DIR		"/usr/spool/news/trn.threads"
#define THREAD_NNTP_CODE	288


/*
**  A group entry.
*/
typedef struct _GROUPENTRY {
    char	*Name;
    ARTNUM	High;
    ARTNUM	Low;
    char	Flag;
    char	*Alias;
} GROUPENTRY;


/*
**  A range of article numbers.
*/
typedef struct _ARTRANGE {
    int		Low;
    int		High;
} ARTRANGE;


/*
**  What READline returns.
*/
typedef enum _READTYPE {
    RTeof,
    RTok,
    RTlong,
    RTtimeout
} READTYPE;


#if	defined(MAINLINE)
#define EXTERN	/* NULL */
#else
#define EXTERN	extern
#endif	/* defined(MAINLINE) */

EXTERN BOOL	PERMauthorized;
EXTERN BOOL	PERMcanpost;
EXTERN BOOL	PERMcanread;
EXTERN BOOL	PERMneedauth;
EXTERN BOOL	PERMspecified;
EXTERN BOOL	Tracing;
EXTERN char	**PERMlist;
EXTERN STRING	MyHostName;
extern char	ACTIVE[];
EXTERN char	ClientHost[SMBUF];
extern char	ACTIVETIMES[];
extern char	HISTORY[];
extern char	NEWSGROUPS[];
extern char	NOACCESS[];
EXTERN char	PERMpass[20];
EXTERN char	PERMuser[20];
EXTERN char	*RemoteMaster;
EXTERN ARTNUM	*ARTcache;
EXTERN ARTNUM	*ARTnumbers;
EXTERN int	ARTindex;
EXTERN int	ARTsize;
extern int	PERMdefault;
EXTERN long	ARTcount;
EXTERN long	GRParticles;
EXTERN long	GRPcount;
EXTERN char	GRPlast[SPOOLNAMEBUFF];
EXTERN long	POSTreceived;
EXTERN long	POSTrejected;


#if	NNRP_LOADLIMIT > 0
extern int		GetLoadAverage();
#endif	/* NNRP_LOADLIMIT > 0 */
extern STRING		ARTpost();
extern void		ARTclose();
extern void		ARTreadschema();
extern char		*Glom();
extern int		Argify();
extern NORETURN		ExitWithStats();
extern BOOL		GetGroupList();
extern void		GRPreport();
extern GROUPENTRY	*GRPfind();
extern char		*HISgetent();
extern long		LOCALtoGMT();
extern BOOL		NGgetlist();
extern long		NNTPtoGMT();
extern BOOL		PERMartok();
extern BOOL		PERMinfile();
extern BOOL		PERMmatch();
extern BOOL		ParseDistlist();
extern READTYPE		READline();
extern void		OVERclose();
#if	defined(VAR_STDARGS)
extern void		Reply(char *, ...);
#endif	/* defined(VAR_STDARGS) */
#if	defined(VAR_VARARGS)
extern void		Reply();
#endif	/* defined(VAR_VARARGS) */
