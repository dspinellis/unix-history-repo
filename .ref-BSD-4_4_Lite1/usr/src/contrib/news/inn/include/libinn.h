/*  $Revision: 1.14 $
**
**  Here be declarations of functions in the InterNetNews library.
*/

/* Memory allocation. */
    /* Worst-case alignment, in order to shut lint up. */
    /* =()<typedef @<ALIGNPTR>@	*ALIGNPTR;>()= */
typedef int	*ALIGNPTR;
extern ALIGNPTR	xmalloc();
extern ALIGNPTR	xrealloc();

/* Headers. */
extern char	*GenerateMessageID();
extern char	*HeaderFind();
extern void	HeaderCleanFrom();

extern struct _DDHANDLE	*DDstart();
extern void		DDcheck();
extern char		*DDend();

/* NNTP functions. */
extern int	NNTPlocalopen();
extern int	NNTPremoteopen();
extern int	NNTPconnect();
extern int	NNTPsendarticle();
extern int	NNTPsendpassword();

/* Opening the active file on a client. */
extern FILE	*CAopen();
extern FILE	*CAlistopen();
extern void	CAclose();

/* Parameter retrieval. */
extern char	*GetFQDN();
extern char	*GetConfigValue();
extern char	*GetFileConfigValue();
extern char	*GetModeratorAddress();

/* Time functions. */
typedef struct _TIMEINFO {
    time_t	time;
    long	usec;
    long	tzone;
} TIMEINFO;
extern time_t	parsedate();
extern int	GetTimeInfo();

/* Miscellaneous. */
extern int	getfdcount();
extern int	wildmat();
extern int	waitnb();
extern int	xread();
extern int	xwrite();
extern int	xwritev();
extern int	LockFile();
extern int	GetResourceUsage();
extern int	SetNonBlocking();
extern void	CloseOnExec();
extern void	Radix32();
extern char	*INNVersion();
extern char	*ReadInDescriptor();
extern char	*ReadInFile();
extern FILE	*xfopena();
