#define	SIGHUP	1
#define	SIGINT	2
#define	SIGQUIT	3
#define	SIGQIT	3
#define	SIGILL	4
#define	SIGTRAP	5
#define	SIGIOT	6
#define	SIGEMT	7
#define	SIGFPE	8
#define	SIGKILL	9
#define	SIGBUS	10
#define	SIGSEGV	11
#define	SIGSYS	12
#define	SIGPIPE	13
#define	SIGALRM	14
#define	SIGTERM	15

int	(*signal())();
#define	SIG_DFL	(int (*)())0
#define	SIG_IGN	(int (*)())1
