/*
 * Shell header file
 */
#define	E
#define	INTR	2
#define	QUIT	3

#define QUOTE 	0200

#define FAND 	1
#define FCAT 	2
#define FPIN 	4
#define FPOU 	8
#define FPAR 	16
#define FINT 	32
#define FPRS 	64
#define FDIAG 	128

#define	TCOM	1
#define	TPAR	2
#define	TFIL	3
#define	TLST	4

#define	DTYP	0
#define	DLEF	1
#define	DRIT	2
#define	DFLG	3
#define	DSPR	4
#define	DCOM	5

#define	ENOENT	2
#define	ENOEXEC	8
#define	ENOMEM	12
#define	EACCES	13
#define	ENOTDIR	20

struct shvar {
	char	*value;
	char	*name;
	struct	shvar *next;
} shvhed, paraml, aliases, interps;

struct shvar2 {
	char	*value;
	struct	shvar *prev;
	struct	shvar *next;
};

char	verbose;
char	nofile;
char	*error;
int	uid;
char	loginsh;
char	doneinp;
char	setintr;
char	*arginp;
char	onelflg;
char	scratch[100];

char	prompt[], shell[], pid[], pcs[], home[], path[], n_args[], tim[];

char	*value(), *value1();
char	**glob();
struct	shvar *adrof(), *adrof1();

#define	seterr(s)	if (error == 0) error = s;
int	errno;

struct htmp {
	int	uid;
	char	home[28];
	int	ttytype;
} hentry;
