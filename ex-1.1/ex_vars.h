/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */
struct varbl {
	char	*vname[2];
	char	vtype;
	int	vdefault;
	int	vvalue;
} varbls[];
#define	value(a)	varbls[a].vvalue

#define	AUTOINDENT	0
#define	AUTOPRINT	1
#define	BEAUTIFY	2
#define	DIRECTORY	3
#define	EDITANY		4
#define	EDITED		5
#define	ERRBELLS	6
#define	FORK		7
#define	HOME		8
#define	HUSH		9
#define	IGNORECASE	10
#define	INDICATEUL	11
#define	LIST		12
#define	MAGIC		13
#define	MODE		14
#define	NOTIFY		15
#define	NUMBER		16
#define	OPEN		17
#define	OPTIMIZE	18
#define	PRINTALL	19
#define	PROMPT		20
#define	SCROLL		21
#define	SHELL		22
#define	SHIFTWIDTH	23
#define	STICKY		24
#define	TERMINALTYPE	25
#define	TERSE		26
#define	VISUALMESSAGE	27
#define	WINDOW		28
#define	WRAP		29
