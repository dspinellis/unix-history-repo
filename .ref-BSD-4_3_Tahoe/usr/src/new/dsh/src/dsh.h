/*  some definitions */
#define HOSTNAMESIZE 32
#define TRUE 1
#define FALSE 0
#define bool char
#define COMMANDSIZE 450
#define PATHSIZE 100
#define BIDCMD "dbid"

/* some basic functions */
#define new(q) ((q *)malloc(sizeof(q)))
#define swab(a) ((a&0xff)<<8 | (a&0xff00)>>8)

/*
 *	the hosts to use
 */
struct hostdef {
    char	h_name[HOSTNAMESIZE];	/* name of this host */
    char	h_user[HOSTNAMESIZE];	/* name of account to use */
    char	h_dir[PATHSIZE];	/* the directory to use */
    double	h_weight;		/* the weighting for this node */
    struct hostdef *h_next;	/* next one */
};
