/*      systat.h     1.4     84/08/09     */

#include <sys/dk.h>
#include <curses.h>

struct p_times {
        short   pt_pid;
        float   pt_pctcpu;
        int     pt_uid;
        int     pt_paddr;
        struct  proc *pt_pp;
} *pt;
long    nproc, procp;
struct	proc *kprocp;

struct procs {
        int     pid;
        char    cmd[16];
} procs[200];
int     numprocs;

struct users {
        int     k_uid;
        char    k_name[16];
} known[30];
int     numknown;

struct  cmdtab {
        char    *c_name;		/* command name */
        int     (*c_refresh)();		/* display refresh */
        int     (*c_fetch)();		/* sets up data structures */
        int     (*c_label)();		/* label display */
	int	(*c_init)();		/* initialize namelist, etc. */
	WINDOW	*(*c_open)();		/* open display */
	int	(*c_close)();		/* close display */
	int	(*c_cmd)();		/* display command interpreter */
	char	c_flags;		/* been initialized (right now) */
};

struct	cmdtab *curcmd;
struct	cmdtab cmdtab[];
struct	cmdtab *lookup();

int     kmem, mem, swap;
int     naptime, col;

long	ntext, textp;
struct	text *xtext;

double  lccpu;

char    *kmemf, *memf, *swapf;
char	dr_name[DK_NDRIVE][10];
int	ndrives;
int	hz;
float	dk_mspw[DK_NDRIVE];
char    c, *namp, hostname[32];

struct  pte *usrpt;
struct  pte *Usrptma;

WINDOW  *wnd;
int	CMDLINE;

char    *malloc(), *calloc(), *strncpy();
long    getw();
