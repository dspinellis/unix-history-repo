/*      systat.h     1.1     83/10/01     */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/timeb.h>
#include <sys/vm.h>
#include <sys/file.h>

#include <machine/pte.h>

#include <nlist.h>
#include <pwd.h>
#include <math.h>
#include <curses.h>
#include <signal.h>
#include <ctype.h>

struct  nlist nlst[];

struct p_times {
        short   pt_pid;
        float   pt_pctcpu;
        int     pt_uid;
        int     pt_paddr;
        struct  proc *pt_pp;
} *pt;

struct procs {
        int     pid;
        char    cmd[16];
} procs[200];
int     numprocs;

struct users {
        int     k_uid;
        char    k_name[16];
} known[30];

char    *kmemf;
char    *memf;
char    *swapf;
int     kmem;
int     mem;
int     swap;
int     pcbpf;
int     argaddr;
int     col;
long    nproc;
long    procp;
double  ccpu;
double  lccpu;
char    *malloc();
char    *namp;
char    *strncpy();
char    c;
char    hostname[32];
int     numprocs;
int     numknown;
int     naptime;
int     maxind;
long    getw();
float   total;
int     factor;
double  lave;
int     dellave;
struct  passwd *getpwuid();
char    pidname[30];
struct  pte *usrpt;
struct  pte *Usrptma;

WINDOW  *wnd;
