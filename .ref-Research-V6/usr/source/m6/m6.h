
struct { 
	int word; 
};

struct {		/*"put stack", currently gathering args*/
	int prev;	/*previous frame ptr, self-relative*/
	int pan;	/*argument number of arg being collected*/
	int pa0;	/*self-relative ptr oo arg0*/
#define pend 6
};

struct {		/*"get stack", currently expanding macros*/
	int prev;	/* prev frame ptr, self-realtive*/
	int mframe;	/* ptr to macro definition frame*/
	int mchar;	/* next char relative to def. frame*/
	int marg;	/* 0 or ptr to next arg char reltive to gf*/
	int ga0;	/* arg0 ptr self-rel*/
};

struct {		/*"definition stack"*/
	int prev;	/* prev frame ptr, self-relative*/
	int dswitch;	/* builtin func code, neg for dead definition*/
	int dtext;	/* definition text prt rel to df*/
	int dident;	/* first char of identifier naming def*/
#define dend 6
};

char metas[];
#define lquote metas[0]
#define rquote metas[1]
#define sharp metas[2]
#define colon metas[3]
#define semi metas[4]
#define comma metas[5]
#define dollar metas[6]
#define NMETA 7

char one[];
int rescan;
int traceflag;
int trashflag;		/* nonzero when dead def'n exist*/

int lp;			/* arg collection level (out level)*/
int lg;			/*input level (get level) */
int lq;		/*quote level*/
int l9;			/*apparent call level within arg 9*/

char c;			/*current input character*/

char *pf;		/*put stack frame ptr*/
char *pe;		/*put stack end*/
char *pmax;		/*top of put stack*/
char p0[];		/*put stack*/

char *gf;		/*get stack frame ptr*/
char *ge;		/*get stack end*/
char *gmax;		/*get stack limit*/
char g0[];		/*get stack*/

char *df;		/*definition stack frame ptr*/
char *de;		/*def stack end*/
char *dmax;		/*def stack limit*/
char d0[];		/*def stack*/

char dummy[];		/*dummy empty macro*/
