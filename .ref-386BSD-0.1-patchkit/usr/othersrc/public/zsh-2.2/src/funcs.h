struct asgment;
struct utmp;

#include "builtin.pro"
#include "cond.pro"
#include "exec.pro"
#include "glob.pro"
#include "hist.pro"
#include "init.pro"
#include "jobs.pro"
#include "lex.pro"
#include "loop.pro"
#include "math.pro"
#include "mem.pro"
#include "params.pro"
#include "parse.pro"
#include "subst.pro"
#include "table.pro"
#include "text.pro"
#include "utils.pro"
#include "watch.pro"
#include "zle_hist.pro"
#include "zle_main.pro"
#include "zle_misc.pro"
#include "zle_move.pro"
#include "zle_refresh.pro"
#include "zle_tricky.pro"
#include "zle_utils.pro"
#include "zle_vi.pro"
#include "zle_word.pro"

char *mktemp DCLPROTO((char *));
#ifndef HAS_STDLIB
char *malloc DCLPROTO((int));
char *realloc DCLPROTO((char *,int));
char *calloc DCLPROTO((int,int));
#endif
char *ttyname DCLPROTO((int));

extern char PC, *BC, *UP;
extern short ospeed;
extern int tgetent DCLPROTO((char *bp, char *name));
extern int tgetnum DCLPROTO((char *id));
extern int tgetflag DCLPROTO((char *id));
extern char *tgetstr DCLPROTO((char *id, char **area));
extern char *tgoto DCLPROTO((char *cm, int destcol, int destline));
extern int tputs DCLPROTO((char *cp, int affcnt, int (*outc)()));
