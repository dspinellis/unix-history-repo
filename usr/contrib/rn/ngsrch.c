/* $Header: ngsrch.c,v 4.3 85/05/01 11:44:51 lwall Exp $
 *
 * $Log:	ngsrch.c,v $
 * Revision 4.3  85/05/01  11:44:51  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "rcstuff.h"
#include "final.h"
#include "search.h"
#include "rn.h"
#include "util.h"
#include "term.h"
#include "rcln.h"
#include "INTERN.h"
#include "ngsrch.h"

#ifdef NGSORONLY
    COMPEX ngcompex;
#endif

void
ngsrch_init()
{
#ifdef ZEROGLOB
    init_compex(&ngcompex);
#endif	/* ZEROGLOB */
    ;
}

#ifdef NGSEARCH
int
ng_search(patbuf,get_cmd)
char *patbuf;				/* if patbuf != buf, get_cmd must */
int get_cmd;				/*   be set to FALSE!!! */
{
    char *pattern;			/* unparsed pattern */
    register char cmdchr = *patbuf;	/* what kind of search? */
    register char *s;
    bool backward = cmdchr == '?';	/* direction of search */

    int_count = 0;
    if (get_cmd && buf == patbuf)
	if (!finish_command(FALSE))		/* get rest of command */
	    return NGS_ABORT;
    for (pattern = patbuf+1; *pattern == ' '; pattern++) ;
    if (*pattern) {
	ng_doread = FALSE;
    }
    s = rindex(pattern,cmdchr);
    if (s != Nullch && *(s-1) != '\\') {
	*s++ = '\0';
	if (index(s,'r') != Nullch)
	    ng_doread = TRUE;
    }
    if ((s = ng_comp(&ngcompex,pattern,TRUE,TRUE)) != Nullch) {
					/* compile regular expression */
	printf("\n%s\n",s) FLUSH;
	return NGS_ABORT;
    }
    fputs("\nSearching...",stdout) FLUSH;	/* give them something to read */
    fflush(stdout);
    for (;;) {
	if (int_count) {
	    int_count = 0;
	    return NGS_INTR;
	}
	if (backward) {
	    if (ng > 0)
		--ng;
	    else
		ng = nextrcline;
	}
	else {
	    if (ng >= nextrcline)
		ng = 0;
	    else
		++ng;
	}
	if (ng == current_ng)
	    return NGS_NOTFOUND;
	if (ng == nextrcline || toread[ng] < TR_NONE || !ng_wanted())
	    continue;
	if (toread[ng] == TR_NONE)
	    set_toread(ng);
	
	if (toread[ng] > TR_NONE)
	    return NGS_FOUND;
	else if (toread[ng] == TR_NONE)
	    if (ng_doread)
		return NGS_FOUND;
	    else
		printf("\n[0 unread in %s--skipping]",rcline[ng]) FLUSH;
    }
}

bool
ng_wanted()
{
    return execute(&ngcompex,rcline[ng]) != Nullch;
}
#endif

#ifdef NGSORONLY
char *
ng_comp(compex,pattern,RE,fold)
COMPEX *compex;
char *pattern;
bool RE;
bool fold;
{
    char ng_pattern[128];
    register char *s = pattern, *d = ng_pattern;

    if (!*s)
	return Nullch;			/* reuse old pattern */
    for (; *s; s++) {
	if (*s == '.') {
	    *d++ = '\\';
	    *d++ = *s;
	}
	else if (*s == '?') {
	    *d++ = '.';
	}
	else if (*s == '*') {
	    *d++ = '.';
	    *d++ = *s;
	}
	else if (strnEQ(s,"all",3)) {
	    *d++ = '.';
	    *d++ = '*';
	    s += 2;
	}
	else
	    *d++ = *s;
    }
    *d = '\0';
    return compile(compex,ng_pattern,RE,fold);
}
#endif

