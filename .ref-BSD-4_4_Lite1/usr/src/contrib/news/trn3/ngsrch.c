/* $Id: ngsrch.c,v 3.0 1992/02/01 03:09:32 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "rcstuff.h"
#include "final.h"
#include "search.h"
#include "trn.h"
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
	return NGS_ERROR;
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
		printf("\n[0 unread in %s -- skipping]",rcline[ng]) FLUSH;
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
bool_int RE;
bool_int fold;
{
    char ng_pattern[128];
    register char *s = pattern, *d = ng_pattern;

    if (!*s) {
	if(compex->expbuf)
	    return Nullch;			/* reuse old pattern */
	else
	    return "No previous search pattern";
    }
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

