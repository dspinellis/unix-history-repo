/* $Id: autosub.c,v 3.0 1992/02/01 03:09:32 davison Trn $
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
#include "search.h"
#include "ngsrch.h"
#include "util.h"
#include "final.h"
#include "INTERN.h"
#include "autosub.h"

/* Consider the newsgroup specified, and return:	*/
/* : if we should autosubscribe to it			*/
/* ! if we should autounsubscribe to it			*/
/* \0 if we should ask the user.			*/
int
auto_subscribe(ng)
char *ng;
{
    char *s;

    if((s = getval("AUTOSUBSCRIBE", (char *)NULL))
	&& matchlist(s, ng)) return ':';
    if((s = getval("AUTOUNSUBSCRIBE", (char *)NULL))
	&& matchlist(s, ng)) return '!';
    return 0;
}

bool
matchlist(patlist, s)
char *patlist;
char *s;
{
    COMPEX ilcompex;
    char *p;
    char *err;
    bool result;
    bool tmpresult;

    result = FALSE;
    init_compex(&ilcompex);
    while(patlist && *patlist) {

	if(*patlist == '!') {
		patlist++;
		tmpresult = FALSE;
	} else tmpresult = TRUE;

	if(p = index(patlist, ',')) *p = '\0';
        /* compile regular expression */
	err = ng_comp(&ilcompex,patlist,TRUE,TRUE);
	if(p) *p++ = ',';

	if(err != Nullch) {
	    printf("\n%s\n", err) FLUSH;
	    finalize(1);
	}
	
	if (execute(&ilcompex,s) != Nullch)
	    result = tmpresult;
	patlist = p;
    }
    free_compex(&ilcompex);
    return result;
}
