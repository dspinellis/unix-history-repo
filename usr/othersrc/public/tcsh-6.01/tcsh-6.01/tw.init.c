/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/tw.init.c,v 3.2 1991/10/12 04:23:51 christos Exp $ */
/*
 * tw.init.c: TwENEX initializations
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: tw.init.c,v 3.2 1991/10/12 04:23:51 christos Exp $")

#include "tw.h"

/*
 * Build the command name list (and print the results).  This is a HACK until
 * I can get the rehash command to include its results in the command list.
 */

static int maxcommands = 0;


void
tw_clear_comm_list()
{
    register int i;

    have_sorted = 0;
    if (numcommands != 0) {
/*        for (i = 0; command_list[i]; i++) { */
	for (i = 0; i < numcommands; i++) {
	    xfree((ptr_t) command_list[i]);
	    command_list[i] = NULL;
	}
	numcommands = 0;
    }
}

void
tw_sort_comms()
{				/* could be re-written */
    register int i, forward;

    /* sort the list. */
    qsort((ptr_t) command_list, (size_t) numcommands, sizeof(command_list[0]), 
	  (int (*) __P((const void *, const void *))) fcompare);

    /* get rid of multiple entries */
    for (i = 0, forward = 0; i < numcommands - 1; i++) {
	if (Strcmp(command_list[i], command_list[i + 1]) == 0) {	/* garbage */
	    xfree((ptr_t) command_list[i]);
	    forward++;		/* increase the forward ref. count */
	}
	else if (forward) {
	    command_list[i - forward] = command_list[i];
	}
    }
    /* Fix fencepost error -- Theodore Ts'o <tytso@athena.mit.edu> */
    if (forward)
	command_list[i - forward] = command_list[i];
    numcommands -= forward;
    command_list[numcommands] = (Char *) NULL;

    have_sorted = 1;
}

void
tw_add_comm_name(name)		/* this is going to be called a LOT at startup */
    Char   *name;
{
    register int length;
    register long i;
    register Char **ncl, **p1, **p2;

    if (maxcommands == 0) {
	command_list = (Char **) xmalloc((size_t) (NUMCMDS_START *
						   sizeof(command_list[0])));
	maxcommands = NUMCMDS_START;
	for (i = 0, p2 = command_list; i < maxcommands; i++)
	    *p2 = NULL;
    }
    else if (numcommands >= maxcommands) {
	ncl = (Char **) xmalloc((size_t) ((maxcommands + NUMCMDS_INCR) *
					  sizeof(command_list[0])));
	for (i = 0, p1 = command_list, p2 = ncl; i < numcommands; i++)
	    *p2++ = *p1++;
	for (; i < maxcommands + NUMCMDS_INCR; i++)
	    *p2++ = NULL;
	xfree((ptr_t) command_list);
	command_list = ncl;
#ifdef COMMENT
	command_list = (Char **) xrealloc(command_list, (maxcommands +
				     NUMCMDS_INCR) * sizeof(command_list[0]));
#endif
	maxcommands += NUMCMDS_INCR;
    }

    if (name[0] == '.')
	return;			/* no dot commands... */
    if (name[0] == '#')
	return;			/* no Emacs buffer checkpoints */

    length = Strlen(name) + 1;

    if (name[length - 2] == '~')
	return;			/* and no Emacs backups either */

    command_list[numcommands] = (Char *) xmalloc((size_t) (length *
							   sizeof(Char)));

    copyn(command_list[numcommands], name, MAXNAMLEN);
    numcommands++;
}

void
tw_add_builtins()
{
    register struct biltins *bptr;

    for (bptr = bfunc; bptr < &bfunc[nbfunc]; bptr++) {
	if (bptr->bname)
	    tw_add_comm_name(str2short(bptr->bname));
    }
}

void
tw_add_aliases()
{
    register struct varent *p;
    register struct varent *c;

    p = &aliases;
    for (;;) {
	while (p->v_left)
	    p = p->v_left;
x:
	if (p->v_parent == 0)	/* is it the header? */
	    return;
	if (p->v_name)
	    tw_add_comm_name(p->v_name);
	if (p->v_right) {
	    p = p->v_right;
	    continue;
	}
	do {
	    c = p;
	    p = p->v_parent;
	} while (p->v_right == c);
	goto x;
    }

}

struct varent *
tw_start_shell_list()
{
    register struct varent *p;
    register struct varent *c;

    p = &shvhed;		/* start at beginning of variable list */

    for (;;) {
	while (p->v_left)
	    p = p->v_left;
x:
	if (p->v_parent == 0)	/* is it the header? */
	    return (NULL);
	if (p->v_name)
	    return (p);		/* found first one */
	if (p->v_right) {
	    p = p->v_right;
	    continue;
	}
	do {
	    c = p;
	    p = p->v_parent;
	} while (p->v_right == c);
	goto x;
    }
}

Char   *
tw_next_shell_var(vptr)
    struct varent **vptr;
{
    register struct varent *p;
    register struct varent *c;
    register Char *cp;

    if ((p = *vptr) == NULL)
	return (NULL);		/* just in case */

    cp = p->v_name;		/* we know that this name is here now */

    /* now find the next one */
    for (;;) {
	if (p->v_right) {	/* if we can go right */
	    p = p->v_right;
	    while (p->v_left)
		p = p->v_left;
	}
	else {			/* else go up */
	    do {
		c = p;
		p = p->v_parent;
	    } while (p->v_right == c);
	}
	if (p->v_parent == 0) {	/* is it the header? */
	    *vptr = NULL;
	    return (cp);
	}
	if (p->v_name) {
	    *vptr = p;		/* save state for the next call */
	    return (cp);
	}
    }

}

Char  **
tw_start_env_list()
{
    return (STR_environ);
}

Char   *
Getenv(str)
    Char   *str;
{
    Char  **var;
    int     len, res;

    len = Strlen(str);
    for (var = STR_environ; var != NULL && *var != NULL; var++)
	if ((*var)[len] == '=') {
	    (*var)[len] = '\0';
	    res = StrQcmp(*var, str);
	    (*var)[len] = '=';
	    if (res == 0)
		return (&((*var)[len + 1]));
	}
    return (NULL);
}

Char   *
tw_next_env_var(evp)
    Char ***evp;
{
    static Char buffer[MAXVARLEN + 1];
    Char   *ps, *pd;

    if (*evp == NULL || **evp == NULL)
	return (NULL);
    for (ps = **evp, pd = buffer;
	 *ps && *ps != '=' && pd <= &buffer[MAXVARLEN]; *pd++ = *ps++);
    *pd = '\0';
    (*evp)++;
    return (buffer);
}
