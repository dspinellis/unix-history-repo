/*
 * [.vms]vms_popen.c -- substitute routines for missing pipe calls.
 */

/*
 * Copyright (C) 1991 the Free Software Foundation, Inc.
 *
 * This file is part of GAWK, the GNU implementation of the
 * AWK Progamming Language.
 *
 * GAWK is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GAWK is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GAWK; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef NO_VMS_PIPES

#include "awk.h"	/* really "../awk.h" */
#include <stdio.h>

#ifndef PIPES_SIMULATED

FILE *
popen( const char *command, const char *mode )
{
    fatal(" Cannot open pipe `%s' (not implemented)", command);
    /* NOT REACHED */
    return 0;
}

int
pclose( FILE *current )
{
    fatal(" Internal error ('pclose' not implemented)");
    /* NOT REACHED */
    return -1;
}

int
fork()
{
    fatal(" Internal error ('fork' not implemented)");
    /* NOT REACHED */
    return -1;
}

#else	/*PIPES_SIMULATED*/
	/*
	 * Simulate pipes using temporary files; hope that the user
	 * doesn't expect pipe i/o to be interleaved with other i/o ;-}.
	 *
	 * This was initially based on the MSDOS version, but cannot
	 * use a static array to hold pipe info, because there's no
	 * fixed limit on the range of valid 'fileno's.  Another
	 * difference is that redirection is handled using LIB$SPAWN
	 * rather than constructing a command for system() which uses
	 * '<' or '>'.
	 */
#include "vms.h"
#include <errno.h>
#include <lnmdef.h>	/* logical name definitions */

static void push_logicals P((void));
static void pop_logicals P((void));
static Itm *save_translation P((const Dsc *));
static void restore_translation P((const Dsc *, const Itm *));

typedef enum { unopened = 0, reading, writing } pipemode;
typedef struct pipe_info {
    char *command;
    char *name;
    pipemode pmode;
} PIPE;
static PIPE *pipes;
static int pipes_lim = 0;

#define psize(n) ((n) * sizeof(PIPE))
#define expand_pipes(k) do {  PIPE *new_p; \
	int new_p_lim = ((k) / _NFILE + 1) * _NFILE; \
	emalloc(new_p, PIPE *, psize(new_p_lim), "expand_pipes"); \
	if (pipes_lim > 0) \
		memcpy(new_p, pipes, psize(pipes_lim)),  free(pipes); \
	memset(new_p + psize(pipes_lim), 0, psize(new_p_lim - pipes_lim)); \
	pipes = new_p,  pipes_lim = new_p_lim;  } while(0)

FILE *
popen( const char *command, const char *mode )
{
    FILE *current;
    char *name, *mktemp();
    int   cur, strcmp();
    pipemode curmode;

    if (strcmp(mode, "r") == 0)
	curmode = reading;
    else if (strcmp(mode, "w") == 0)
	curmode = writing;
    else
	return NULL;

    /* make a name for the temporary file */
    if ((name = mktemp(strdup("sys$scratch:gawk-pipe_XXXXXX.tmp"))) == 0)
	return NULL;

    if (curmode == reading) {
	/* an input pipe reads a temporary file created by the command */
	vms_execute(command, (char *)0, name);	/* 'command >tempfile' */
    }
    if ((current = fopen(name, mode, "mbc=24", "mbf=2")) == NULL) {
	free(name);
	return NULL;
    }
    cur = fileno(current);
    if (cur >= pipes_lim)  expand_pipes(cur);
 /* assert( cur >= 0 && cur < pipes_lim ); */
    pipes[cur].name = name;
    pipes[cur].pmode = curmode;
    pipes[cur].command = strdup(command);
    return current;
}

int
pclose( FILE *current )
{
    int rval, cur = fileno(current);

 /* assert( cur >= 0 && cur < pipes_lim ); */
    if (pipes[cur].pmode == unopened)
	return -1;	/* should never happen */

    rval = fclose(current);	/* close temp file; if reading, we're done */
    if (pipes[cur].pmode == writing) {
	/* an output pipe feeds the temporary file to the other program */
	rval = vms_execute(pipes[cur].command, pipes[cur].name, (char *)0);
    }
    /* clean up */
    unlink(pipes[cur].name);	/* get rid of the temporary file */
    pipes[cur].pmode = unopened;
    free(pipes[cur].name),  pipes[cur].name = 0;
    free(pipes[cur].command),  pipes[cur].command = 0;
    return rval;
}

    /*
     * Create a process and execute a command in it.  This is essentially
     * the same as system() but allows us to specify SYS$INPUT (stdin)
     * and/or SYS$OUTPUT (stdout) for the process.
     * [With more work it could truly simulate a pipe using mailboxes.]
     */
int
vms_execute( const char *command, const char *input, const char *output )
{
    Dsc cmd, in, out, *in_p, *out_p;
    u_long sts, cmpltn_sts, LIB$SPAWN();

    cmd.len = strlen(cmd.adr = (char *)command);
    if (input)
	in.len = strlen(in.adr = (char *)input),  in_p = &in;
    else
	in_p = 0;
    if (output)
	out.len = strlen(out.adr = (char *)output),  out_p = &out;
    else
	out_p = 0;

    push_logicals();	/* guard against user-mode definitions of sys$Xput */
    sts = LIB$SPAWN(&cmd, in_p, out_p, (long *)0,
		    (Dsc *)0, (u_long *)0, &cmpltn_sts);
    pop_logicals();	/* restore environment */

    if (vmswork(sts) && vmsfail(cmpltn_sts))  sts = cmpltn_sts;
    if (vmsfail(sts)) {
	errno = EVMSERR,  vaxc$errno = sts;
	return -1;
    } else
	return 0;
}

/*----*
	This rigmarole is to guard against interference from the current
	environment.  User-mode definitions of SYS$INPUT and/or SYS$OUTPUT
	will interact with spawned subprocesses--including LIB$SPAWN with
	explicit input and/or output arguments specified--if they were
	defined without the 'CONFINED' attribute.  The definitions created
	in vms_args.c as part of command line I/O redirection happened to
	fall into this category :-(, but even though that's been fixed,
	there's still the possibility of the user doing something like
	 |$ define/user sys$output foo.out
	prior to starting the program.  Without ``/name_attr=confine'',
	that will really screw up pipe simulation, so we've got to work-
	around it here.  This is true whether pipes are implemented via
	mailboxes or temporary files, as long as lib$spawn() is being used.

	push_logicals() calls save_translation() the first time it's
	invoked; the latter allocates some memory to hold a full logical
	name translation and uses $trnlnm to fill that in.  Then if either
	sys$input or sys$output has a user-mode, non-confined translation,
	push_logicals() will delete the definition(s) using $dellnm.
	After the spawned command has returned, pop_logicals() is called;
	it calls restore_translation() for any deleted values; the latter
	uses $crllnm or $crelog to recreate the original definition.

	SYS$ERROR is currently ignored; perhaps it should receive the same
	treatment...
*----*/

 /* logical name table, and names of interest; these are all constant */
static const Descrip(lnmtable,"LNM$PROCESS_TABLE");
static const Descrip(sys_input,"SYS$INPUT");
static const Descrip(sys_output,"SYS$OUTPUT");
static const unsigned char acmode = PSL$C_USER; /* only care about user-mode */

 /* macros for simplfying the code a bunch */
#define DelTrans(l)	SYS$DELLNM(&lnmtable, (l), &acmode)
#define GetTrans(l,i)	SYS$TRNLNM((u_long *)0, &lnmtable, (l), &acmode, (i))
#define SetTrans(l,i)	SYS$CRELNM((u_long *)0, &lnmtable, (l), &acmode, (i))
 /* itemlist manipulation macros; separate versions for aggregate and scalar */
#define SetItmA(i,c,p,r) ((i).code = (c), (i).len = sizeof (p),\
			  (i).buffer = (p), (i).retlen = (u_short *)(r))
#define SetItmS(i,c,p)	 ((i).code = (c), (i).len = sizeof *(p),\
			  (i).buffer = (p), (i).retlen = (u_short *)0)
#define EndItm0(i)	 ((i).code = (i).len = 0)

 /* translate things once, then hold the results here for multiple re-use */
static Itm *input_definition, *output_definition;

static void
push_logicals( void )		/* deassign sys$input and/or sys$output */
{
    static int init_done = 0;

    if (!init_done) {	/* do logical name lookups one-time only */
	input_definition = save_translation(&sys_input);
	output_definition = save_translation(&sys_output);
	init_done = 1;
    }
    if (input_definition) DelTrans(&sys_input);		/* kill sys$input */
    if (output_definition) DelTrans(&sys_output);	/* and sys$output */
}

static void
pop_logicals( void )		/* redefine sys$input and/or sys$output */
{
    if (input_definition) restore_translation(&sys_input, input_definition);
    if (output_definition) restore_translation(&sys_output, output_definition);
}

static Itm *
save_translation( const Dsc *logname )
{
    Itm trans[4], *itmlst;
    long trans_attr, max_trans_indx;	/* 0-based translation index count */
    unsigned char trans_acmode;		/* translation's access mode */
    unsigned itmlst_size;
    register int i, j;

    itmlst = 0;
    /* Want translation index count for non-confined, user-mode definition;
	unfortunately, $trnlnm does not provide that much control.  Try to
	fetch several values of interest, then decide based on the result.
     */
    SetItmS(trans[0], LNM$_MAX_INDEX, &max_trans_indx),	 max_trans_indx = -1;
    SetItmS(trans[1], LNM$_ACMODE, &trans_acmode),	 trans_acmode = 0;
    SetItmS(trans[2], LNM$_ATTRIBUTES, &trans_attr),	 trans_attr = 0;
    EndItm0(trans[3]);
    if (vmswork(GetTrans(logname, trans)) && max_trans_indx >= 0
      && trans_acmode == PSL$C_USER && !(trans_attr & LNM$M_CONFINE)) {
	/* Now know that definition of interest exists;
	    allocate and initialize an item list and associated buffers;
	    use three entries for each translation.
	 */
	itmlst_size = (3 * (max_trans_indx + 1) + 1) * sizeof(Itm);
	emalloc(itmlst, Itm *, itmlst_size, "save_translation");
	for (i = 0; i <= max_trans_indx; i++) {
	    struct def { u_long indx, attr; u_short len;
			 char str[LNM$C_NAMLENGTH], eos; } *wrk;
	    emalloc(wrk, struct def *, sizeof (struct def), "save_translation");
	    wrk->indx = (u_long)i;  /* this one's an input value for $trnlnm */
	    SetItmS(itmlst[3*i+0], LNM$_INDEX, &wrk->indx);
	    SetItmS(itmlst[3*i+1], LNM$_ATTRIBUTES, &wrk->attr),  wrk->attr = 0;
	    SetItmA(itmlst[3*i+2], LNM$_STRING, &wrk->str, &wrk->len),  wrk->len = 0;
	}
	EndItm0(itmlst[3*i]);   /* assert( i == max_trans_indx+1 ); */
	/* Time to perform full logical name translation,
	    then update item list for subsequent restoration.
	    If there are any holes [don't know whether that's possible]
	    collapse them out of the list; don't want them at restore time.
	 */
	if (vmswork(GetTrans(logname, itmlst))) {
	    for (i = 0, j = -1; i <= max_trans_indx; i++) {
		u_long *attr_p;
		attr_p = itmlst[3*i+1].buffer;	/* copy (void *) to true type */
		if (*attr_p & LNM$M_EXISTS) {
		    *attr_p &= ~LNM$M_EXISTS;	/* must clear this bit */
		    if (++j < i)  itmlst[3*j+0] = itmlst[3*i+0],
				  itmlst[3*j+1] = itmlst[3*i+1],
				  itmlst[3*j+2] = itmlst[3*i+2];
		    if (itmlst[3*j+2].retlen) { /* fixup buffer length */
			itmlst[3*j+2].len = *itmlst[3*j+2].retlen;
			itmlst[3*j+2].retlen = (u_short *)0;
		    }
		}
	    }
	    if (++j < i)  EndItm0(itmlst[3*j]);
	} else	    /* should never happen; tolerate potential memory leak */
	    free(itmlst),  itmlst = 0;  /*('wrk' buffer(s) will become lost)*/
    }
    return itmlst;
}

static void
restore_translation( const Dsc *logname, const Itm *itemlist )
{
    Dsc trans_val;
    u_long *attr_p;
# define LOG_PROCESS_TABLE 2		/* <obsolete> */
# define LOG_USERMODE PSL$C_USER

 /* assert( itemlist[1].code == LNM$_ATTRIBUTES ); */
    attr_p = itemlist[1].buffer;	/* copy (void *) to (u_long *) */
    if (*attr_p & LNM$M_CRELOG) {	/* check original creation method */
	/* $crelog values can have only one translation;
	    so it'll be the first string entry in the itemlist.
	 */
     /* assert( itemlist[2].code == LNM$_STRING ); */
	trans_val.adr = itemlist[2].buffer;
	trans_val.len = itemlist[2].len;
	(void) SYS$CRELOG(LOG_PROCESS_TABLE, logname, &trans_val, LOG_USERMODE);
    } else {
	/* $crelnm definition; itemlist could specify multiple translations,
	    but has already been setup properly for use as-is.
	 */
	(void) SetTrans(logname, itemlist);
    }
}

#endif	/*PIPES_SIMULATED*/

#endif	/*!NO_VMS_PIPES*/
