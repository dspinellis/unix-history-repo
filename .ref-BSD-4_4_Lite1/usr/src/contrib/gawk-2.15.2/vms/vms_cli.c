/*
 * vms_cli.c - command line interface routines.
 *							Pat Rankin, Nov'89
 *	Routines called from vms_gawk.c for DCL parsing.
 */

#define  P(foo) ()
#include "config.h"	/* in case we want to suppress 'const' &c */
#include "vms.h"
#ifndef _STRING_H
#include <string.h>
#endif

extern u_long CLI$PRESENT(const Dsc *);
extern u_long CLI$GET_VALUE(const Dsc *, Dsc *, short *);
extern u_long CLI$DCL_PARSE(const Dsc *, const void *, ...);
extern u_long SYS$CLI(void *, ...);
extern u_long SYS$FILESCAN(const Dsc *, void *, long *);
extern void  *LIB$ESTABLISH(u_long (*handler)(void *, void *));
extern u_long LIB$SIG_TO_RET(void *, void *);	/* condition handler */

/* Cli_Present() - call CLI$PRESENT to determine whether a parameter or     */
/*		  qualifier is present on the [already parsed] command line */
u_long
Cli_Present( const char *item )
{
    Dsc item_dsc;
    (void)LIB$ESTABLISH(LIB$SIG_TO_RET);

    item_dsc.len = strlen(item_dsc.adr = (char *)item);
    return CLI$PRESENT(&item_dsc);
}

/* Cli_Get_Value() - call CLI$GET_VALUE to retreive the value of a */
/*		    parameter or qualifier from the command line   */
u_long
Cli_Get_Value( const char *item, char *result, int size )
{
    Dsc item_dsc, res_dsc;
    u_long sts;
    short len = 0;
    (void)LIB$ESTABLISH(LIB$SIG_TO_RET);

    item_dsc.len = strlen(item_dsc.adr = (char *)item);
    res_dsc.len = size,  res_dsc.adr = result;
    sts = CLI$GET_VALUE(&item_dsc, &res_dsc, &len);
    result[len] = '\0';
    return sts;
}

/* Cli_Parse_Command() - use the $CLI system service (undocumented) to	 */
/*			retreive the actual command line (which might be */
/*			"run prog" or "mcr prog [params]") and then call */
/*			CLI$DCL_PARSE to parse it using specified tables */
u_long
Cli_Parse_Command( const void *cmd_tables, const char *cmd_verb )
{
    struct { short len, code; void *adr; } fscn[2];
    struct { char rqtype, rqindx, rqflags, rqstat; unsigned :32;
	     Dsc rdesc; unsigned :32; unsigned :32; unsigned :32; } cmd;
    u_long sts;
    int    ltmp;
    char   longbuf[2600];
    (void)LIB$ESTABLISH(LIB$SIG_TO_RET);

    memset(&cmd, 0, sizeof cmd);
    cmd.rqtype = CLI$K_GETCMD;		/* command line minus the verb */
    sts = SYS$CLI( &cmd, (void *)0, (void *)0); /* get actual command line */

    if (vmswork(sts)) {		/* ok => cli available & verb wasn't "RUN" */
	/* invoked via symbol => have command line (which might be empty) */
	/*    [might also be invoked via mcr or dcl; that's ok]		  */
	if (cmd.rqstat == CLI$K_VERB_MCR) {
	    /* need to strip image name from MCR invocation   */
	    memset(fscn, 0, sizeof fscn);
	    fscn[0].code = FSCN$_FILESPEC;	/* full file specification */
	    (void)SYS$FILESCAN( &cmd.rdesc, fscn, (long *)0);
	    cmd.rdesc.len -= fscn[0].len;	/* shrink size */
	    cmd.rdesc.adr += fscn[0].len;	/* advance ptr */
	}
	/* prepend verb and then parse the command line */
	strcat(strcpy(longbuf, cmd_verb), " "),  ltmp = strlen(longbuf);
	if (cmd.rdesc.len + ltmp > sizeof longbuf)
	    cmd.rdesc.len = sizeof longbuf - ltmp;
	strncpy(&longbuf[ltmp], cmd.rdesc.adr, cmd.rdesc.len);
	cmd.rdesc.len += ltmp,	cmd.rdesc.adr = longbuf;
	sts = CLI$DCL_PARSE( &cmd.rdesc, cmd_tables);
    }

    return sts;
}
