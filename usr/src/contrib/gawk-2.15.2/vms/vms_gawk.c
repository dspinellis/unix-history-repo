/*
 * vms_gawk.c -- parse GAWK command line using DCL syntax ]
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

/*
 * vms_gawk.c - routines to parse the command line as a native DCL command
 *	       rather than as a foreign command string.
 *							Pat Rankin, Nov'89
 *						[ revised for 2.12, May'91 ]
 */

#include "awk.h"
#include "vms.h"
#define COMMAND_NAME "GAWK"	/* verb name & for 'usage' message(s) */
#define USAGE_PROG_RQRD 1
#define USAGE_FILE_RQRD 2
#define USAGE_BAD_COMBO 3
#define USAGE_RUN_CMD	4
#define STS$M_INHIB_MSG 0x10000000

#define Present(arg)		vmswork(Cli_Present(arg))
#define Get_Value(arg,buf,siz)	vmswork(Cli_Get_Value(arg,buf,siz))

extern void   gawk_cmd();	/* created with $ SET COMMAND/OBJECT */
#define GAWK_CMD ((const void *)gawk_cmd)
extern void   _exit(int);
static int    vms_usage(int);

#define ARG_SIZ 250
union arg_w_prefix {	/* structure used to simplify prepending of "-" */
    char     value[3+ARG_SIZ+1];
    struct {
	char prefix[3];		/* for "-? " */
	char buf[ARG_SIZ];
	char suffix[1];		/* room for '\0' */
    } arg;
};

#define chk_option(qualifier,optname)	\
    if (Present(qualifier))	\
	strcat(strcat(buf.arg.buf, W_cnt++ ? "," : ""), optname)


/* vms_gawk() - parse GAWK command line using DCL and convert it into the */
/*	       appropriate "-arg" values for compatability with GNU code  */
int
vms_gawk()
{
    u_long sts;
    union arg_w_prefix buf;
    char misc_args[10], *misc_argp;
    int  argc, W_cnt;

    /* check "GAWK_P1"--it's required; its presence will tip us off */
    sts = Cli_Present("GAWK_P1");
    if (CondVal(sts) == CondVal(CLI$_SYNTAX)) {
	/* syntax error indicates that we weren't invoked as a native DCL
	   command, so we'll now attempt to generate a command from the
	   foreign command string and parse that.
	*/
	sts = Cli_Parse_Command(GAWK_CMD, COMMAND_NAME);
	if (vmswork(sts))
	    sts = Cli_Present("GAWK_P1");
    }
    if (vmswork(sts))		/* command parsed successfully */
	v_add_arg(argc = 0, COMMAND_NAME);	/* save "GAWK" as argv[0] */
    else if (CondVal(sts) == CondVal(CLI$_INSFPRM))
	return vms_usage(USAGE_FILE_RQRD);  /* insufficient parameters */
    else if (CondVal(sts) == CondVal(CLI$_CONFLICT))
	return vms_usage(USAGE_BAD_COMBO);  /* conflicting qualifiers (/input+/command) */
    else if (CondVal(sts) == CondVal(CLI$_RUNUSED))
	return vms_usage(USAGE_RUN_CMD);    /* RUN GAWK won't work (no command line) */
    else
	return 0;	/* forced to rely on original parsing */

    if (Present("USAGE"))	/* give usage message and quit */
	return vms_usage(0);
    else if (! (Present("PROGRAM") || Present("PROGFILE")) )
	return vms_usage(USAGE_PROG_RQRD);  /* missing required option */

    misc_argp = misc_args;
    *misc_argp++ = '-';		/* now points at &misc_args[1] */
    if (Present("REG_EXPR")) {
	if (Present("REG_EXPR.AWK"))		/* /reg_exp=awk -> -a */
	    *misc_argp++ = 'a';
	else if (Present("REG_EXPR.EGREP")	/* /reg_exp=egrep -> -e */
	      || Present("REG_EXPR.POSIX"))	/* /reg_exp=posix -> -e */
	    *misc_argp++ = 'e';
    }
#if 0	/* gawk 2.11.1 */
    if (Present("STRICT"))		/* /strict -> -c */
	*misc_argp++ = 'c';
    if (Present("COPYRIGHT"))		/* /copyright -> -C */
	*misc_argp++ = 'C';
    if (Present("VERSION"))		/* /version -> -V */
	*misc_argp++ = 'V';
#else	/* gawk 2.12 */
    W_cnt = 0,	buf.arg.buf[0] = '\0';
    strncpy(buf.arg.prefix, "-W ", 3);
    chk_option("LINT","lint");
    chk_option("POSIX","posix");
    chk_option("STRICT","compat");
    chk_option("COPYRIGHT","copyright");
    chk_option("VERSION","version");
    if (W_cnt > 0)			/* got something */
	v_add_arg(++argc, strdup(buf.value));
#endif	/*0*/
#ifdef DEBUG
    if (Present("DEBUG")) {
#if 0
	int both = Present("DEBUG.ALL");
	if (both || Present("DEBUG.EXECUTION"))
	    *misc_argp++ = 'd';
	if (both || Present("DEBUG.PARSE"))
#endif
	    *misc_argp++ = 'D';
    }
#endif
    *misc_argp = '\0';		/* terminate misc_args[] */
    if (misc_argp > &misc_args[1])	/* got something */
	v_add_arg(++argc, misc_args);	/* store it/them */

    if (Present("FIELD_SEP")) {     /* field separator */
	strncpy(buf.arg.prefix, "-F ", 3);
	if (Get_Value("FIELD_SEP", buf.arg.buf, sizeof buf.arg.buf))
	    v_add_arg(++argc, strdup(buf.value));
    }
    if (Present("VARIABLES")) {     /* variables to init prior to BEGIN */
	strncpy(buf.arg.prefix, "-v ", 3);
	while (Get_Value("VARIABLES", buf.arg.buf, sizeof buf.arg.buf))
	    v_add_arg(++argc, strdup(buf.value));
    }
    if (Present("PROGFILE")) {	    /* program files, /input=file -> -f file */
	strncpy(buf.arg.prefix, "-f ", 3);
	while (Get_Value("PROGFILE", buf.arg.buf, sizeof buf.arg.buf))
	    v_add_arg(++argc, strdup(buf.value));
	v_add_arg(++argc, "--");
    } else if (Present("PROGRAM")) {	/* program text, /program -> 'text' */
	v_add_arg(++argc, "--");
	if (Get_Value("PROGRAM", buf.value, sizeof buf.value))
	    v_add_arg(++argc, strdup(buf.value));
    }

    /* we know that "GAWK_P1" is present [data files and/or 'var=value'] */
    while (Get_Value("GAWK_P1", buf.value, sizeof buf.value))
	v_add_arg(++argc, strdup(buf.value));

    if (Present("OUTPUT")) {	/* let other parser treat this as 'stdout' */
	strncpy(buf.arg.prefix, ">$ ", 3);
	if (Get_Value("OUTPUT", buf.arg.buf, sizeof buf.arg.buf))
	    v_add_arg(++argc, strdup(buf.value));
    }

    return ++argc;		/*(increment to account for arg[0])*/
}

/* vms_usage() - display one or more messages and then terminate */
static int	/* note: doesn't return anything; allows 'return vms_usage()' */
vms_usage( int complaint )
{
static char
    *usage_txt = "\n\
usage:	%s  /COMMANDS=\"awk program text\"  data_file[,data_file,...] \n\
   or	%s  /INPUT=awk_file  data_file[,\"Var=value\",data_file,...] \n\
   or	%s  /INPUT=(awk_file1,awk_file2,...)  data_file[,...] \n\
",  *options_txt = "\n\
options:  /FIELD_SEPARATOR=\"FS_value\" \n\
   -	  /VARIABLES=(\"Var1=value1\",\"Var2=value2\",...) \n\
   -	  /REG_EXPR= AWK or EGREP or POSIX \n\
   -	  /LINT  /POSIX  /[NO]STRICT  /VERSION	/COPYRIGHT  /USAGE \n\
   -	  /OUTPUT=out_file \n\
",  *no_prog = "missing required element: /COMMANDS or /INPUT",
    *no_file = "missing required element: data_file \n\
       (use \"SYS$INPUT:\" to read data lines from the terminal)",
    *bad_combo = "invalid combination of qualifiers \n\
       (/INPUT=awk_file and /COMMANDS=\"awk program\" are mutually exclusive)",
    *run_used = "\"RUN\" was used; required command components missing";
int status, argc;

    fflush(stdout);
    switch (complaint) {
      case USAGE_PROG_RQRD:
	fprintf(stderr, "\n%%%s-W-%s, %s \n", COMMAND_NAME, "PROG_RQRD", no_prog);
	status = CLI$_VALREQ | STS$M_INHIB_MSG;
	break;
      case USAGE_FILE_RQRD:
	if (Present("USAGE")) {
	    status = 1;		/* clean exit */
	} else if (Present("COPYRIGHT") || Present("VERSION")) {
	    v_add_arg(argc=0, COMMAND_NAME);	/* save "GAWK" as argv[0] */
#if 0
	    v_add_arg(++argc, Present("COPYRIGHT") ? "-C" : "-V");
#else
	    v_add_arg(++argc, "-W");
	    v_add_arg(++argc, Present("COPYRIGHT") ? "copyright" : "version");
#endif
	    v_add_arg(++argc, "{}");		/* kludge to suppress 'usage' */
	    v_add_arg(++argc, "NL:");		/* dummy input for kludge */
	    return ++argc;			/* count argv[0] too */
	} else {
	    fprintf(stderr, "\n%%%s-W-%s, %s \n", COMMAND_NAME, "FILE_RQRD", no_file);
	    status = CLI$_INSFPRM | STS$M_INHIB_MSG;
	}
	break;
      case USAGE_BAD_COMBO:
	fprintf(stderr, "\n%%%s-W-%s, %s \n", COMMAND_NAME, "BAD_COMBO", bad_combo);
	status = CLI$_CONFLICT | STS$M_INHIB_MSG;
	break;
      case USAGE_RUN_CMD:
	fprintf(stderr, "\n%%%s-W-%s, %s \n", COMMAND_NAME, "RUN_CMD", run_used);
	status = CLI$_NOOPTPRS | STS$M_INHIB_MSG;
	break;
      default:
	status = 1;
	break;
    }
    fprintf(stderr, usage_txt, COMMAND_NAME, COMMAND_NAME, COMMAND_NAME);
    fprintf(stderr, options_txt);
    fflush(stderr);

    errno = EVMSERR;
    vaxc$errno = status;
    _exit(status);
    /* NOTREACHED */
    return 0;
}
