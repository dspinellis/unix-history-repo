/*
 * vms_args.c -- command line parsing, to emulate shell i/o redirection.
 *		[ Escape sequence parsing now suppressed. ]
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
 * [.vms]vms_arg_fixup - emulate shell's command line processing: handle
 *	    stdio redirection, backslash escape sequences, and file wildcard
 *	    expansion.	Should be called immediately upon image startup.
 *
 *						      Pat Rankin, Nov'89
 *						    rankin@eql.Caltech.EDU
 *
 *	<ifile	    - open 'ifile' (readonly) as 'stdin'
 *	>nfile	    - create 'nfile' as 'stdout' (stream-lf format)
 *	>>ofile     - append to 'ofile' for 'stdout'; create it if necessary
 *	>&efile     - point 'stderr' (SYS$ERROR) at 'efile', but don't open
 *	>$vfile     - create 'vfile' as 'stdout', using rms attributes
 *		      appropriate for a standard text file (variable length
 *		      records with implied carriage control)
 *	2>&1        - special case: direct error messages into output file
 *	1>&2        - special case: direct output data to error destination
 *	<<sentinal  - error; reading stdin until 'sentinal' not supported
 *	<-, >-	    - error: stdin/stdout closure not implemented
 *	| anything  - error; pipes not implemented
 *	& <end-of-line> - error; background execution not implemented
 *
 *	any\Xany    - convert 'X' as appropriate; \000 will not work as
 *		      intended since subsequent processing will misinterpret
 *
 *	any*any     - perform wildcard directory lookup to find file(s)
 *	any%any     -	 "       "    ('%' is vms wildcard for '?' [ie, /./])
 *	any?any     - treat like 'any%any' unless no files match
 *	*, %, ?     - if no file(s) match, leave original value in arg list
 *
 *
 * Notes:  a redirection operator  can have optional white space between it
 *	and its filename; the  operator itself *must* be preceded by  white
 *	space  so that it starts  a  separate  argument.  '<' is  ambiguous
 *	since "<dir>file" is a valid VMS file specification; leading '<' is
 *	assumed  to be	stdin--use "\<dir>file" to override.  '>$' is local
 *	kludge to force  stdout to be created with text file RMS attributes
 *	instead of  stream  format;  file  sharing is disabled	for  stdout
 *	regardless.  Multiple  instances of  stdin  or stdout or stderr are
 *	treated as fatal errors  rather than using the first or last.  If a
 *	wildcard file specification is detected, it is expanded into a list
 *	of  filenames  which match; if there  are no  matches, the original
 *	file-spec is left in the argument list rather than having it expand
 *	into thin  air.   No  attempt is made to identify  and	make $(var)
 *	environment substitutions--must draw the line somewhere!
 *
 *   Oct'91, gawk 2.13.3
 *	Open '<' with full sharing allowed, so that we can  read batch logs
 *	and other open files.  Create record-format output ('>$') with read
 *	sharing permited,  so that others can read our output file to check
 *	progess.  For stream  output ('>' or  '>>'), sharing is  disallowed
 *	(for performance reasons).
 */

#include "awk.h"	/* really "../awk.h" */
#include "vms.h"
#include <lnmdef.h>

       void   v_add_arg(int, const char *);
static char  *skipblanks(const char *);
static void   vms_expand_wildcards(const char *);
static u_long vms_define(const char *, const char *);
static char  *t_strstr(const char *, const char *);
#define strstr t_strstr		/* strstr() missing from vaxcrtl for V4.x */

static	int	v_argc,  v_argz = 0;
static	char  **v_argv;

/* vms_arg_fixup() - scan argv[] for i/o redirection and wildcards and also */
/*		    rebuild it with those removed or expanded, respectively */
void
vms_arg_fixup( int *pargc, char ***pargv )
{
    char *f_in, *f_out, *f_err,
	*out_mode, *rms_opt1, *rms_opt2, *rms_opt3, *rms_opt4;
    char **argv = *pargv;
    int i, argc = *pargc;
    int err_to_out_redirect = 0, out_to_err_redirect = 0;

#ifdef CHECK_DECSHELL	    /* don't define this if linking with DECC$SHR */
    if (shell$is_shell())
	return;		    /* don't do anything if we're running DEC/Shell */
#endif
#ifndef NO_DCL_CMD
    for (i = 1; i < argc ; i++)     /* check for dash or other non-VMS args */
	if (strchr("->\\|", *argv[i]))	break;	    /* found => (i < argc) */
    if (i >= argc && (v_argc = vms_gawk()) > 0) {   /* vms_gawk => dcl_parse */
	/* if we successfully parsed the command, replace original argv[] */
	argc = v_argc,	argv = v_argv;
	v_argz = v_argc = 0,  v_argv = NULL;
    }
#endif
    v_add_arg(v_argc = 0, argv[0]);	/* store arg #0 (image name) */

    f_in = f_out = f_err = NULL;	/* stdio setup (no filenames yet) */
    out_mode = "w";			/* default access for stdout */
    rms_opt1 = rms_opt2 = "ctx=stm";	/* ("context = stream") == no-opt */
    rms_opt3 = "shr=nil";		/* no sharing (for '>' output file) */
    rms_opt4 = "mrs=0";			/* maximum record size */

    for (i = 1; i < argc; i++) {
	char *p, *fn;
	int  is_arg;

	is_arg = 0;		/* current arg does not begin with dash */
	p = argv[i];		/* current arg */
	switch (*p) {
	  case '<':		/* stdin */
	      /*[should try to determine whether this is really a directory
		 spec using <>; for now, force user to quote them with '\<']*/
		if ( f_in ) {
		    fatal("multiple specification of '<' for stdin");
		} else if (*++p == '<') {   /* '<<' is not supported */
		    fatal("'<<' not available for stdin");
		} else {
		    p = skipblanks(p);
		    fn = (*p ? p : argv[++i]);	/* use next arg if necessary */
		    if (i >= argc || *fn == '-')
			fatal("invalid i/o redirection, null filespec after '<'");
		    else
			f_in = fn;	    /* save filename for stdin */
		}
		break;
	  case '>':   {		/* stdout or stderr */
	      /*[vms-specific kludge '>$' added to force stdout to be created
		 as record-oriented text file instead of in stream-lf format]*/
		int is_out = 1;		    /* assume stdout */
		if (*++p == '>')	/* '>>' => append */
		    out_mode = "a",  p++;
		else if (*p == '&')	/* '>&' => stderr */
		    is_out = 0,  p++;
		else if (*p == '$')	/* '>$' => kludge for record format */
		    rms_opt1 = "rfm=var",  rms_opt2 = "rat=cr",
		    rms_opt3 = "shr=get",  rms_opt4 = "mrs=32767",  p++;
		else			/* '>'	=> create */
		    {}	    /* use default values initialized prior to loop */
		p = skipblanks(p);
		fn = (*p ? p : argv[++i]);	/* use next arg if necessary */
		if (i >= argc || *fn == '-') {
		    fatal("invalid i/o redirection, null filespec after '>'");
		} else if (is_out) {
		    if (out_to_err_redirect)
			fatal("conflicting specifications for stdout");
		    else if (f_out)
			fatal("multiple specification of '>' for stdout");
		    else
			f_out = fn;	    /* save filename for stdout */
		} else {
		    if (err_to_out_redirect)
			fatal("conflicting specifications for stderr");
		    else if (f_err)
			fatal("multiple specification of '>&' for stderr");
		    else
			f_err = fn;	    /* save filename for stderr */
		}
	    }	break;
	  case '2':		/* check for ``2>&1'' special case'' */
		if (strcmp(p, "2>&1") != 0)
		    goto ordinary_arg;
		else if (f_err || out_to_err_redirect)
		    fatal("conflicting specifications for stderr");
		else {
		    err_to_out_redirect = 1;
		    f_err = "SYS$OUTPUT:";
		}  break;
	  case '1':		/* check for ``1>&2'' special case'' */
		if (strcmp(p, "1>&2") != 0)
		    goto ordinary_arg;
		else if (f_out || err_to_out_redirect)
		    fatal("conflicting specifications for stdout");
		else {
		    out_to_err_redirect = 1;
		    f_out = "SYS$ERROR:";
		}  break;
	  case '|':		/* pipe */
	      /* command pipelines are not supported */
		fatal("command pipes not available ('|' encountered)");
		break;
	  case '&':		/* background */
	      /*[we could probably spawn or fork ourself--maybe someday]*/
		if (*(p+1) == '\0' && i == argc - 1) {
		    fatal("background tasks not available ('&' encountered)");
		    break;
		} else {	/* fall through */
		    ;	/*NOBREAK*/
		}
	  case '-':		/* argument */
		is_arg = 1;		/*(=> skip wildcard check)*/
	  default:		/* other (filespec assumed) */
ordinary_arg:
	      /* process escape sequences or expand wildcards */
		v_add_arg(++v_argc, p);		/* include this arg */
		p = strchr(p, '\\');		/* look for backslash */
		if (p != NULL) {    /* does it have escape sequence(s)? */
#if 0	/* disable escape parsing; it's now done elsewhere within gawk */
		    register int c;
		    char *q = v_argv[v_argc] + (p - argv[i]);
		    do {
			c = *p++;
			if (c == '\\')
			    c = parse_escape(&p);
			*q++ = (c >= 0 ? (char)c : '\\');
		    } while (*p != '\0');
		    *q = '\0';
#endif	/*0*/
		} else if (!is_arg && strchr(v_argv[v_argc], '=') == NULL) {
		    vms_expand_wildcards(v_argv[v_argc]);
		}
		break;
	} /* end switch */
    } /* loop */

    /*
     * Now process any/all I/O options encountered above.
     */

    /* must do stderr first, or vaxcrtl init might not see it */
    /*[ catch 22:  we'll also redirect errors encountered doing <in or >out ]*/
    if (f_err) {	/* define logical name but don't open file */
	int len = strlen(f_err);
	if (strncasecmp(f_err, "SYS$OUTPUT", len) == 0
	 && (f_err[len] == ':' || f_err[len] == '\0'))
	    err_to_out_redirect = 1;
	else
	    (void) vms_define("SYS$ERROR", f_err);
    }
    /* do stdin before stdout, so if we bomb we won't make empty output file */
    if (f_in) {		/* [re]open file and define logical name */
	stdin = freopen(f_in, "r", stdin,
			"ctx=rec", "shr=get,put,del,upd",
			"mrs=32767", "mbc=24", "mbf=2");
	if (stdin != NULL)
	    (void) vms_define("SYS$INPUT", f_in);
	else
	    fatal("<%s (%s)", f_in, strerror(errno));
    }
    if (f_out) {
	stdout = freopen(f_out, out_mode, stdout,
			 rms_opt1, rms_opt2, rms_opt3, rms_opt4,
			 "mbc=24", "mbf=2");
	if (stdout != NULL)
	    (void) vms_define("SYS$OUTPUT", f_out);
	else
	    fatal(">%s%s (%s)", (*out_mode == 'a' ? ">" : ""),
		  f_out, strerror(errno));
    }
    if (err_to_out_redirect) {	/* special case for ``2>&1'' construct */
	(void) fclose(stderr);
	(void) dup2(1, 2);	/* make file 2 (stderr) share file 1 (stdout) */
	stderr = stdout;
	(void) vms_define("SYS$ERROR", "SYS$OUTPUT:");
    } else if (out_to_err_redirect) {	/* ``1>&2'' */
	(void) fclose(stdout);
	(void) dup2(2, 1);	/* make file 1 (stdout) share file 2 (stderr) */
	stdout = stderr;
	(void) vms_define("SYS$OUTPUT", "SYS$ERROR:");
    }

#ifndef NO_DCL_CMD
    /* if we replaced argv[] with our own, we can release it now */
    if (argv != *pargv)
	free((void *)argv),  argv = NULL;
#endif
    *pargc = ++v_argc;		/* increment to account for argv[0] */
    *pargv = v_argv;
    return;
}

/* vms_expand_wildcards() - check a string for wildcard punctuation; */
/*			   if it has any, attempt a directory lookup */
/*			   and store resulting name(s) in argv array */
static void
vms_expand_wildcards( const char *prospective_filespec )
{
    char *p, spec_buf[255+1], res_buf[255+1], *strstr();
    Dsc   spec, result;
    void *context;
    register int len = strlen(prospective_filespec);

    if (len >= sizeof spec_buf)
	return;		/* can't be valid--or at least we can't handle it */
    strcpy(spec_buf, prospective_filespec);	/* copy the arg */
    p = strchr(spec_buf, '?');
    if (p != NULL)	/* change '?' single-char wildcard to '%' */
	do  *p++ = '%',  p = strchr(p, '?');
	    while (p != NULL);
    else if (strchr(spec_buf, '*') == strchr(spec_buf, '%')  /* => both NULL */
	  && strstr(spec_buf, "...") == NULL)
	return;		/* no wildcards present; don't attempt file lookup */
    spec.len = len,  spec.adr = spec_buf;
    result.len = sizeof res_buf - 1,  result.adr = res_buf;

    /* The filespec is already in v_argv[v_argc]; if we fail to match anything,
       we'll just leave it there (unlike most shells, where it would evaporate).
     */
    len = -1;			/* overload 'len' with flag value */
    context = NULL;		/* init */
    while (vmswork(LIB$FIND_FILE(&spec, &result, &context))) {
	for (len = sizeof(res_buf)-1; len > 0 && res_buf[len-1] == ' '; len--) ;
	res_buf[len] = '\0';	/* terminate after discarding trailing blanks */
	v_add_arg(v_argc++, strdup(res_buf));		/* store result */
    }
    (void)LIB$FIND_FILE_END(&context);
    if (len >= 0)		/* (still -1 => never entered loop) */
	--v_argc;		/* undo final post-increment */
    return;
}

/* v_add_arg() - store string pointer in v_argv[]; expand array if necessary */
void
v_add_arg( int idx, const char *val )
{
#ifdef DEBUG_VMS
    fprintf(stderr, "v_add_arg: v_argv[%d] ", idx);
#endif
    if (idx + 1 >= v_argz) {	/* 'v_argz' is the current size of v_argv[] */
	int old_size = v_argz;

	v_argz = idx + 10;	/* increment by arbitrary amount */
	if (old_size == 0)
	    v_argv = (char **)malloc((unsigned)(v_argz * sizeof(char **)));
	else
	    v_argv = (char **)realloc((char *)v_argv,
				     (unsigned)(v_argz * sizeof(char **)));
	if (v_argv == NULL) {	/* error */
	    fatal("%s: %s: can't allocate memory (%s)", "vms_args",
		  "v_argv", strerror(errno));
	} else {
	    while (old_size < v_argz)  v_argv[old_size++] = NULL;
	}
    }
    v_argv[idx] = (char *)val;
#ifdef DEBUG_VMS
    fprintf(stderr, "= \"%s\"\n", val);
#endif
}

/* skipblanks() - return a pointer to the first non-blank in the string */
static char *
skipblanks( const char *ptr )
{
    if (ptr)
	while (*ptr == ' ' || *ptr == '\t')
	    ptr++;
    return (char *)ptr;
}

/* vms_define() - assign a value to a logical name [define/process/user_mode] */
static u_long
vms_define( const char *log_name, const char *trans_val )
{
    Dsc log_dsc;
    static Descrip(lnmtable,"LNM$PROCESS_TABLE");
    static long attr = LNM$M_CONFINE;
    static Itm itemlist[] = { {sizeof attr,LNM$_ATTRIBUTES,&attr,0},
			      {0,LNM$_STRING,0,0}, {0,0} };
    static unsigned char acmode = PSL$C_USER;

    /* avoid "define SYS$OUTPUT sys$output:" for redundant ">sys$output:" */
    if (strncasecmp(log_name, trans_val, strlen(log_name)) == 0)
	return 0;

    log_dsc.len = strlen(log_dsc.adr = (char *)log_name);
    itemlist[1].buffer = (char *)trans_val;
    itemlist[1].len = strlen(trans_val);
    return SYS$CRELNM((u_long *)0, &lnmtable, &log_dsc, &acmode, itemlist);
}

/* t_strstr -- strstr() substitute; search 'str' for 'sub' */
static char *t_strstr ( const char *str, const char *sub )
{
    register const char *s0, *s1, *s2;

    /* special case: empty substring */
    if (!*sub)	return (char *)str;

    /* brute force method */
    for (s0 = s1 = str; *s1; s1 = ++s0) {
	s2 = sub;
	while (*s1++ == *s2++)
	    if (!*s2)  return (char *)s0;	/* full match */
    }
    return (char *)0;	/* not found */
}
