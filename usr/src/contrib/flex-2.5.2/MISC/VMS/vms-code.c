/* vms-code.c -- additional VMS-specific support code for flex
 */

#include "flexdef.h"

static const char *original_arg0;
static const char default_arg0[] = "flex.exe";

#define IN_FD	0
#define OUT_FD	1
#define ERR_FD	2

static char *fix_arg0 PROTO((const char *));

/* Command line arguments fixup -- simplify argv[0], and handle `>'
   output redirection request; called first thing from main().  */

void argv_fixup( iargc, iargv )
int *iargc;
char ***iargv;
{
    const char *mode[3], *rfm[3], *name[3];
    char *p;
    int i, oargc, punct, which, append, alt_rfm;

    /*
     * Get original argv[0] supplied by run-time library startup code,
     * then replace it with a stripped down one.
     */
    original_arg0 = (*iargv)[0];
    (*iargv)[0] = fix_arg0(original_arg0);

    /*
     *	Check command line arguments for redirection request(s).
     *	For simplicity, if multiple attempts are made, the last one wins.
     */
    name[0] = name[1] = name[2] = 0;
    oargc = 1;	/* number of args caller will see; count includes argv[0] */
    for (i = 1; i < *iargc; i++) {
	p = (*iargv)[i];
	switch (*p) {
	    case '<':
		/* might be "<dir>file"; then again, perhaps "<<dir>file" */
		punct = (strchr(p, '>') != 0);
		if (p[1] == '<') {
		    if (!punct || p[2] == '<')
			flexerror("<<'sentinel' input not supported.");
		    punct = 0;
		}
		if (punct)	/* the '<' seems to be directory punctuation */
		    goto arg;	/*GOTO*/
		mode[IN_FD] = "r";
		rfm[IN_FD]  = 0;
		name[IN_FD] = ++p;
		if (!*p && (i + 1) < *iargc)
		    name[IN_FD] = (*iargv)[++i];
		break;
	    case '>':
		append = (p[1] == '>');
		if (append) ++p;
		alt_rfm = (p[1] == '$');
		if (alt_rfm) ++p;
		which = (p[1] == '&' ? ERR_FD : OUT_FD);
		if (which == ERR_FD) ++p;
		mode[which] = append ? "a" : "w";
		rfm[which]  = alt_rfm ? "rfm=var" : "rfm=stmlf";
		name[which] = ++p;
		if (!*p && (i + 1) < *iargc)
		    name[which] = (*iargv)[++i];
		break;
	    case '|':
		flexerror("pipe output not supported.");
		/*NOTREACHED*/
		break;
	    default:
 arg:		/* ordinary option or argument */
		(*iargv)[oargc++] = p;
		break;
	}
    }
    /* perform any requested redirection; don't bother with SYS$xxx logicals */
    if (name[IN_FD])
	if (!freopen(name[IN_FD], mode[IN_FD], stdin))
	    lerrsf("failed to redirect `stdin' from \"%s\"", name[IN_FD]);
    if (name[OUT_FD])
	if (!freopen(name[OUT_FD], mode[OUT_FD], stdout,
		     rfm[OUT_FD], "rat=cr", "mbc=32", "shr=nil"))
	    lerrsf("failed to redirect `stdout' to \"%s\"", name[OUT_FD]);
    if (name[ERR_FD])  /* likely won't see message if this fails; oh well... */
	if (!freopen(name[ERR_FD], mode[ERR_FD], stderr,
		     rfm[ERR_FD], "rat=cr"))
	    lerrsf("failed to redirect `stderr' to \"%s\"", name[ERR_FD]);
    /* remove any excess arguments (used up from redirection) */
    while (*iargc > oargc)
	(*iargv)[--*iargc] = 0;
    /* all done */
    return;
}

/* Pick out the basename of a full filename, and return a pointer
   to a modifiable copy of it.  */

static char *fix_arg0( arg0 )
const char *arg0;
{
    char *p, *new_arg0;

    if (arg0) {
	/* strip off the path */
	if ((p = strrchr(arg0, ':')) != 0)	/* device punctuation */
	    arg0 = p + 1;
	if ((p = strrchr(arg0, ']')) != 0)	/* directory punctuation */
	    arg0 = p + 1;
	if ((p = strrchr(arg0, '>')) != 0)	/* alternate dir punct */
	    arg0 = p + 1;
    }
    if (!arg0 || !*arg0)
	arg0 = default_arg0;
    /* should now have "something.exe;#"; make a modifiable copy */
    new_arg0 = copy_string(arg0);

    /* strip off ".exe" and/or ";#" (version number),
       unless it ended up as the whole name */
    if ((p = strchr(new_arg0, '.')) != 0 && (p > new_arg0)
	&& (p[1] == 'e' || p[1] == 'E')
	&& (p[2] == 'x' || p[2] == 'X')
	&& (p[3] == 'e' || p[3] == 'E')
	&& (p[4] == ';' || p[4] == '.' || p[4] == '\0'))
	*p = '\0';
    else if ((p = strchr(new_arg0, ';')) != 0 && (p > new_arg0))
	*p = '\0';

    return new_arg0;
}


#include <ssdef.h>
#include <stsdef.h>

#ifdef exit
#undef exit
extern void exit PROTO((int));	/* <stdlib.h> ended up prototyping vms_exit */
#endif

/* Convert zero to VMS success and non-zero to VMS failure.  The latter
   does not bother trying to distinguish between various failure reasons.  */

void vms_exit( status )
int status;
{
    exit( status == 0 ? SS$_NORMAL : (SS$_ABORT | STS$M_INHIB_MSG) );
}
