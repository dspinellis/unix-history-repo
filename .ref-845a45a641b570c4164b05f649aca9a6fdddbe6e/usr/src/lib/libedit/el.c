/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 */

#if !defined(lint) && !defined(SCCSID)
static char sccsid[] = "@(#)el.c	8.1 (Berkeley) %G%";
#endif /* not lint && not SCCSID */

/*
 * el.c: EditLine interface functions
 */
#include "sys.h"

#include <sys/types.h>
#include <sys/param.h>
#include <string.h>
#include <stdlib.h>
#if __STDC__
# include <stdarg.h>
#else
# include <varargs.h>
#endif
#include "el.h"

/* el_init():
 *	Initialize editline and set default parameters.
 */
public EditLine *
el_init(prog, fin, fout)
    const char *prog;
    FILE *fin, *fout;
{
    EditLine *el = (EditLine *) el_malloc(sizeof(EditLine));
#ifdef DEBUG
    char *tty;
#endif

    if (el == NULL)
	return NULL;

    memset(el, 0, sizeof(EditLine));

    el->el_infd  = fileno(fin);
    el->el_outfile = fout;
    el->el_prog = strdup(prog);

#ifdef DEBUG
    if ((tty = getenv("DEBUGTTY")) != NULL) {
	el->el_errfile = fopen(tty, "w");
	if (el->el_errfile == NULL) {
		extern errno;
		(void) fprintf(stderr, "Cannot open %s (%s).\n",
			       tty, strerror(errno));
		return NULL;
	}
    }
    else 
#endif
	el->el_errfile = stderr;

    /*
     * Initialize all the modules. Order is important!!!
     */
    (void) term_init(el);
    (void) tty_init(el);
    (void) key_init(el);
    (void) map_init(el);
    (void) ch_init(el);
    (void) search_init(el);
    (void) hist_init(el);
    (void) prompt_init(el);
    (void) sig_init(el);
    el->el_flags = 0;

    return el;
} /* end el_init */


/* el_end():
 *	Clean up.
 */
public void
el_end(el)
    EditLine *el;
{
    if (el == NULL)
	return;

    el_reset(el);

    term_end(el);
    tty_end(el);
    key_end(el);
    map_end(el);
    ch_end(el);
    search_end(el);
    hist_end(el);
    prompt_end(el);
    sig_end(el);

    el_free((ptr_t) el->el_prog);
    el_free((ptr_t) el);
} /* end el_end */ 


/* el_reset():
 *	Reset the tty and the parser
 */
public void
el_reset(el)
    EditLine *el;
{
    tty_cookedmode(el);
    ch_reset(el);	/* XXX: Do we want that? */
}


/* el_set():
 *	set the editline parameters
 */
public int
#if __STDC__
el_set(EditLine *el, int op, ...)
#else
el_set(va_alist)
    va_dcl
#endif
{
    va_list va;
    int rv;
#if __STDC__
    va_start(va, op);
#else
    EditLine *el;
    int op;

    va_start(va);
    el = va_arg(va, EditLine *);
    op = va_arg(va, int);
#endif
    
    switch (op) {
    case EL_PROMPT:
	rv = prompt_set(el, va_arg(va, el_pfunc_t));
	break;

    case EL_TERMINAL:
	rv = term_set(el, va_arg(va, char *));
	break;

    case EL_EDITOR:
	rv = map_set_editor(el, va_arg(va, char *));
	break;

    case EL_SIGNAL:
	if (va_arg(va, int))
	    el->el_flags |= HANDLE_SIGNALS;
	else
	    el->el_flags &= ~HANDLE_SIGNALS;
	rv = 0;
	break;

    case EL_BIND:
    case EL_TELLTC:
    case EL_SETTC:
    case EL_ECHOTC:
    case EL_SETTY:
	{
	    char *argv[20];
	    int i;
	    for (i = 1; i < 20; i++)
		if ((argv[i] = va_arg(va, char *)) == NULL)
		     break;

	    switch (op) {
	    case EL_BIND:
		argv[0] = "bind";
		rv = map_bind(el, i, argv);
		break;

	    case EL_TELLTC:
		argv[0] = "telltc";
		rv = term_telltc(el, i, argv);
		break;

	    case EL_SETTC:
		argv[0] = "settc";
		rv = term_settc(el, i, argv);
		break;

	    case EL_ECHOTC:
		argv[0] = "echotc";
		rv = term_echotc(el, i, argv);
		break;

	    case EL_SETTY:
		argv[0] = "setty";
		rv = tty_stty(el, i, argv);
		break;

	    default:
		rv = -1;
		abort();
		break;
	    }
	}
	break;
    
    case EL_ADDFN:
	{
	    char 	*name = va_arg(va, char *);
	    char 	*help = va_arg(va, char *);
	    el_func_t    func = va_arg(va, el_func_t);
	    rv = map_addfunc(el, name, help, func);
	}
	break;

    case EL_HIST:
	{
	    hist_fun_t func = va_arg(va, hist_fun_t);
	    ptr_t      ptr = va_arg(va, char *);
	    rv = hist_set(el, func, ptr);
	}
	break;

    default:
	rv = -1;
    }

    va_end(va);
    return rv;
} /* end el_set */


/* el_line():
 *	Return editing info
 */
public const LineInfo *
el_line(el)
    EditLine *el;
{
    return (const LineInfo *) &el->el_line;
}

static const char elpath[] = "/.editrc";

/* el_source():
 *	Source a file
 */
public int
el_source(el, fname)
    EditLine *el;
    const char *fname;
{
    FILE *fp;
    size_t len;
    char *ptr, path[MAXPATHLEN];

    if (fname == NULL) {
	fname = &elpath[1];
	if ((fp = fopen(fname, "r")) == NULL) {
	    if ((ptr = getenv("HOME")) == NULL) 
		return -1;
	    fname = strncpy(path, ptr, MAXPATHLEN);
	    (void) strncat(path, elpath, MAXPATHLEN);
	    path[MAXPATHLEN-1] = '\0';
	}
    }

    if ((fp = fopen(fname, "r")) == NULL) 
	return -1;

    while ((ptr = fgetline(fp, &len)) != NULL)
	ptr[len - 1] = '\0';
	if (parse_line(el, ptr) == -1) {
	    (void) fclose(fp);
	    return -1;
	}

    (void) fclose(fp);
    return 0;
}


/* el_resize():
 *	Called from program when terminal is resized
 */
public void
el_resize(el)
    EditLine *el;
{
    int lins, cols;
    sigset_t oset, nset;
    (void) sigemptyset(&nset);
    (void) sigaddset(&nset, SIGWINCH);
    (void) sigprocmask(SIG_BLOCK, &nset, &oset);

    /* get the correct window size */
    if (term_get_size(el, &lins, &cols))
	term_change_size(el, lins, cols);

    (void) sigprocmask(SIG_SETMASK, &oset, NULL);
}
