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
static char sccsid[] = "@(#)parse.c	8.1 (Berkeley) %G%";
#endif /* not lint && not SCCSID */

/*
 * parse.c: parse an editline extended command
 *
 * commands are:
 *
 *	bind
 *	echotc
 *	settc
 *	gettc
 */
#include "sys.h"
#include "el.h"
#include "tokenizer.h"

private struct {
    char *name;
    int (*func) __P((EditLine *, int, char **));
} cmds[] = {
    {	"bind",		map_bind 	},
    {	"echotc",	term_echotc 	},
    {	"history",	hist_list	},
    {	"telltc",	term_telltc 	},
    {	"settc",	term_settc	},
    {	"setty",	tty_stty	},
    {	NULL,		NULL		}
};


/* parse_line():
 *	Parse a line and dispatch it
 */
protected int
parse_line(el, line)
    EditLine *el;
    const char *line;
{
    char **argv;
    int argc;
    Tokenizer *tok;

    tok = tok_init(NULL);
    tok_line(tok, line, &argc, &argv);
    argc = el_parse(el, argc, argv);
    tok_end(tok);
    return argc;
}

/* el_parse():
 *	Command dispatcher
 */
public int
el_parse(el, argc, argv)
    EditLine *el;
    int argc;
    char *argv[];
{
    char *ptr;
    int i;

    for (ptr = argv[0]; *ptr && *ptr != ':'; ptr++)
	continue;

    if (*ptr == ':') {
	*ptr = '\0';
	if (el_match(el->el_prog, ptr))
	    return 0;
    }
    else
	ptr = argv[0];

    for (i = 0; cmds[i].name != NULL; i++)
	if (strcmp(cmds[i].name, ptr) == 0) {
	    i = (*cmds[i].func)(el, argc, argv);
	    return -i;
	}

    return -1;
}


/* parse__escape():
 *	Parse a string of the form ^<char> \<odigit> \<char> and return
 *	the appropriate character or -1 if the escape is not valid
 */
protected int
parse__escape(ptr)
    const char  ** const ptr;
{
    const char   *p;
    int   c;

    p = *ptr;

    if (p[1] == 0) 
	return -1;

    if (*p == '\\') {
	p++;
	switch (*p) {
	case 'a':
	    c = '\007';		/* Bell */
	    break;
	case 'b':
	    c = '\010';		/* Backspace */
	    break;
	case 't':
	    c = '\011';		/* Horizontal Tab */
	    break;
	case 'n':
	    c = '\012';		/* New Line */
	    break;
	case 'v':
	    c = '\013';		/* Vertical Tab */
	    break;
	case 'f':
	    c = '\014';		/* Form Feed */
	    break;
	case 'r':
	    c = '\015';		/* Carriage Return */
	    break;
	case 'e':
	    c = '\033';		/* Escape */
	    break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	    {
		int cnt, ch;

		for (cnt = 0, c = 0; cnt < 3; cnt++) {
		    ch = *p++;
		    if (ch < '0' || ch > '7') {
			p--;
			break;
		    }
		    c = (c << 3) | (ch - '0');
		}
		if ((c & 0xffffff00) != 0) 
		    return -1;
		--p;
	    }
	    break;
	default:
	    c = *p;
	    break;
	}
    }
    else if (*p == '^' && isalpha((unsigned char) *p)) {
	p++;
	c = (*p == '?') ? '\177' : (*p & 0237);
    }
    else
	c = *p;
    *ptr = ++p;
    return c;
}

/* parse__string():
 *	Parse the escapes from in and put the raw string out
 */
protected char *
parse__string(out, in)
    char *out;
    const char *in;
{
    char *rv = out;
    int n;
    for (;;)
	switch (*in) {
	case '\0':
	    *out = '\0';
	    return rv;

	case '\\':
	case '^':
	    if ((n = parse__escape(&in)) == -1)
		return NULL;
	    *out++ = n;
	    break;

	default:
	    *out++ = *in++;
	    break;
	}
}

/* parse_cmd():
 *	Return the command number for the command string given
 *	or -1 if one is not found
 */
protected int
parse_cmd(el, cmd)
    EditLine *el;
    const char *cmd;
{
    el_bindings_t *b;

    for (b = el->el_map.help; b->name != NULL; b++)
	if (strcmp(b->name, cmd) == 0)
	    return b->func;
    return -1;
}
