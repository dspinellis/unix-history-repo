/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)events.c 1.2 %G%";

/*
 * Event/breakpoint managment.
 */

#include "defs.h"
#include "events.h"
#include "main.h"
#include "symbols.h"
#include "tree.h"
#include "eval.h"
#include "source.h"
#include "mappings.h"
#include "process.h"
#include "machine.h"
#include "lists.h"

#ifndef public
typedef struct Event *Event;
typedef struct Breakpoint *Breakpoint;

Boolean inst_tracing;
Boolean single_stepping;
Boolean isstopped;

#include "symbols.h"

Symbol linesym;
Symbol procsym;
Symbol pcsym;
Symbol retaddrsym;

#define addevent(cond, cmdlist) event_alloc(false, cond, cmdlist)
#define event_once(cond, cmdlist) event_alloc(true, cond, cmdlist)

#endif

struct Event {
    unsigned int id;
    Boolean temporary;
    Node condition;
    Cmdlist actions;
};

struct Breakpoint {
    Event event;
    Address bpaddr;	
    Lineno bpline;
    Cmdlist actions;
};

typedef List Eventlist;
typedef List Bplist;

#define eventlist_append(event, el) list_append(list_item(event), nil, el)
#define bplist_append(bp, bl) list_append(list_item(bp), nil, bl)

private Eventlist eventlist;		/* list of active events */
private Bplist bplist;			/* list of active breakpoints */
private Integer eventid;		/* id number of next allocated event */
private Integer trid;			/* id number of next allocated trace */

typedef struct Trcmd {
    Integer trid;
    Event event;
    Cmdlist cmdlist;
} *Trcmd;

private List eachline;		/* commands to execute after each line */
private List eachinst;		/* commands to execute after each instruction */

private Breakpoint bp_alloc();

/*
 * Initialize breakpoint information.
 */

private Symbol builtinsym(str, class, type)
String str;
Symclass class;
Symbol type;
{
    Symbol s;

    s = insert(identname(str, true));
    s->language = findlanguage(".s");
    s->class = class;
    s->type = type;
    return s;
}

public bpinit()
{
    linesym = builtinsym("$line", VAR, t_int);
    procsym = builtinsym("$proc", PROC, nil);
    pcsym = lookup(identname("$pc", true));
    if (pcsym == nil) {
	panic("can't find $pc");
    }
    retaddrsym = builtinsym("$retaddr", VAR, t_int);
    eventlist = list_alloc();
    bplist = list_alloc();
    eachline = list_alloc();
    eachinst = list_alloc();
}

/*
 * Trap an event and do the associated commands when it occurs.
 */

public Event event_alloc(istmp, econd, cmdlist)
Boolean istmp;
Node econd;
Cmdlist cmdlist;
{
    register Event e;

    e = new(Event);
    ++eventid;
    e->id = eventid;
    e->temporary = istmp;
    e->condition = econd;
    e->actions = cmdlist;
    eventlist_append(e, eventlist);
    translate(e);
    return e;
}

/*
 * Delete the event with the given id.
 */

public delevent(id)
unsigned int id;
{
    Event e;
    Breakpoint bp;
    Trcmd t;

    foreach (Event, e, eventlist)
	if (e->id == id) {
	    list_delete(list_curitem(eventlist), eventlist);
	    foreach (Breakpoint, bp, bplist)
		if (bp->event == e) {
		    list_delete(list_curitem(bplist), bplist);
		}
	    endfor
	    break;
	}
    endfor
    foreach (Trcmd, t, eachline)
	if (t->event->id == id) {
	    printrmtr(t);
	    list_delete(list_curitem(eachline), eachline);
	}
    endfor
    foreach (Trcmd, t, eachinst)
	if (t->event->id == id) {
	    printrmtr(t);
	    list_delete(list_curitem(eachinst), eachinst);
	}
    endfor
    if (list_size(eachinst) == 0) {
	inst_tracing = false;
	if (list_size(eachline) == 0) {
	    single_stepping = false;
	}
    }
}

/*
 * Translate an event into the appropriate breakpoints and actions.
 * While we're at it, turn on the breakpoints if the condition is true.
 */

private translate(e)
Event e;
{
    Breakpoint bp;
    Symbol s;
    Node place;
    Lineno line;
    Address addr;

    checkref(e->condition);
    switch (e->condition->op) {
	case O_EQ:
	    if (e->condition->value.arg[0]->op == O_SYM) {
		s = e->condition->value.arg[0]->value.sym;
		place = e->condition->value.arg[1];
		if (s == linesym) {
		    if (place->op == O_QLINE) {
			line = place->value.arg[1]->value.lcon;
			addr = objaddr(line,
			    place->value.arg[0]->value.scon);
		    } else {
			eval(place);
			line = pop(long);
			addr = objaddr(line, cursource);
		    }
		    if (addr == NOADDR) {
			delevent(e->id);
			beginerrmsg();
			fprintf(stderr, "no executable code at line ");
			prtree(stderr, place);
			enderrmsg();
		    }
		    bp = bp_alloc(e, addr, line, e->actions);
		} else if (s == procsym) {
		    eval(place);
		    s = pop(Symbol);
		    bp = bp_alloc(e, codeloc(s), 0, e->actions);
		    if (isactive(s) and pc != codeloc(program)) {
			evalcmdlist(e->actions);
		    }
		} else if (s == pcsym) {
		    eval(place);
		    bp = bp_alloc(e, pop(Address), 0, e->actions);
		} else {
		    condbp(e);
		}
	    } else {
		condbp(e);
	    }
	    break;

	/*
	 * These should be handled specially.
	 * But for now I'm ignoring the problem.
	 */
	case O_AND:
	case O_OR:
	default:
	    condbp(e);
	    break;
    }
}

/*
 * Create a breakpoint for a condition that cannot be pinpointed
 * to happening at a particular address, but one for which we
 * must single step and check the condition after each statement.
 */

private condbp(e)
Event e;
{
    Symbol p;
    Breakpoint bp;
    Cmdlist actions;

    p = tcontainer(e->condition);
    if (p == nil) {
	p = program;
    }
    actions = buildcmdlist(build(O_IF, e->condition, e->actions));
    actions = buildcmdlist(build(O_TRACEON, false, actions));
    bp = bp_alloc(e, codeloc(p), 0, actions);
}

/*
 * Determine the deepest nested subprogram that still contains
 * all elements in the given expression.
 */

public Symbol tcontainer(exp)
Node exp;
{
    Integer i;
    Symbol s, t, u, v;

    checkref(exp);
    s = nil;
    if (exp->op == O_SYM) {
	s = container(exp->value.sym);
    } else if (not isleaf(exp->op)) {
	for (i = 0; i < nargs(exp->op); i++) {
	    t = tcontainer(exp->value.arg[i]);
	    if (t != nil) {
		if (s == nil) {
		    s = t;
		} else {
		    u = s;
		    v = t;
		    while (u != v and u != nil) {
			u = container(u);
			v = container(v);
		    }
		    if (u == nil) {
			panic("bad ancestry for \"%s\"", symname(s));
		    } else {
			s = u;
		    }
		}
	    }
	}
    }
    return s;
}

/*
 * Print out what's currently being traced by looking at
 * the currently active events.
 *
 * Some convolution here to translate internal representation
 * of events back into something more palatable.
 */

public status()
{
    Event e;
    Command cmd;

    foreach (Event, e, eventlist)
	if (not e->temporary) {
	    if (not isredirected()) {
		printf("(%d) ", e->id);
	    }
	    cmd = list_element(Command, list_head(e->actions));
	    if (cmd->op == O_PRINTCALL) {
		printf("trace ");
		printname(stdout, cmd->value.sym);
	    } else {
		if (list_size(e->actions) > 1) {
		    printf("{ ");
		}
		foreach (Command, cmd, e->actions)
		    printcmd(stdout, cmd);
		    if (not list_islast()) {
			printf("; ");
		    }
		endfor
		if (list_size(e->actions) > 1) {
		    printf(" }");
		}
		printcond(e->condition);
	    }
	    printf("\n");
	}
    endfor
}

/*
 * Print out a condition.
 */

private printcond(cond)
Node cond;
{
    Symbol s;
    Node place;

    if (cond->op == O_EQ and cond->value.arg[0]->op == O_SYM) {
	s = cond->value.arg[0]->value.sym;
	place = cond->value.arg[1];
	if (s == procsym) {
	    if (place->value.sym != program) {
		printf(" in ");
		printname(stdout, place->value.sym);
	    }
	} else if (s == linesym) {
	    printf(" at ");
	    prtree(stdout, place);
	} else if (s == pcsym or s == retaddrsym) {
	    printf("i at ");
	    prtree(stdout, place);
	} else {
	    printf(" when ");
	    prtree(stdout, cond);
	}
    } else {
	printf(" when ");
	prtree(stdout, cond);
    }
}

/*
 * Add a breakpoint to the list and return it.
 */

private Breakpoint bp_alloc(e, addr, line, actions)
Event e;
Address addr;
Lineno line;
Cmdlist actions;
{
    register Breakpoint p;

    p = new(Breakpoint);
    p->event = e;
    p->bpaddr = addr;
    p->bpline = line;
    p->actions = actions;
    if (tracebpts) {
	printf("new bp at 0x%x\n", addr);
	fflush(stdout);
    }
    bplist_append(p, bplist);
    return p;
}

/*
 * Free all storage in the event and breakpoint tables.
 */

public bpfree()
{
    register Event e;

    fixbps();
    foreach (Event, e, eventlist)
	delevent(e->id);
	list_delete(list_curitem(eventlist), eventlist);
    endfor
}

/*
 * Determine if the program stopped at a known breakpoint
 * and if so do the associated commands.
 */

public Boolean bpact()
{
    register Breakpoint p;
    Boolean found;

    found = false;
    foreach (Breakpoint, p, bplist)
	if (p->bpaddr == pc) {
	    if (tracebpts) {
		printf("breakpoint found at location 0x%x\n", pc);
	    }
	    found = true;
	    if (p->event->temporary) {
		delevent(p->event->id);
	    }
	    evalcmdlist(p->actions);
	}
    endfor
    if (isstopped) {
	printstatus();
    }
    fflush(stdout);
    return found;
}

/*
 * Begin single stepping and executing the given commands after each step.
 * If the first argument is true step by instructions, otherwise
 * step by source lines.
 *
 * We automatically set a breakpoint at the end of the current procedure
 * to turn off the given tracing.
 */

public traceon(inst, event, cmdlist)
Boolean inst;
Event event;
Cmdlist cmdlist;
{
    register Trcmd trcmd;
    Breakpoint bp;
    Node until;
    Cmdlist actions;

    trcmd = new(Trcmd);
    ++trid;
    trcmd->trid = trid;
    trcmd->event = event;
    trcmd->cmdlist = cmdlist;
    single_stepping = true;
    if (inst) {
	inst_tracing = true;
	list_append(list_item(trcmd), nil, eachinst);
    } else {
	list_append(list_item(trcmd), nil, eachline);
    }
    until = build(O_EQ, build(O_SYM, pcsym), build(O_LCON, return_addr()));
    actions = buildcmdlist(build(O_TRACEOFF, trcmd->trid));
    event_once(until, actions);
    if (tracebpts) {
	printf("adding trace %d for event %d\n", trcmd->trid, event->id);
    }
}

/*
 * Turn off some kind of tracing.
 * Strictly an internal command, this cannot be invoked by the user.
 */

public traceoff(id)
Integer id;
{
    register Trcmd t;
    register Boolean found;

    found = false;
    foreach (Trcmd, t, eachline)
	if (t->trid == id) {
	    printrmtr(t);
	    list_delete(list_curitem(eachline), eachline);
	    found = true;
	    break;
	}
    endfor
    if (not found) {
	foreach (Trcmd, t, eachinst)
	    if (t->event->id == id) {
		printrmtr(t);
		list_delete(list_curitem(eachinst), eachinst);
		found = true;
		break;
	    }
	endfor
	if (not found) {
	    panic("missing trid %d", id);
	}
    }
    if (list_size(eachinst) == 0) {
	inst_tracing = false;
	if (list_size(eachline) == 0) {
	    single_stepping = false;
	}
    }
}

/*
 * If breakpoints are being traced, note that a Trcmd is being deleted.
 */

private printrmtr(t)
Trcmd t;
{
    if (tracebpts) {
	printf("removing trace %d for event %d\n", t->trid, t->event->id);
    }
}

/*
 * Print out news during single step tracing.
 */

public printnews()
{
    register Trcmd t;

    foreach (Trcmd, t, eachline)
	evalcmdlist(t->cmdlist);
    endfor
    foreach (Trcmd, t, eachinst)
	evalcmdlist(t->cmdlist);
    endfor
    bpact();
}

/*
 * A procedure call/return has occurred while single-stepping,
 * note it if we're tracing lines.
 */

private Boolean chklist();

public callnews(iscall)
Boolean iscall;
{
    if (not chklist(eachline, iscall)) {
	chklist(eachinst, iscall);
    }
}

private Boolean chklist(list, iscall)
List list;
Boolean iscall;
{
    register Trcmd t;
    register Command cmd;

    foreach (Trcmd, t, list)
	foreach (Command, cmd, t->cmdlist)
	    if (cmd->op == O_PRINTSRCPOS and
	      (cmd->value.arg[0] == nil or cmd->value.arg[0]->op == O_QLINE)) {
		curfunc = whatblock(pc);
		if (iscall) {
		    printentry(curfunc);
		} else {
		    printexit(curfunc);
		}
		return true;
	    }
	endfor
    endfor
    return false;
}

/*
 * When tracing variables we keep a copy of their most recent value
 * and compare it to the current one each time a breakpoint occurs.
 * MAXTRSIZE is the maximum size variable we allow.
 */

#define MAXTRSIZE 512

/*
 * List of variables being watched.
 */

typedef struct Trinfo *Trinfo;

struct Trinfo {
    Node variable;
    Address traddr;
    Symbol trblock;
    char *trvalue;
};

private List trinfolist;

/*
 * Find the trace information record associated with the given record.
 * If there isn't one then create it and add it to the list.
 */

private Trinfo findtrinfo(p)
Node p;
{
    register Trinfo tp;
    Boolean isnew;

    isnew = true;
    if (trinfolist == nil) {
	trinfolist = list_alloc();
    } else {
	foreach (Trinfo, tp, trinfolist)
	    if (tp->variable == p) {
		isnew = false;
		break;
	    }
	endfor
    }
    if (isnew) {
	if (tracebpts) {
	    printf("adding trinfo for \"");
	    prtree(stdout, p);
	    printf("\"\n");
	}
	tp = new(Trinfo);
	tp->variable = p;
	tp->traddr = lval(p);
	tp->trvalue = nil;
	list_append(list_item(tp), nil, trinfolist);
    }
    return tp;
}

/*
 * Print out the value of a variable if it has changed since the
 * last time we checked.
 */

public printifchanged(p)
Node p;
{
    register Trinfo tp;
    register int n;
    char buff[MAXTRSIZE];
    static Lineno prevline;

    tp = findtrinfo(p);
    n = size(p->nodetype);
    dread(buff, tp->traddr, n);
    if (tp->trvalue == nil) {
	tp->trvalue = newarr(char, n);
	mov(buff, tp->trvalue, n);
	mov(buff, sp, n);
	sp += n;
	printf("initially (at line %d):\t", curline);
	prtree(stdout, p);
	printf(" = ");
	printval(p->nodetype);
	putchar('\n');
    } else if (cmp(tp->trvalue, buff, n) != 0) {
	mov(buff, tp->trvalue, n);
	mov(buff, sp, n);
	sp += n;
	printf("after line %d:\t", prevline);
	prtree(stdout, p);
	printf(" = ");
	printval(p->nodetype);
	putchar('\n');
    }
    prevline = curline;
}

/*
 * Stop if the value of the given expression has changed.
 */

public stopifchanged(p)
Node p;
{
    register Trinfo tp;
    register int n;
    char buff[MAXTRSIZE];
    static Lineno prevline;

    tp = findtrinfo(p);
    n = size(p->nodetype);
    dread(buff, tp->traddr, n);
    if (tp->trvalue == nil) {
	tp->trvalue = newarr(char, n);
	mov(buff, tp->trvalue, n);
	isstopped = true;
    } else if (cmp(tp->trvalue, buff, n) != 0) {
	mov(buff, tp->trvalue, n);
	isstopped = true;
    }
    prevline = curline;
}

/*
 * Free the tracing table.
 */

public trfree()
{
    register Trinfo tp;

    foreach (Trinfo, tp, trinfolist)
	dispose(tp->trvalue);
	dispose(tp);
	list_delete(list_curitem(trinfolist), trinfolist);
    endfor
}

/*
 * Fix up breakpoint information before continuing execution.
 *
 * It's necessary to destroy events and breakpoints that were created
 * temporarily and still exist because the program terminated abnormally.
 */

public fixbps()
{
    register Event e;
    register Trcmd t;

    single_stepping = false;
    inst_tracing = false;
    trfree();
    foreach (Event, e, eventlist)
	if (e->temporary) {
	    delevent(e->id);
	}
    endfor
    foreach (Trcmd, t, eachline)
	printrmtr(t);
	list_delete(list_curitem(eachline), eachline);
    endfor
    foreach (Trcmd, t, eachinst)
	printrmtr(t);
	list_delete(list_curitem(eachinst), eachinst);
    endfor
}

/*
 * Set all breakpoints in object code.
 */

public setallbps()
{
    register Breakpoint p;

    foreach (Breakpoint, p, bplist)
	setbp(p->bpaddr);
    endfor
}

/*
 * Undo damage done by "setallbps".
 */

public unsetallbps()
{
    register Breakpoint p;

    foreach (Breakpoint, p, bplist)
	unsetbp(p->bpaddr);
    endfor
}
