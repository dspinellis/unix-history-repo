#include <setjmp.h>
#include <signal.h>
#include "rc.h"
#include "jbwrap.h"

/*
   A return goes back stack frames to the last return. A break does
   not. A signal goes to the last interactive level. (see below)
*/

bool nl_on_intr = TRUE;

static Estack *estack;

/* add an exception to the input stack. */

extern void except(ecodes e, Edata data, Estack *ex) {
	ex->prev = estack;
	estack = ex;
	estack->e = e;
	estack->data = data;
	if (e == eError || e == eBreak || e == eReturn)
		estack->interactive = interactive;
}

/* remove an exception, restore last interactive value */

extern void unexcept() {
	switch (estack->e) {
	default:
		break;
	case eError:
		interactive = estack->interactive;
		break;
	case eArena:
		restoreblock(estack->data.b);
		break;
	case eFifo:
		unlink(estack->data.name);
		break;
	case eFd:
		close(estack->data.fd);
		break;
	}
	estack = estack->prev;
}

/*
   Raise an exception. The rules are pretty complicated: you can return
   from a loop inside a function, but you can't break from a function
   inside of a loop. On errors, rc_raise() goes back to the LAST
   INTERACTIVE stack frame. If no such frame exists, then rc_raise()
   exits the shell.  This is what happens, say, when there is a syntax
   error in a noninteractive shell script. While traversing the
   exception stack backwards, rc_raise() also removes input sources
   (closing file-descriptors, etc.) and pops instances of variables
   that have been pushed onto the variable stack (e.g., for a function
   call (for $*) or a local assignment).
*/

extern void rc_raise(ecodes e) {
	if (e == eError && rc_pid != getpid())
		exit(1); /* child processes exit on an error/signal */
	for (; estack != NULL; estack = estack->prev)
		if (estack->e != e) {
			if (e == eBreak && estack->e != eArena)
				rc_error("break outside of loop");
			else if (e == eReturn && estack->e == eError) /* can return from loops inside functions */
				rc_error("return outside of function");
			switch (estack->e) {
			default:
				break;
			case eVarstack:
				varrm(estack->data.name, TRUE);
				break;
			case eArena:
				restoreblock(estack->data.b);
				break;
			case eFifo:
				unlink(estack->data.name);
				break;
			case eFd:
				close(estack->data.fd);
				break;
			}
		} else {
			if (e == eError && !estack->interactive) {
				popinput();
			} else {
				Jbwrap *j = estack->data.jb;

				interactive = estack->interactive;
				estack = estack->prev;
				longjmp(j->j, 1);
			}
		}
	rc_exit(1); /* top of exception stack */
}

extern bool outstanding_cmdarg() {
	return estack->e == eFifo || estack->e == eFd;
}

extern void pop_cmdarg(bool remove) {
	for (; estack != NULL; estack = estack->prev)
		switch (estack->e) {
		case eFifo:
			if (remove)
				unlink(estack->data.name);
			break;
		case eFd:
			if (remove)
				close(estack->data.fd);
			break;
		default:
			return;
		}
}

/* exception handlers */

extern void rc_error(char *s) {
	pr_error(s);
	set(FALSE);
	redirq = NULL;
	cond = FALSE; /* no longer inside conditional */
	rc_raise(eError);
}

extern void sigint(int s) {
	if (s != SIGINT)
		panic("s != SIGINT in sigint catcher");
	/* this is the newline you see when you hit ^C while typing a command */
	if (nl_on_intr)
		fprint(2, "\n");
	nl_on_intr = TRUE;
	redirq = NULL;
	cond = FALSE;
	rc_raise(eError);
}
