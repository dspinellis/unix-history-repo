/*
   fn.c: functions for adding and deleting functions from the symbol table.
   Support for signal handlers is also found here.
*/

#include <signal.h>
#include <errno.h>
#include "rc.h"
#include "sigmsgs.h"

static void fn_handler(int), dud_handler(int);

static bool runexit = FALSE;
static Node *handlers[NUMOFSIGNALS], null;
static void (*def_sigint)(int) = SIG_DFL,
	    (*def_sigquit)(int) = SIG_DFL,
	    (*def_sigterm)(int) = SIG_DFL;

/*
   Set signals to default values for rc. This means that interactive
   shells ignore SIGTERM, etc.
*/

extern void inithandler() {
	int i;
	null.type = nBody;
	null.u[0].p = null.u[1].p = NULL;
	for (i = 1; i < NUMOFSIGNALS; i++)
		if (sighandlers[i] == SIG_IGN)
			fnassign(signals[i].name, NULL); /* ignore incoming ignored signals */
	if (interactive || sighandlers[SIGINT] != SIG_IGN) {
		def_sigint = sigint;
		fnrm("sigint"); /* installs SIGINT catcher if not inherited ignored */
	}
	if (!dashdee) {
		if (interactive || sighandlers[SIGQUIT] != SIG_IGN) {
			def_sigquit = dud_handler;
			fnrm("sigquit"); /* "ignores" SIGQUIT unless inherited ignored */
		}
		if (interactive) {
			def_sigterm = dud_handler;
			fnrm("sigterm"); /* ditto for SIGTERM */
		}
	}
}

/* only run this in a child process! resets signals to their default values */

extern void setsigdefaults(bool sysvbackground) {
	int i;
	/*
	   General housekeeping: setsigdefaults happens after fork(),
	   so it's a convenient place to clean up open file descriptors.
	   (history file, scripts, etc.)
	*/
	closefds();
	/*
	   Restore signals to SIG_DFL, paying close attention to
	   a few quirks: SIGINT, SIGQUIT and are treated specially
	   depending on whether we are doing v7-style backgrounding
	   or not; the default action for SIGINT, SIGQUIT and SIGTERM
	   must be set to the appropriate action; finally, care must
	   be taken not to set to SIG_DFL any signals which are being
	   ignored.
	*/
	for (i = 1; i < NUMOFSIGNALS; i++)
		if (sighandlers[i] != SIG_IGN) {
			handlers[i] = NULL;
			switch (i) {
			case SIGINT:
				if (sysvbackground) {
					def_sigint = SIG_IGN;
					fnassign("sigint", NULL); /* ignore */
				} else {
					def_sigint = SIG_DFL;
					goto sigcommon;
				}
				break;
			case SIGQUIT:
				if (sysvbackground) {
					def_sigquit = SIG_IGN;
					fnassign("sigquit", NULL); /* ignore */
				} else {
					def_sigquit = SIG_DFL;
					goto sigcommon;
				}
				break;
			case SIGTERM:
				def_sigterm = SIG_DFL;
				/* FALLTHROUGH */
			sigcommon:
			default:
				if (sighandlers[i] != SIG_DFL) {
					rc_signal(i, SIG_DFL);
					delete_fn(signals[i].name);
				}
			}
		}
	delete_fn("sigexit");
	runexit = FALSE; /* No sigexit on subshells */
}

/* rc's exit. if runexit is set, run the sigexit function. */

extern void rc_exit(int stat) {
	static char *sigexit[2] = {
		"sigexit",
		NULL
	};
	if (runexit) {
		runexit = FALSE;
		funcall(sigexit);
		stat = getstatus();
	}
	exit(stat);
}

/* The signal handler for all functions. calls walk() */

static void fn_handler(int s) {
	List *dollarzero;
	Estack e;
	Edata star;
	int olderrno;
	if (s < 1 || s >= NUMOFSIGNALS)
		panic("unknown signal");
	olderrno = errno;
	dollarzero = nnew(List);
	dollarzero->w = signals[s].name;
	dollarzero->n = NULL;
	varassign("*", dollarzero, TRUE);
	star.name = "*";
	except(eVarstack, star, &e);
	walk(handlers[s], TRUE);
	varrm("*", TRUE);
	unexcept(); /* eVarstack */
	errno = olderrno;
}

/* A dud signal handler for SIGQUIT and SIGTERM */

static void dud_handler(int s) {
}

/*
   Assign a function in Node form. Check to see if the function is also
   a signal, and set the signal vectors appropriately.
*/

extern void fnassign(char *name, Node *def) {
	Node *newdef = treecpy(def == NULL ? &null : def, ealloc); /* important to do the treecopy first */
	Function *new = get_fn_place(name);
	int i;
	new->def = newdef;
	new->extdef = NULL;
	if (strncmp(name, "sig", conststrlen("sig")) == 0) { /* slight optimization */
		if (streq(name, "sigexit"))
			runexit = TRUE;
		for (i = 1; i < NUMOFSIGNALS; i++) /* zero is a bogus signal */
			if (streq(signals[i].name, name)) {
				handlers[i] = newdef;
				if (def == NULL)
					rc_signal(i, SIG_IGN);
				else
					rc_signal(i, fn_handler);
				break;
			}
	}
}

/* Assign a function from the environment. Store just the external representation */

extern void fnassign_string(char *extdef) {
	char *name = get_name(extdef+3); /* +3 to skip over "fn_" */
	Function *new;
	if (name == NULL)
		return;
	new = get_fn_place(name);
	new->def = NULL;
	new->extdef = ecpy(extdef);
}

/* Return a function in Node form, evaluating an entry from the environment if necessary */

extern Node *fnlookup(char *name) {
	Function *look = lookup_fn(name);
	Node *ret;
	if (look == NULL)
		return NULL; /* not found */
	if (look->def != NULL)
		return look->def;
	if (look->extdef == NULL) /* function was set to null, e.g., fn foo {} */
		return &null;
	ret = parse_fn(name, look->extdef);
	if (ret == NULL) {
		efree(look->extdef);
		look->extdef = NULL;
		return &null;
	} else {
		return look->def = treecpy(ret, ealloc); /* Need to take it out of nalloc space */
	}
}

/* Return a function in string form (used by makeenv) */

extern char *fnlookup_string(char *name) {
	Function *look = lookup_fn(name);

	if (look == NULL)
		return NULL;
	if (look->extdef != NULL)
		return look->extdef;
	return look->extdef = fun2str(name, look->def);
}

/*
   Remove a function from the symbol table. If it also defines a signal
   handler, restore the signal handler to its default value.
*/

extern void fnrm(char *name) {
	int i;
	for (i = 1; i < NUMOFSIGNALS; i++)
		if (streq(signals[i].name, name)) {
			handlers[i] = NULL;
			switch (i) {
			case SIGINT:
				rc_signal(i, def_sigint);
				break;
			case SIGQUIT:
				rc_signal(i, def_sigquit);
				break;
			case SIGTERM:
				rc_signal(i, def_sigterm);
				break;
			default:
				rc_signal(i, SIG_DFL);
			}
		}
	if (streq(name, "sigexit"))
		runexit = FALSE;
	delete_fn(name);
}

extern void whatare_all_signals() {
	int i;
	for (i = 1; i < NUMOFSIGNALS; i++)
		if (*signals[i].name != '\0')
			if (sighandlers[i] == SIG_IGN)
				fprint(1, "fn %s {}\n", signals[i].name);
			else if (sighandlers[i] == fn_handler)
				fprint(1, "fn %S {%T}\n", signals[i].name, handlers[i]);
			else
				fprint(1, "fn %s\n", signals[i].name);
}

extern void prettyprint_fn(int fd, char *name, Node *n) {
	fprint(fd, "fn %S {%T}\n", name, n);
}
