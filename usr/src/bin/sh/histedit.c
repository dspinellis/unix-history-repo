/*
 * Editline and history functions (and glue).
 */
#include <stdio.h>
#include "parser.h"
#include "options.h"
#include "error.h"

#include "histedit.h"

History *hist;	/* history cookie */
EditLine *el;	/* editline cookie */
static FILE *el_in, *el_out;

extern int is_interactive;

/*
 * Set history and editing status.  Called whenever the status may
 * have changed (figures out what to do).
 */
histedit() {

	if (is_interactive) {
		if (!hist) {
			/*
			 * turn history on
			 */
			INTOFF;
			hist = history_init();
			INTON;

			if (hist != NULL)
				sethistsize();
			else
				out2str("sh: can't initialize history\n");
		}
		if (Eflag && !el && isatty(0)) { /* && isatty(2) ??? */
			/*
			 * turn editing on
			 */
			INTOFF;
			if (el_in == NULL)
				el_in = fdopen(0, "r");
			if (el_out == NULL)
				el_out = fdopen(2, "w");
			if (el_in == NULL || el_out == NULL)
				goto bad;
			el = el_init(arg0, el_in, el_out);
			if (el != NULL) {
				if (hist)
					el_set(el, EL_HIST, history_set, hist);
				el_set(el, EL_PROMPT, getprompt);
			} else {
bad:
				out2str("sh: can't initialize editing\n");
			}
			INTON;
			if (el)	/* XXX - -o vi etc... */
				el_set(el, EL_EDITOR, "vi");
		} else if (!Eflag && el) {
			INTOFF;
			el_end(el);
			el = NULL;
			INTON;
		}
	} else {
		INTOFF;
		if (el) {	/* no editing if not interactive */
			el_end(el);
			el = NULL;
		}
		if (hist) {
			history_end(hist);
			hist = NULL;
		}
		INTON;
	}
}

sethistsize() {
	char *cp;
	int histsize;

	if (hist != NULL) {
		cp = lookupvar("HISTSIZE");
		if (cp == NULL || *cp == '\0' || 
		   (histsize = atoi(cp)) < 0)
			histsize = 100;
		history_set(hist, H_EVENT, histsize);
	}
}
