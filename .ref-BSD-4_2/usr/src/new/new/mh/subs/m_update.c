#include "mh.h"
#include <stdio.h>
#include <signal.h>

char    defpath[];

m_update()
{
	FILE *out;
	register struct node *np;
	int save;

	if(def_flags & DEFMOD) {
		save = (int) signal(SIGINT, SIG_IGN);
		if((out = fopen(defpath, "w")) == NULL) {
			fprintf(stderr, "Can't create %s!!\n", defpath);
			done(1);
		}
		for(np = m_defs; np; np = np->n_next)
			fprintf(out, "%s: %s\n", np->n_name, np->n_field);
		fclose(out);
		signal(SIGINT, (int (*)()) save);
		def_flags &= ~DEFMOD;
	}
}
