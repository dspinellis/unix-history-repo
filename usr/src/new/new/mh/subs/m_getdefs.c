#include "mh.h"
#include <stdio.h>

char    defpath[128];

struct procs {
	char    *procname;
	char    **procnaddr;
} procs [] = {
	{ "lsproc",     &lsproc         },
	{ "mh_deliver", &mh_deliver     },
	{ "prproc",     &prproc         },
	{ "scanproc",   &scanproc       },
	{ "showproc",   &showproc       },
	{ "sendproc",   &sendproc       },
	{ "fileproc",   &fileproc       },
	{ 0,            0               },
};

m_getdefs()
{
	register struct node *np;
	register int state, wpid, pid;
	register struct procs *ps;
	int status;
	FILE *ib;
	char name[NAMESZ], field[128];

	if(defpath[0])
		return;         /* We've already been called!   */
	if(!mypath)
		if((mypath = getenv("HOME")) == NULL) {
			fprintf(stderr, "HOME environment variable not set!\n");
			done(1);
		}
	sprintf(defpath, "%s%s", mypath, mh_prof);
/***    copy(mh_prof, copy(mypath, defpath));           ***/

	if((ib = fopen(defpath, "r")) == NULL) {
		if((pid = fork()) == 0) {
			execl(installproc, "install-mh", "-auto", 0);
			fprintf(stderr, "Can't exec ");perror(installproc);
			done(1);
		} else if(pid == -1) {
			fprintf(stderr, "No forks!\n");
			done(1);
		} else
			while((wpid = wait(&status)) != -1 && wpid != pid)
				;
		if(status || (ib = fopen(defpath, "r")) == NULL) {
			fprintf(stderr, "[install-mh aborted]\n");
			done(1);
		}
	}

#ifdef NEWS                     /* NOT CONVERTED TO V7!!! */
	fstat(fildes(ib), field);
	deftime = (&field)->i_atime;
#endif

	np = (struct node *) &m_defs;
	state = FLD;
    for(;;)
	switch(state = m_getfld(state,name,field,sizeof field,ib)) {
	case FLD:
	case FLDEOF:
		np->n_next = (struct node *) malloc(sizeof *np);
		np = np->n_next;
		np->n_name = getcpy(name);
		np->n_field = trimcpy(field);
		np->n_next = 0;
		for(ps = procs; ps->procname; ps++)
			if(strcmp(np->n_name, ps->procname) == 0) {
				*ps->procnaddr = np->n_field;
				break;
			}
		if(state == FLDEOF) {
			fclose(ib);
			return(0);
		}
		continue;
	case BODY:
	case BODYEOF:
		fprintf(stderr, ".mh_profile must not contain a body--it can't \
end with a blank line!\n");
	default:
		fprintf(stderr, "Bad format: .mh_profile!\n");
		done(1);
	}
}
