/* status.c: functions for printing fancy status messages in rc */

#include "rc.h"
#include "sigmsgs.h"

/* status == the wait() value of the last command in the pipeline, or the last command */

static int statuses[512];
static int pipelength = 1;

/*
   Test to see if rc's status is true. According to td, status is true
   if and only if every pipe-member has an exit status of zero.
*/

extern int istrue() {
	int i;
	for (i = 0; i < pipelength; i++)
		if (statuses[i] != 0)
			return FALSE;
	return TRUE;
}

/*
   Return the status as an integer. A status which has low-bits set is
   a signal number, whereas a status with high bits set is a value set
   from exit(). The presence of a signal just sets status to 1. Also,
   a pipeline with nonzero exit statuses in it just sets status to 1.
*/

extern int getstatus() {
	int s;
	if (pipelength > 1)
		return !istrue();
	s = statuses[0];
	return (s&0xff) ? 1 : (s >> 8) & 0xff;
}

extern void set(bool code) {
	setstatus(-1, (!code) << 8); /* exit status 1 == 0x100 */
}

/* take a pipeline and store the exit statuses. Check to see whether any of the children dumped core */

extern void setpipestatus(int stats[], int num) {
	int i;
	for (i = 0; i < (pipelength = num); i++) {
		statprint(-1, stats[i]);
		statuses[i] = stats[i];
	}
}

/* set a simple status, as opposed to a pipeline */

extern void setstatus(int pid, int i) {
	pipelength = 1;
	statuses[0] = i;
	statprint(pid, i);
}

/* print a message if termination was with a signal, and if the child dumped core. exit on error if -e is set */

extern void statprint(int pid, int i) {
	if (i & 0xff) {
		char *msg = ((i & 0x7f) < NUMOFSIGNALS ? signals[i & 0x7f].msg : "");
		if (pid != -1)
			fprint(2, "%d: ", pid);
		if (i & 0x80) {
			if (*msg == '\0')
				fprint(2, "core dumped\n");
			else
				fprint(2, "%s--core dumped\n", msg);
		} else if (*msg != '\0')
			fprint(2, "%s\n", msg);
	}
	if (i != 0 && dashee && !cond)
		rc_exit(getstatus());
}

/* prepare a list to be passed back. Used whenever $status is dereferenced */

extern List *sgetstatus() {
	List *r;
	int i;
	for (r = NULL, i = 0; i < pipelength; i++) {
		List *q = nnew(List);
		int s = statuses[i];
		int t;
		q->n = r;
		r = q;
		if ((t = s & 0x7f) != 0) {
			const char *core = (s & 0x80) ? "+core" : "";
			if (t < NUMOFSIGNALS && *signals[t].name != '\0')
				r->w = nprint("%s%s", signals[t].name, core);
			else
				r->w = nprint("-%d%s", t, core); /* unknown signals are negated */
		} else
			r->w = nprint("%d", (s >> 8) & 0xff);
		r->m = NULL;
	}
	return r;
}

extern void ssetstatus(char **av) {
	int i, j, k, l;
	bool found;
	for (l = 0; av[l] != NULL; l++)
		; /* count up array length */
	--l;
	for (i = 0; av[i] != NULL; i++) {
		j = a2u(av[i]);
		if (j >= 0) {
			statuses[l - i] = j << 8;
			continue;
		}
		found = FALSE;
		for (k = 0; k < NUMOFSIGNALS; k++) {
			if (streq(signals[k].name, av[i])) {
				statuses[l - i] = k;
				found = TRUE;
				break;
			} 
			else {
				size_t len = strlen(signals[k].name);
				if (strncmp(signals[k].name, av[i], len) == 0 && streq(av[i] + len, "+core")) {
					statuses[l - i] = k + 0x80;
					found = TRUE;
					break;
				}
			}
		}
		if (!found) {
			fprint(2, "bad status\n");
			set(FALSE);
			return;
		}
	}
	pipelength = i;
}
