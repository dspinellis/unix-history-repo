/* redir.c: code for opening files and piping heredocs after fork but before exec. */

#include "rc.h"

/*
   Walk the redirection queue, and open files and dup2 to them. Also,
   here-documents are treated here by dumping them down a pipe. (this
   should make here-documents fast on systems with lots of memory which
   do pipes right. Under sh, a file is copied to /tmp, and then read
   out of /tmp again. I'm interested in knowing how much faster, say,
   shar runs when unpacking when invoked with rc instead of sh. On my
   sun4/280, it runs in about 60-75% of the time of sh for unpacking
   the rc source distribution.)
*/

extern void doredirs() {
	List *fname;
	int fd, p[2];
	Rq *r;
	for (r = redirq; r != NULL; r = r->n) {
		switch(r->r->type) {
		default:
			panic("unexpected node in doredirs");
			/* NOTREACHED */
		case nRedir:
			if (r->r->u[0].i == rHerestring) {
				fname = flatten(glom(r->r->u[2].p)); /* fname is really a string */
				if (pipe(p) < 0) {
					uerror("pipe");
					rc_error(NULL);
				}
				if (rc_fork() == 0) { /* child writes to pipe */
					setsigdefaults(FALSE);
					close(p[0]);
					if (fname != NULL)
						writeall(p[1], fname->w, strlen(fname->w));
					exit(0);
				} else {
					close(p[1]);
					if (mvfd(p[0], r->r->u[1].i) < 0)
						rc_error(NULL);
				}
			} else {
				fname = glob(glom(r->r->u[2].p));
				if (fname == NULL)
					rc_error("null filename in redirection");
				if (fname->n != NULL)
					rc_error("multi-word filename in redirection");
				switch (r->r->u[0].i) {
				default:
					panic("unexpected node in doredirs");
					/* NOTREACHED */
				case rCreate: case rAppend: case rFrom:
					fd = rc_open(fname->w, r->r->u[0].i);
					break;
				}
				if (fd < 0) {
					uerror(fname->w);
					rc_error(NULL);
				}
				if (mvfd(fd, r->r->u[1].i) < 0)
					rc_error(NULL);
			}
			break;
		case nDup:
			if (r->r->u[2].i == -1)
				close(r->r->u[1].i);
			else if (r->r->u[2].i != r->r->u[1].i) {
				if (dup2(r->r->u[2].i, r->r->u[1].i) < 0) {
					uerror("dup2");
					rc_error(NULL);
				}
			}
		}
	}
	redirq = NULL;
}
