/* walk.c: walks the parse tree. */

#include <signal.h>
#include <setjmp.h>
#include "rc.h"
#include "jbwrap.h"

/*
   global which indicates whether rc is executing a test;
   used by rc -e so that if (false) does not exit.
*/
bool cond = FALSE;

static bool isallpre(Node *);
static bool dofork(bool);
static void dopipe(Node *);

/* Tail-recursive version of walk() */

#define WALK(x, y) { n = x; parent = y; goto top; }

/* walk the parse-tree. "obvious". */

extern bool walk(Node *n, bool parent) {
top:	SIGCHK;
	if (n == NULL) {
		if (!parent)
			exit(0);
		set(TRUE);
		return TRUE;
	}
	switch (n->type) {
	case nArgs: case nBackq: case nConcat: case nCount:
	case nFlat: case nLappend: case nRedir: case nVar:
	case nVarsub: case nWord: case nQword:
		exec(glob(glom(n)), parent);	/* simple command */
		break;
	case nBody:
		walk(n->u[0].p, TRUE);
		WALK(n->u[1].p, parent);
		/* WALK doesn't fall through */
	case nNowait: {
		int pid;
		if ((pid = rc_fork()) == 0) {
			setsigdefaults(FALSE);
			rc_signal(SIGTTOU, SIG_IGN);	/* Berkeleyized version: put it in a new pgroup. */
			rc_signal(SIGTTIN, SIG_IGN);
			rc_signal(SIGTSTP, SIG_IGN);
			setpgrp(0, getpid());
			mvfd(rc_open("/dev/null", rFrom), 0);
			walk(n->u[0].p, FALSE);
			exit(getstatus());
		}
		if (interactive)
			fprint(2, "%d\n", pid);
		varassign("apid", word(nprint("%d", pid), NULL), FALSE);
		redirq = NULL; /* kill pre-redir queue */
		break;
	}
	case nAndalso: {
		bool oldcond = cond;
		cond = TRUE;
		if (walk(n->u[0].p, TRUE)) {
			cond = oldcond;
			WALK(n->u[1].p, parent);
		} else
			cond = oldcond;
		break;
	}
	case nOrelse: {
		bool oldcond = cond;
		cond = TRUE;
		if (!walk(n->u[0].p, TRUE)) {
			cond = oldcond;
			WALK(n->u[1].p, parent);
		} else
			cond = oldcond;
		break;
	}
	case nBang:
		set(!walk(n->u[0].p, TRUE));
		break;
	case nIf: {
		bool oldcond = cond;
		Node *true_cmd = n->u[1].p, *false_cmd = NULL;
		if (true_cmd != NULL && true_cmd->type == nElse) {
			false_cmd = true_cmd->u[1].p;
			true_cmd = true_cmd->u[0].p;
		}
		cond = TRUE;
		if (!walk(n->u[0].p, TRUE))
			true_cmd = false_cmd; /* run the else clause */
		cond = oldcond;
		WALK(true_cmd, parent);
	}
	case nWhile: {
		Jbwrap j;
		Edata jbreak;
		Estack e1, e2;
		bool testtrue, oldcond = cond;
		cond = TRUE;
		if (!walk(n->u[0].p, TRUE)) { /* prevent spurious breaks inside test */
			cond = oldcond;
			break;
		}
		if (setjmp(j.j))
			break;
		jbreak.jb = &j;
		except(eBreak, jbreak, &e1);
		do {
			Edata block;
			block.b = newblock();
			cond = oldcond;
			except(eArena, block, &e2);
			walk(n->u[1].p, TRUE);
			testtrue = walk(n->u[0].p, TRUE);
			unexcept(); /* eArena */
			cond = TRUE;
		} while (testtrue);
		cond = oldcond;
		unexcept(); /* eBreak */
		break;
	}
	case nForin: {
		List *l, *var = glom(n->u[0].p);
		Jbwrap j;
		Estack e1, e2;
		Edata jbreak;
		if (setjmp(j.j))
			break;
		jbreak.jb = &j;
		except(eBreak, jbreak, &e1);
		for (l = listcpy(glob(glom(n->u[1].p)), nalloc); l != NULL; l = l->n) {
			Edata block;
			assign(var, word(l->w, NULL), FALSE);
			block.b = newblock();
			except(eArena, block, &e2);
			walk(n->u[2].p, TRUE);
			unexcept(); /* eArena */
		}
		unexcept(); /* eBreak */
		break;
	}
	case nSubshell:
		if (dofork(TRUE)) {
			setsigdefaults(FALSE);
			walk(n->u[0].p, FALSE);
			rc_exit(getstatus());
		}
		break;
	case nAssign:
		if (n->u[0].p == NULL)
			rc_error("null variable name");
		assign(glom(n->u[0].p), glob(glom(n->u[1].p)), FALSE);
		set(TRUE);
		break;
	case nPipe:
		dopipe(n);
		break;
	case nNewfn: {
		List *l = glom(n->u[0].p);
		if (l == NULL)
			rc_error("null function name");
		while (l != NULL) {
			if (dashex)
				prettyprint_fn(2, l->w, n->u[1].p);
			fnassign(l->w, n->u[1].p);
			l = l->n;
		}
		set(TRUE);
		break;
	}
	case nRmfn: {
		List *l = glom(n->u[0].p);
		while (l != NULL) {
			if (dashex)
				fprint(2, "fn %S\n", l->w);
			fnrm(l->w);
			l = l->n;
		}
		set(TRUE);
		break;
	}
	case nDup:
		redirq = NULL;
		break; /* Null command */
	case nMatch: {
		List *a = glob(glom(n->u[0].p)), *b = glom(n->u[1].p);
		if (dashex)
			fprint(2, (a != NULL && a->n != NULL) ? "~ (%L) %L\n" : "~ %L %L\n", a, " ", b, " ");
		set(lmatch(a, b));
		break;
	}
	case nSwitch: {
		List *v = glom(n->u[0].p);
		while (1) {
			do {
				n = n->u[1].p;
				if (n == NULL)
					return istrue();
			} while (n->u[0].p == NULL || n->u[0].p->type != nCase);
			if (lmatch(v, glom(n->u[0].p->u[0].p))) {
				for (n = n->u[1].p; n != NULL && (n->u[0].p == NULL || n->u[0].p->type != nCase); n = n->u[1].p)
					walk(n->u[0].p, TRUE);
				break;
			}
		}
		break;
	}
	case nPre: {
		List *v;
		if (n->u[0].p->type == nRedir || n->u[0].p->type == nDup) {
			qredir(n->u[0].p);
			walk(n->u[1].p, parent);
		} else if (n->u[0].p->type == nAssign) {
			if (isallpre(n->u[1].p)) {
				walk(n->u[0].p, TRUE);
				WALK(n->u[1].p, parent);
			} else {
				Estack e;
				Edata var;
				v = glom(n->u[0].p->u[0].p);
				assign(v, glob(glom(n->u[0].p->u[1].p)), TRUE);
				var.name = v->w;
				except(eVarstack, var, &e);
				walk(n->u[1].p, parent);
				varrm(v->w, TRUE);
				unexcept(); /* eVarstack */
			}
		} else
			panic("unexpected node in preredir section of walk");
		break;
	}
	case nBrace:
		if (n->u[1].p == NULL) {
			WALK(n->u[0].p, parent);
		} else if (dofork(parent)) {
			setsigdefaults(FALSE);
			walk(n->u[1].p, TRUE); /* Do redirections */
			redirq = NULL;   /* Reset redirection queue */
			walk(n->u[0].p, FALSE); /* Do commands */
			rc_exit(getstatus());
			/* NOTREACHED */
		}
		break;
	case nEpilog:
		qredir(n->u[0].p);
		if (n->u[1].p != NULL) {
			WALK(n->u[1].p, parent); /* Do more redirections. */
		} else {
			doredirs();	/* Okay, we hit the bottom. */
		}
		break;
	case nNmpipe:
		rc_error("named pipes cannot be executed as commands");
		/* NOTREACHED */
	default:
		panic("unknown node in walk");
		/* NOTREACHED */
	}
	return istrue();
}

/* checks to see whether a subtree is all pre-command directives, i.e., assignments and redirs only */

static bool isallpre(Node *n) {
	while (n != NULL && n->type == nPre)
		n = n->u[1].p;
	return n == NULL || n->type == nRedir || n->type == nAssign || n->type == nDup;
}

/*
   A code-saver. Forks, child returns (for further processing in walk()), and the parent
   waits for the child to finish, setting $status appropriately.
*/

static bool dofork(bool parent) {
	int pid, sp;

	if (!parent || (pid = rc_fork()) == 0)
		return TRUE;
	redirq = NULL; /* clear out the pre-redirection queue in the parent */
	rc_wait4(pid, &sp, TRUE);
	setstatus(-1, sp);
	SIGCHK;
	return FALSE;
}

static void dopipe(Node *n) {
	int i, j, sp, pid, fd_prev, fd_out, pids[512], stats[512], p[2];
	bool intr;
	Node *r;

	fd_prev = fd_out = 1;
	for (r = n, i = 0; r != NULL && r->type == nPipe; r = r->u[2].p, i++) {
		if (i > 500) /* the only hard-wired limit in rc? */
			rc_error("pipe too long");
		if (pipe(p) < 0) {
			uerror("pipe");
			rc_error(NULL);
		}
		if ((pid = rc_fork()) == 0) {
			setsigdefaults(FALSE);
			redirq = NULL; /* clear preredir queue */
			mvfd(p[0], r->u[1].i);
			if (fd_prev != 1)
				mvfd(fd_prev, fd_out);
			close(p[1]);
			walk(r->u[3].p, FALSE);
			exit(getstatus());
		}
		if (fd_prev != 1)
			close(fd_prev); /* parent must close all pipe fd's */
		pids[i] = pid;
		fd_prev = p[1];
		fd_out = r->u[0].i;
		close(p[0]);
	}
	if ((pid = rc_fork()) == 0) {
		setsigdefaults(FALSE);
		mvfd(fd_prev, fd_out);
		walk(r, FALSE);
		exit(getstatus());
		/* NOTREACHED */
	}
	redirq = NULL; /* clear preredir queue */
	close(fd_prev);
	pids[i++] = pid;

	/* collect statuses */

	intr = FALSE;
	for (j = 0; j < i; j++) {
		rc_wait4(pids[j], &sp, TRUE);
		stats[j] = sp;
		intr |= (sp == SIGINT);
	}
	setpipestatus(stats, i);
	SIGCHK;
}
