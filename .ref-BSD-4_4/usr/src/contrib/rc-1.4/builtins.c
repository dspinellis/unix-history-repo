/* builtins.c: the collection of rc's builtin commands */

/*
	NOTE: rc's builtins do not call "rc_error" because they are
	commands, and rc errors usually arise from syntax errors. e.g.,
	you probably don't want interpretation of a shell script to stop
	because of a bad umask.
*/

#include <sys/ioctl.h>
#include <setjmp.h>
#include <errno.h>
#include "rc.h"
#include "jbwrap.h"
#include <sys/time.h>
#include <sys/resource.h>

extern int umask(int);

static void b_break(char **), b_cd(char **), b_eval(char **), b_exit(char **),
	b_newpgrp(char **), b_return(char **), b_shift(char **), b_umask(char **),
	b_wait(char **), b_whatis(char **);

static void b_limit(char **);
static void b_echo(char **);

static struct {
	builtin_t *p;
	char *name;
} builtins[] = {
	{ b_break,	"break" },
	{ b_builtin,	"builtin" },
	{ b_cd,		"cd" },
	{ b_echo,	"echo" },
	{ b_eval,	"eval" },
	{ b_exec,	"exec" },
	{ b_exit,	"exit" },
	{ b_limit,	"limit" },
	{ b_newpgrp,	"newpgrp" },
	{ b_return,	"return" },
	{ b_shift,	"shift" },
	{ b_umask,	"umask" },
	{ b_wait,	"wait" },
	{ b_whatis,	"whatis" },
	{ b_dot,	"." },
};

extern builtin_t *isbuiltin(char *s) {
	int i;
	for (i = 0; i < arraysize(builtins); i++)
		if (streq(builtins[i].name, s))
			return builtins[i].p;
	return NULL;
}

/* funcall() is the wrapper used to invoke shell functions. pushes $*, and "return" returns here. */

extern void funcall(char **av) {
	Jbwrap j;
	Estack e1, e2;
	Edata jreturn, star;
	if (setjmp(j.j))
		return;
	starassign(*av, av+1, TRUE);
	jreturn.jb = &j;
	star.name = "*";
	except(eReturn, jreturn, &e1);
	except(eVarstack, star, &e2);
	walk(treecpy(fnlookup(*av), nalloc), TRUE);
	varrm("*", TRUE);
	unexcept(); /* eVarstack */
	unexcept(); /* eReturn */
}

static void arg_count(char *name) {
	fprint(2, "too many arguments to %s\n", name);
	set(FALSE);
}

static void badnum(char *num) {
	fprint(2, "%s is a bad number\n", num);
	set(FALSE);
}

/* a dummy command. (exec() performs "exec" simply by not forking) */

extern void b_exec(char **av) {
}

/* echo -n omits a newline. echo -- -n echos '-n' */

static void b_echo(char **av) {
	char *format = "%A\n";
	if (*++av != NULL) {
		if (streq(*av, "-n"))
                	format = "%A", av++;
		else if (streq(*av, "--"))
			av++;
	}
	fprint(1, format, av);
	set(TRUE);
}

/* cd. traverse $cdpath if the directory given is not an absolute pathname */

static void b_cd(char **av) {
	List *s, nil;
	char *path = NULL;
	size_t t, pathlen = 0;
	if (*++av == NULL) {
		s = varlookup("home");
		*av = (s == NULL) ? "/" : s->w;
	} else if (av[1] != NULL) {
		arg_count("cd");
		return;
	}
	if (isabsolute(*av) || streq(*av, ".") || streq(*av, "..")) { /* absolute pathname? */
		if (chdir(*av) < 0) {
			set(FALSE);
			uerror(*av);
		} else
			set(TRUE);
	} else {
		s = varlookup("cdpath");
		if (s == NULL) {
			s = &nil;
			nil.w = "";
			nil.n = NULL;
		}
		do {
			if (s != &nil && *s->w != '\0') {
				t = strlen(*av) + strlen(s->w) + 2;
				if (t > pathlen)
					path = nalloc(pathlen = t);
				strcpy(path, s->w);
				if (!streq(s->w, "/")) /* "//" is special to POSIX */
					strcat(path, "/");
				strcat(path, *av);
			} else {
				pathlen = 0;
				path = *av;
			}
			if (chdir(path) >= 0) {
				set(TRUE);
				if (interactive && *s->w != '\0' && !streq(s->w, "."))
					fprint(1, "%s\n", path);
				return;
			}
			s = s->n;
		} while (s != NULL);
		fprint(2, "couldn't cd to %s\n", *av);
		set(FALSE);
	}
}

static void b_umask(char **av) {
	int i;
	if (*++av == NULL) {
		set(TRUE);
		i = umask(0);
		umask(i);
		fprint(1, "0%o\n", i);
	} else if (av[1] == NULL) {
		i = o2u(*av);
		if ((unsigned int) i > 0777) {
			fprint(2, "bad umask\n");
			set(FALSE);
		} else {
			umask(i);
			set(TRUE);
		}
	} else {
		arg_count("umask");
		return;
	}
}

static void b_exit(char **av) {
	if (*++av != NULL)
		ssetstatus(av);
	rc_exit(getstatus());
}

/* raise a "return" exception, i.e., return from a function. if an integer argument is present, set $status to it */

static void b_return(char **av) {
	if (*++av != NULL)
		ssetstatus(av);
	rc_raise(eReturn);
}

/* raise a "break" exception for breaking out of for and while loops */

static void b_break(char **av) {
	if (av[1] != NULL) {
		arg_count("break");
		return;
	}
	rc_raise(eBreak);
}

/* shift $* n places (default 1) */

static void b_shift(char **av) {
	int shift = (av[1] == NULL ? 1 : a2u(av[1]));
	List *s, *dollarzero;
	if (av[1] != NULL && av[2] != NULL) {
		arg_count("shift");
		return;
	}
	if (shift < 0) {
		badnum(av[1]);
		return;
	}
	s = varlookup("*")->n;
	dollarzero = varlookup("0");
	while (s != NULL && shift != 0) {
		s = s->n;
		--shift;
	}
	if (s == NULL && shift != 0) {
		fprint(2, "cannot shift\n");
		set(FALSE);
	} else {
		varassign("*", append(dollarzero, s), FALSE);
		set(TRUE);
	}
}

/* dud function */

extern void b_builtin(char **av) {
}

/* wait for a given process, or all outstanding processes */

static void b_wait(char **av) {
	int stat, pid;
	if (av[1] == NULL) {
		waitforall();
		return;
	}
	if (av[2] != NULL) {
		arg_count("wait");
		return;
	}
	if ((pid = a2u(av[1])) < 0) {
		badnum(av[1]);
		return;
	}
	if (rc_wait4(pid, &stat, FALSE) > 0)
		setstatus(pid, stat);
	else
		set(FALSE);
	SIGCHK;
}

/*
   whatis without arguments prints all variables and functions. Otherwise, check to see if a name
   is defined as a variable, function or pathname.
*/

static void b_whatis(char **av) {
	bool f, found;
	bool ess = FALSE;
	int i, ac, c;
	List *s;
	Node *n;
	char *e;
	for (rc_optind = ac = 0; av[ac] != NULL; ac++)
		; /* count the arguments for getopt */
	while ((c = rc_getopt(ac, av, "s")) == 's')
		ess = TRUE;
	if (c != -1) {
		set(FALSE);
		return;
	}
	av += rc_optind;
	if (*av == NULL && !ess) {
		whatare_all_vars();
		set(TRUE);
		return;
	}
	if (ess)
		whatare_all_signals();
	found = TRUE;
	for (i = 0; av[i] != NULL; i++) {
		f = FALSE;
		errno = ENOENT;
		if ((s = varlookup(av[i])) != NULL) {
			f = TRUE;
			prettyprint_var(1, av[i], s);
		}
		if ((n = fnlookup(av[i])) != NULL) {
			f = TRUE;
			prettyprint_fn(1, av[i], n);
		} else if (isbuiltin(av[i]) != NULL) {
			f = TRUE;
			fprint(1, "builtin %s\n", av[i]);
		} else if ((e = which(av[i], FALSE)) != NULL) {
			f = TRUE;
			fprint(1, "%s\n", e);
		}
		if (!f) {
			found = FALSE;
			if (errno != ENOENT)
				uerror(av[i]);
			else
				fprint(2, "%s not found\n", av[i]);
		}
	}
	set(found);
}

/* push a string to be eval'ed onto the input stack. evaluate it */

static void b_eval(char **av) {
	bool i = interactive;
	if (av[1] == NULL)
		return;
	interactive = FALSE;
	pushstring(av + 1, i); /* don't reset line numbers on noninteractive eval */
	doit(TRUE);
	interactive = i;
}

/*
   push a file to be interpreted onto the input stack. with "-i" treat this as an interactive
   input source.
*/

extern void b_dot(char **av) {
	int fd;
	bool old_i = interactive, i = FALSE;
	Estack e;
	Edata star;
	av++;
	if (*av == NULL)
		return;
	if (streq(*av, "-i")) {
		av++;
		i = TRUE;
	}
	if (dasheye) { /* rc -i file has to do the right thing. reset the dasheye state to FALSE, though. */
		dasheye = FALSE;
		i = TRUE;
	}
	if (*av == NULL)
		return;
	fd = rc_open(*av, rFrom);
	if (fd < 0) {
		if (rcrc) /* on rc -l, don't flag nonexistence of .rcrc */
			rcrc = FALSE;
		else {
			uerror(*av);
			set(FALSE);
		}
		return;
	}
	rcrc = FALSE;
	starassign(*av, av+1, TRUE);
	pushfd(fd);
	interactive = i;
	star.name = "*";
	except(eVarstack, star, &e);
	doit(TRUE);
	varrm("*", TRUE);
	unexcept(); /* eVarstack */
	interactive = old_i;
}

/* put rc into a new pgrp. Used on the NeXT where the Terminal program is broken (sigh) */

static void b_newpgrp(char **av) {
	if (av[1] != NULL) {
		arg_count("newpgrp");
		return;
	}
	setpgrp(rc_pid, rc_pid);
	ioctl(2, TIOCSPGRP, &rc_pid);
}

/* Berkeley limit support was cleaned up by Paul Haahr. */

typedef struct Suffix Suffix;
struct Suffix {
	const Suffix *next;
	long amount;
	char *name;
};

static const Suffix
	kbsuf = { NULL, 1024, "k" },
	mbsuf = { &kbsuf, 1024*1024, "m" },
	gbsuf = { &mbsuf, 1024*1024*1024, "g" },
	stsuf = { NULL, 1, "s" },
	mtsuf = { &stsuf, 60, "m" },
	htsuf = { &mtsuf, 60*60, "h" };
#define	SIZESUF &gbsuf
#define	TIMESUF &htsuf
#define	NOSUF ((Suffix *) NULL)  /* for RLIMIT_NOFILE on SunOS 4.1 */

typedef struct {
	char *name;
	int flag;
	const Suffix *suffix;
} Limit;
static const Limit limits[] = {
	{ "cputime",		RLIMIT_CPU,	TIMESUF },
	{ "filesize",		RLIMIT_FSIZE,	SIZESUF },
	{ "datasize",		RLIMIT_DATA,	SIZESUF },
	{ "stacksize",		RLIMIT_STACK,	SIZESUF },
	{ "coredumpsize",	RLIMIT_CORE,	SIZESUF },
	{ "memoryuse",		RLIMIT_RSS,	SIZESUF },
	{ "descriptors",	RLIMIT_NOFILE,	NOSUF },
	{ NULL, 0, NULL }
};

static void printlimit(const Limit *limit, bool hard) {
	struct rlimit rlim;
	long lim;
	getrlimit(limit->flag, &rlim);
	if (hard)
		lim = rlim.rlim_max;
	else
		lim = rlim.rlim_cur;
	if ((unsigned) lim == (unsigned) RLIM_INFINITY)
		fprint(1, "%s \tunlimited\n", limit->name);
	else {
		const Suffix *suf;
		for (suf = limit->suffix; suf != NULL; suf = suf->next)
			if (lim % suf->amount == 0 && (lim != 0 || suf->amount > 1)) {
				lim /= suf->amount;
				break;
			}
		fprint(1, "%s \t%d%s\n", limit->name, lim, (suf == NULL || lim == 0) ? "" : suf->name);
	}
}

static long parselimit(const Limit *limit, char *s) {
	char *t;
	int len = strlen(s);
	long lim = 1;
	const Suffix *suf = limit->suffix;
	if (streq(s, "unlimited"))
		return RLIM_INFINITY;
	if (suf == TIMESUF && (t = strchr(s, ':')) != NULL) {
		*t++ = '\0';
		lim = 60 * a2u(s) + a2u(t);
	} else {
		for (; suf != NULL; suf = suf->next)
			if (streq(suf->name, s + len - strlen(suf->name))) {
				s[len - strlen(suf->name)] = '\0';
				lim *= suf->amount;
				break;
			}
		lim *= a2u(s);
	}
	return lim;
}

static void b_limit(char **av) {
	const Limit *lp = limits;
	bool hard = FALSE;
	if (*++av != NULL && streq(*av, "-h")) {
		av++;
		hard = TRUE;
	}
	if (*av == NULL) {
		for (; lp->name != NULL; lp++)
			printlimit(lp, hard);
		return;
	}
	for (;; lp++) {
		if (lp->name == NULL) {
			fprint(2, "no such limit\n");
			set(FALSE);
			return;
		}
		if (streq(*av, lp->name))
			break;
	}
	if (*++av == NULL)
		printlimit(lp, hard);
	else {
		struct rlimit rlim;
		long pl;
		getrlimit(lp->flag, &rlim);
		if ((pl = parselimit(lp, *av)) < 0) {
			fprint(2, "bad limit\n");
			set(FALSE);
			return;
		}
		if (hard)
			rlim.rlim_max = pl;
		else
			rlim.rlim_cur = pl;
		if (setrlimit(lp->flag, &rlim) == -1) {
			uerror("setrlimit");
			set(FALSE);
		} else
			set(TRUE);
	}
}
