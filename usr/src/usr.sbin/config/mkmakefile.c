/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)mkmakefile.c	5.16 (Berkeley) %G%";
#endif not lint

/*
 * Build the makefile for the system, from
 * the information in the files files and the
 * additional files for the machine being compiled to.
 */

#include <stdio.h>
#include <ctype.h>
#include "y.tab.h"
#include "config.h"

#define next_word(fp, wd) \
	{ register char *word = get_word(fp); \
	  if (word == (char *)EOF) \
		return; \
	  else \
		wd = word; \
	}

static	struct file_list *fcur;
char *tail();

/*
 * Lookup a file, by name.
 */
struct file_list *
fl_lookup(file)
	register char *file;
{
	register struct file_list *fp;

	for (fp = ftab ; fp != 0; fp = fp->f_next) {
		if (eq(fp->f_fn, file))
			return (fp);
	}
	return (0);
}

/*
 * Lookup a file, by final component name.
 */
struct file_list *
fltail_lookup(file)
	register char *file;
{
	register struct file_list *fp;

	for (fp = ftab ; fp != 0; fp = fp->f_next) {
		if (eq(tail(fp->f_fn), tail(file)))
			return (fp);
	}
	return (0);
}

/*
 * Make a new file list entry
 */
struct file_list *
new_fent()
{
	register struct file_list *fp;

	fp = (struct file_list *) malloc(sizeof *fp);
	fp->f_needs = 0;
	fp->f_next = 0;
	fp->f_flags = 0;
	fp->f_type = 0;
	if (fcur == 0)
		fcur = ftab = fp;
	else
		fcur->f_next = fp;
	fcur = fp;
	return (fp);
}

char	*COPTS;
static	struct users {
	int	u_default;
	int	u_min;
	int	u_max;
} users[] = {
	{ 24, 8, 1024 },		/* MACHINE_VAX */
	{ 4, 2, 128 },			/* MACHINE_TAHOE */
};
#define	NUSERS	(sizeof (users) / sizeof (users[0]))

/*
 * Build the makefile from the skeleton
 */
makefile()
{
	FILE *ifp, *ofp;
	char line[BUFSIZ];
	struct opt *op;
	struct users *up;

	read_files();
	strcpy(line, "../conf/Makefile.");
	(void) strcat(line, machinename);
	ifp = fopen(line, "r");
	if (ifp == 0) {
		perror(line);
		exit(1);
	}
	ofp = fopen(path("Makefile"), "w");
	if (ofp == 0) {
		perror(path("Makefile"));
		exit(1);
	}
	fprintf(ofp, "IDENT=-D%s", raise(ident));
	if (profiling)
		fprintf(ofp, " -DGPROF");
	if (cputype == 0) {
		printf("cpu type must be specified\n");
		exit(1);
	}
	{ struct cputype *cp;
	  for (cp = cputype; cp; cp = cp->cpu_next)
		fprintf(ofp, " -D%s", cp->cpu_name);
	}
	for (op = opt; op; op = op->op_next)
		if (op->op_value)
			fprintf(ofp, " -D%s=\"%s\"", op->op_name, op->op_value);
		else
			fprintf(ofp, " -D%s", op->op_name);
	fprintf(ofp, "\n");
	if (hadtz == 0)
		printf("timezone not specified; gmt assumed\n");
	if ((unsigned)machine > NUSERS) {
		printf("maxusers config info isn't present, using vax\n");
		up = &users[MACHINE_VAX-1];
	} else
		up = &users[machine-1];
	if (maxusers == 0) {
		printf("maxusers not specified; %d assumed\n", up->u_default);
		maxusers = up->u_default;
	} else if (maxusers < up->u_min) {
		printf("minimum of %d maxusers assumed\n", up->u_min);
		maxusers = up->u_min;
	} else if (maxusers > up->u_max)
		printf("warning: maxusers > %d (%d)\n", up->u_max, maxusers);
	fprintf(ofp, "PARAM=-DTIMEZONE=%d -DDST=%d -DMAXUSERS=%d\n",
	    timezone, dst, maxusers);
	for (op = mkopt; op; op = op->op_next)
		fprintf(ofp, "%s=%s\n", op->op_name, op->op_value);
	while (fgets(line, BUFSIZ, ifp) != 0) {
		if (*line == '%')
			goto percent;
		if (profiling && strncmp(line, "COPTS=", 6) == 0) {
			register char *cp;

			fprintf(ofp, 
			    "GPROF.EX=/usr/src/lib/libc/%s/csu/gmon.ex\n",
			    machinename);
			cp = index(line, '\n');
			if (cp)
				*cp = 0;
			cp = line + 6;
			while (*cp && (*cp == ' ' || *cp == '\t'))
				cp++;
			COPTS = malloc((unsigned)(strlen(cp) + 1));
			if (COPTS == 0) {
				printf("config: out of memory\n");
				exit(1);
			}
			strcpy(COPTS, cp);
			fprintf(ofp, "%s -pg\n", line);
			continue;
		}
		fprintf(ofp, "%s", line);
		continue;
	percent:
		if (eq(line, "%OBJS\n"))
			do_objs(ofp);
		else if (eq(line, "%CFILES\n"))
			do_cfiles(ofp);
		else if (eq(line, "%RULES\n"))
			do_rules(ofp);
		else if (eq(line, "%LOAD\n"))
			do_load(ofp);
		else
			fprintf(stderr,
			    "Unknown %% construct in generic makefile: %s",
			    line);
	}
	(void) fclose(ifp);
	(void) fclose(ofp);
}

/*
 * Read in the information about files used in making the system.
 * Store it in the ftab linked list.
 */
read_files()
{
	FILE *fp;
	register struct file_list *tp, *pf;
	register struct device *dp;
	register struct opt *op;
	char *wd, *this, *needs, *devorprof;
	char fname[32];
	int nreqs, first = 1, configdep, isdup;

	ftab = 0;
	(void) strcpy(fname, "files");
openit:
	fp = fopen(fname, "r");
	if (fp == 0) {
		perror(fname);
		exit(1);
	}
next:
	/*
	 * filename	[ standard | optional ] [ config-dependent ]
	 *	[ dev* | profiling-routine ] [ device-driver]
	 */
	wd = get_word(fp);
	if (wd == (char *)EOF) {
		(void) fclose(fp);
		if (first == 1) {
			(void) sprintf(fname, "files.%s", machinename);
			first++;
			goto openit;
		}
		if (first == 2) {
			(void) sprintf(fname, "files.%s", raise(ident));
			first++;
			fp = fopen(fname, "r");
			if (fp != 0)
				goto next;
		}
		return;
	}
	if (wd == 0)
		goto next;
	this = ns(wd);
	next_word(fp, wd);
	if (wd == 0) {
		printf("%s: No type for %s.\n",
		    fname, this);
		exit(1);
	}
	if ((pf = fl_lookup(this)) && (pf->f_type != INVISIBLE || pf->f_flags))
		isdup = 1;
	else
		isdup = 0;
	tp = 0;
	if (first == 3 && (tp = fltail_lookup(this)) != 0)
		printf("%s: Local file %s overrides %s.\n",
		    fname, this, tp->f_fn);
	nreqs = 0;
	devorprof = "";
	configdep = 0;
	needs = 0;
	if (eq(wd, "standard"))
		goto checkdev;
	if (!eq(wd, "optional")) {
		printf("%s: %s must be optional or standard\n", fname, this);
		exit(1);
	}
nextopt:
	next_word(fp, wd);
	if (wd == 0)
		goto doneopt;
	if (eq(wd, "config-dependent")) {
		configdep++;
		goto nextopt;
	}
	devorprof = wd;
	if (eq(wd, "device-driver") || eq(wd, "profiling-routine")) {
		next_word(fp, wd);
		goto save;
	}
	nreqs++;
	if (needs == 0 && nreqs == 1)
		needs = ns(wd);
	if (isdup)
		goto invis;
	for (dp = dtab; dp != 0; dp = dp->d_next)
		if (eq(dp->d_name, wd))
			goto nextopt;
	for (op = opt; op != 0; op = op->op_next)
		if (op->op_value == 0 && opteq(op->op_name, wd)) {
			if (nreqs == 1) {
				free(needs);
				needs = 0;
			}
			goto nextopt;
		}
invis:
	while ((wd = get_word(fp)) != 0)
		;
	if (tp == 0)
		tp = new_fent();
	tp->f_fn = this;
	tp->f_type = INVISIBLE;
	tp->f_needs = needs;
	tp->f_flags = isdup;
	goto next;

doneopt:
	if (nreqs == 0) {
		printf("%s: what is %s optional on?\n",
		    fname, this);
		exit(1);
	}

checkdev:
	if (wd) {
		next_word(fp, wd);
		if (wd) {
			if (eq(wd, "config-dependent")) {
				configdep++;
				goto checkdev;
			}
			devorprof = wd;
			next_word(fp, wd);
		}
	}

save:
	if (wd) {
		printf("%s: syntax error describing %s\n",
		    fname, this);
		exit(1);
	}
	if (eq(devorprof, "profiling-routine") && profiling == 0)
		goto next;
	if (tp == 0)
		tp = new_fent();
	tp->f_fn = this;
	if (eq(devorprof, "device-driver"))
		tp->f_type = DRIVER;
	else if (eq(devorprof, "profiling-routine"))
		tp->f_type = PROFILING;
	else
		tp->f_type = NORMAL;
	tp->f_flags = 0;
	if (configdep)
		tp->f_flags |= CONFIGDEP;
	tp->f_needs = needs;
	if (pf && pf->f_type == INVISIBLE)
		pf->f_flags = 1;		/* mark as duplicate */
	goto next;
}

opteq(cp, dp)
	char *cp, *dp;
{
	char c, d;

	for (; ; cp++, dp++) {
		if (*cp != *dp) {
			c = isupper(*cp) ? tolower(*cp) : *cp;
			d = isupper(*dp) ? tolower(*dp) : *dp;
			if (c != d)
				return (0);
		}
		if (*cp == 0)
			return (1);
	}
}

do_objs(fp)
	FILE *fp;
{
	register struct file_list *tp, *fl;
	register int lpos, len;
	register char *cp, och, *sp;
	char swapname[32];

	fprintf(fp, "OBJS=");
	lpos = 6;
	for (tp = ftab; tp != 0; tp = tp->f_next) {
		if (tp->f_type == INVISIBLE)
			continue;
		sp = tail(tp->f_fn);
		for (fl = conf_list; fl; fl = fl->f_next) {
			if (fl->f_type != SWAPSPEC)
				continue;
			sprintf(swapname, "swap%s.c", fl->f_fn);
			if (eq(sp, swapname))
				goto cont;
		}
		cp = sp + (len = strlen(sp)) - 1;
		och = *cp;
		*cp = 'o';
		if (len + lpos > 72) {
			lpos = 8;
			fprintf(fp, "\\\n\t");
		}
		fprintf(fp, "%s ", sp);
		lpos += len + 1;
		*cp = och;
cont:
		;
	}
	if (lpos != 8)
		putc('\n', fp);
}

do_cfiles(fp)
	FILE *fp;
{
	register struct file_list *tp, *fl;
	register int lpos, len;
	char swapname[32];

	fputs("CFILES=", fp);
	lpos = 8;
	for (tp = ftab; tp; tp = tp->f_next)
		if (tp->f_type != INVISIBLE) {
			len = strlen(tp->f_fn);
			if (tp->f_fn[len - 1] != 'c')
				continue;
			if ((len = 3 + len) + lpos > 72) {
				lpos = 8;
				fputs("\\\n\t", fp);
			}
			fprintf(fp, "../%s ", tp->f_fn);
			lpos += len + 1;
		}
	for (fl = conf_list; fl; fl = fl->f_next)
		if (fl->f_type == SYSTEMSPEC) {
			sprintf(swapname, "swap%s.c", fl->f_fn);
			if ((len = 3 + strlen(swapname)) + lpos > 72) {
				lpos = 8;
				fputs("\\\n\t", fp);
			}
			if (eq(fl->f_fn, "generic"))
				fprintf(fp, "../%s/%s ", machinename, swapname);
			else
				fprintf(fp, "%s ", swapname);
			lpos += len + 1;
		}
	if (lpos != 8)
		putc('\n', fp);
}

char *
tail(fn)
	char *fn;
{
	register char *cp;

	cp = rindex(fn, '/');
	if (cp == 0)
		return (fn);
	return (cp+1);
}

/*
 * Create the makerules for each file
 * which is part of the system.
 * Devices are processed with the special c2 option -i
 * which avoids any problem areas with i/o addressing
 * (e.g. for the VAX); assembler files are processed by as.
 */
do_rules(f)
	FILE *f;
{
	register char *cp, *np, och, *tp;
	register struct file_list *ftp;
	char *extras;

for (ftp = ftab; ftp != 0; ftp = ftp->f_next) {
	if (ftp->f_type == INVISIBLE)
		continue;
	cp = (np = ftp->f_fn) + strlen(ftp->f_fn) - 1;
	och = *cp;
	*cp = '\0';
	if (och == 'o') {
		fprintf(f, "%so:\n\t-cp ../%so .\n\n", tail(np), np);
		continue;
	}
	fprintf(f, "%so: ../%s%c\n", tail(np), np, och);
	tp = tail(np);
	if (och == 's') {
		fprintf(f, "\t-ln -s ../%ss %sc\n", np, tp);
		fprintf(f, "\t${CC} -E ${COPTS} %sc | ${AS} -o %so\n",
			tp, tp);
		fprintf(f, "\trm -f %sc\n\n", tp);
		continue;
	}
	if (ftp->f_flags & CONFIGDEP)
		extras = "${PARAM} ";
	else
		extras = "";
	switch (ftp->f_type) {

	case NORMAL:
		switch (machine) {

		case MACHINE_VAX:
		case MACHINE_TAHOE:
			fprintf(f, "\t${CC} -c -S ${COPTS} %s../%sc\n",
				extras, np);
			fprintf(f, "\t${C2} %ss | ${INLINE} | ${AS} -o %so\n",
			    tp, tp);
			fprintf(f, "\trm -f %ss\n\n", tp);
			break;
		}
		break;

	case DRIVER:
		switch (machine) {

		case MACHINE_VAX:
		case MACHINE_TAHOE:
			fprintf(f, "\t${CC} -c -S ${COPTS} %s../%sc\n",
				extras, np);
			fprintf(f,"\t${C2} -i %ss | ${INLINE} | ${AS} -o %so\n",
			    tp, tp);
			fprintf(f, "\trm -f %ss\n\n", tp);
			break;
		}
		break;

	case PROFILING:
		if (!profiling)
			continue;
		if (COPTS == 0) {
			fprintf(stderr,
			    "config: COPTS undefined in generic makefile");
			COPTS = "";
		}
		switch (machine) {

		case MACHINE_TAHOE:
			fprintf(f, "\t${CC} -c -S %s %s../%sc\n",
				COPTS, extras, np);
			fprintf(f, "\tex - %ss < ${GPROF.EX}\n", tp);
			fprintf(f,"\t${C2} %ss | ${INLINE} | ${AS} -o %so\n",
			    tp, tp);
			fprintf(f, "\trm -f %ss\n\n", tp);
			break;

		case MACHINE_VAX:
			fprintf(f, "\t${CC} -c -S %s %s../%sc\n",
				COPTS, extras, np);
			fprintf(f, "\tex - %ss < ${GPROF.EX}\n", tp);
			fprintf(f, "\t${INLINE} %ss | ${AS} -o %so\n", tp, tp);
			fprintf(f, "\trm -f %ss\n\n", tp);
			break;
		}
		break;

	default:
		printf("Don't know rules for %s\n", np);
		break;
	}
	*cp = och;
}
}

/*
 * Create the load strings
 */
do_load(f)
	register FILE *f;
{
	register struct file_list *fl;
	register int first;
	struct file_list *do_systemspec();

	for (first = 1, fl = conf_list; fl; first = 0)
		fl = fl->f_type == SYSTEMSPEC ?
			do_systemspec(f, fl, first) : fl->f_next;
	fputs("all:", f);
	for (fl = conf_list; fl; fl = fl->f_next)
		if (fl->f_type == SYSTEMSPEC)
			fprintf(f, " %s", fl->f_needs);
	putc('\n', f);
}

struct file_list *
do_systemspec(f, fl, first)
	FILE *f;
	register struct file_list *fl;
	int first;
{

	fprintf(f, "%s: Makefile", fl->f_needs);
	if (machine == MACHINE_VAX)
		fprintf(f, " ${INLINE} locore.o emulate.o");
	else if (machine == MACHINE_TAHOE)
		fprintf(f, " ${INLINE} locore.o");
	else
		fprintf(f, " locore.o");
	fprintf(f, " ${OBJS} param.o ioconf.o swap%s.o\n", fl->f_fn);
	fprintf(f, "\t@echo loading %s\n\t@rm -f %s\n",
	    fl->f_needs, fl->f_needs);
	if (first) {
		fprintf(f, "\t@sh ../conf/newvers.sh\n");
		fprintf(f, "\t@${CC} $(CFLAGS) -c vers.c\n");
	}
	switch (machine) {

	case MACHINE_VAX:
		fprintf(f, "\t@${LD} -n -o %s -e start -x -T 80000000 ",
			fl->f_needs);
		fprintf(f,
		    "locore.o emulate.o ${OBJS} vers.o ioconf.o param.o ");
		break;

	case MACHINE_TAHOE:
		fprintf(f, "\t@${LD} -n -o %s -e start -x -T C0000800 ",
			fl->f_needs);
		fprintf(f, "locore.o ${OBJS} vers.o ioconf.o param.o ");
		break;
	}
	fprintf(f, "swap%s.o\n", fl->f_fn);
	fprintf(f, "\t@echo rearranging symbols\n");
	fprintf(f, "\t@-symorder ../%s/symbols.sort %s\n",
	    machinename, fl->f_needs);
	fprintf(f, "\t@size %s\n", fl->f_needs);
	fprintf(f, "\t@chmod 755 %s\n\n", fl->f_needs);
	do_swapspec(f, fl->f_fn);
	for (fl = fl->f_next; fl->f_type == SWAPSPEC; fl = fl->f_next)
		;
	return (fl);
}

do_swapspec(f, name)
	FILE *f;
	register char *name;
{

	if (!eq(name, "generic")) {
		fprintf(f, "swap%s.o: swap%s.c\n", name, name);
		fprintf(f, "\t${CC} -c -O ${COPTS} swap%s.c\n\n", name);
		return;
	}
	fprintf(f, "swapgeneric.o: ../%s/swapgeneric.c\n", machinename);
	switch (machine) {

	case MACHINE_VAX:
	case MACHINE_TAHOE:
		fprintf(f, "\t${CC} -c -S ${COPTS} ");
		fprintf(f, "../%s/swapgeneric.c\n", machinename);
		fprintf(f, "\t${C2} swapgeneric.s | ${INLINE}");
		fprintf(f, " | ${AS} -o swapgeneric.o\n");
		fprintf(f, "\trm -f swapgeneric.s\n\n");
		break;
	}
}

char *
raise(str)
	register char *str;
{
	register char *cp = str;

	while (*str) {
		if (islower(*str))
			*str = toupper(*str);
		str++;
	}
	return (cp);
}
