/*
 * mkmakefile.c	1.3	81/02/26
 *	Functions in this file build the makefile from the files list
 *	and the information in the config table
 */

#include <stdio.h>
#include <ctype.h>
#include "y.tab.h"
#include "config.h"

#define next_word(fp, wd)\
    { register char *word = get_word(fp);\
	if (word == EOF) return EOF; \
	else wd = word; }

static struct file_list *fcur;

/*
 * fl_lookup
 *	look up a file name
 */

struct file_list *fl_lookup(file)
register char *file;
{
    register struct file_list *fp;

    for (fp = ftab ; fp != NULL; fp = fp->f_next)
    {
	if (eq(fp->f_fn, file))
	    return fp;
    }
    return NULL;
}

/*
 * new_fent
 *	Make a new file list entry
 */

struct file_list *new_fent()
{
    register struct file_list *fp;

    fp = (struct file_list *) malloc(sizeof *fp);
    fp->f_needs = fp->f_next = NULL;
    if (fcur == NULL)
	fcur = ftab = fp;
    else
	fcur->f_next = fp;
    fcur = fp;
    return fp;
}

/*
 * makefile:
 *	Build the makefile from the skeleton
 */

makefile()
{
    FILE *ifp, *ofp;
    char line[BUFSIZ];

    read_files();			/* Read in the "files" file */
    ifp = fopen("../conf/makefile", "r");
    ofp = fopen(path("makefile"), "w");
    fprintf(ofp, "IDENT=-D%s -D%s\n", raise(ident), cpu_type);
    while(fgets(line, BUFSIZ, ifp) != NULL)
    {
	if (*line != '%')
	{
	    fprintf(ofp, "%s", line);
	    continue;
	}
	else if (eq(line, "%OBJS\n"))
	    do_objs(ofp);
	else if (eq(line, "%CFILES\n"))
	    do_cfiles(ofp);
	else if (eq(line, "%RULES\n"))
	    do_rules(ofp);
	else if (eq(line, "%LOAD\n"))
	    do_load(ofp);
	else
	    fprintf(stderr, "Unknown %% construct in generic makefile: %s", line);
    }
    fclose(ifp);
    fclose(ofp);
}

/*
 * files:
 *	Read in the "files" file.
 *	Store it in the ftab linked list
 */

read_files()
{
    FILE *fp;
    register struct file_list *tp;
    register struct device *dp;
    register char *wd, *this;
    int type;

    fp = fopen("../conf/files", "r");
    if (fp == NULL) {
	perror("../conf/files");
	exit(1);
    }
    ftab = NULL;
    while((wd = get_word(fp)) != EOF)
    {
	if (wd == NULL)
	    continue;
	this = ns(wd);
	/*
	 * Read standard/optional
	 */
	next_word(fp, wd);
	if (wd == NULL)
	{
	    fprintf(stderr, "Huh, no type for %s in files.\n", this);
	    exit(10);
	}
	if ((tp = fl_lookup(wd)) == NULL)
	    tp = new_fent();
	else
	    free(tp->f_fn);
	tp->f_fn = this;
	type = 0;
	if (eq(wd, "optional"))
	{
	    next_word(fp, wd);
	    if (wd == NULL)
	    {
		fprintf(stderr, "Needed a dev for optional(%s)\n", this);
		exit(11);
	    }
	    tp->f_needs = ns(wd);
	    for (dp = dtab ; dp != NULL; dp = dp->d_next)
	    {
		if (eq(dp->d_name, wd))
		    break;
	    }
	    if (dp == NULL)
		type = INVISIBLE;
	}
	next_word(fp, wd);
	if (type == 0 && wd != NULL)
	    type = DEVICE;
	else if (type == 0)
	    type = NORMAL;
	tp->f_type = type;
    }
    fclose(fp);
}

/*
 * do_objs
 *	Spew forth the OBJS definition
 */

do_objs(fp)
FILE *fp;
{
    register struct file_list *tp;
    register int lpos, len;
    register char *cp, och, *sp;

    fprintf(fp, "OBJS=");
    lpos = 6;
    for (tp = ftab; tp != NULL; tp = tp->f_next)
    {
	if (tp->f_type == INVISIBLE)
	    continue;
	sp = rindex(tp->f_fn, '/') + 1;
	cp = sp + (len = strlen(sp)) - 1;
	och = *cp;
	*cp = 'o';
	if (len + lpos > 72)
	{
	    lpos = 8;
	    fprintf(fp, "\\\n\t");
	}
	fprintf(fp, "%s ", sp);
	lpos += len + 1;
	*cp = och;
    }
    if (lpos != 8)
	putc('\n', fp);
}

/*
 * do_cfiles
 *	Spew forth the CFILES definition
 */

do_cfiles(fp)
FILE *fp;
{
    register struct file_list *tp;
    register int lpos, len;

    fprintf(fp, "CFILES=");
    lpos = 8;
    for (tp = ftab; tp != NULL; tp = tp->f_next)
    {
	if (tp->f_type == INVISIBLE)
	    continue;
	if ((len = 3 + strlen(tp->f_fn)) + lpos > 72)
	{
	    lpos = 8;
	    fprintf(fp, "\\\n\t");
	}
	fprintf(fp, "../%s ", tp->f_fn);
	lpos += len + 1;
    }
    if (lpos != 8)
	putc('\n', fp);
}

/*
 * tail:
 *	Return tail end of a filename
 */

char *tail(fn)
char *fn;
{
    register char *cp;

    cp = rindex(fn, '/');
    return cp+1;
}

/*
 * do_rules:
 *	Spit out the rules for making each file
 */

do_rules(f)
FILE *f;
{
    register char *cp, *np, och, *tp;
    register struct file_list *ftp;

    for (ftp = ftab; ftp != NULL; ftp = ftp->f_next)
    {
	if (ftp->f_type == INVISIBLE)
	    continue;
	cp = (np = ftp->f_fn) + strlen(ftp->f_fn) - 1;
	och = *cp;
	*cp = '\0';
	fprintf(f, "%so: ../%s%c\n", tail(np), np, och);
	tp = tail(np);
	if (och == 's')
	    fprintf(f, "\t${AS} -o %so ../%ss\n\n", tp, np);
	else if (ftp->f_type == NORMAL)
	{
	    fprintf(f, "\t${CC} -I. -c -S ${COPTS} ../%sc\n", np);
	    fprintf(f, "\t${C2} %ss | sed -f ../sys/asm.sed | ${AS} -o %so\n",
		    tp, tp);
	    fprintf(f, "\trm -f %ss\n\n", tp);
	}
	else if (ftp->f_type == DEVICE)
	{
	    fprintf(f, "\t${CC} -I. -c -S ${COPTS} ../%sc\n", np);
	    fprintf(f,"\t${C2} -i %ss | sed -f ../sys/asm.sed | ${AS} -o %so\n",
		    tp, tp);
	    fprintf(f, "\trm -f %ss\n\n", tp);
	}
	else
	    fprintf(stderr, "Don't know rules for %s", np);
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

    fprintf(f, "all:");
    for (fl = conf_list; fl != NULL; fl = fl->f_next)
	fprintf(f, " %s", fl->f_needs);
    putc('\n', f);
    for (fl = conf_list; fl != NULL; fl = fl->f_next)
    {
	fprintf(f, "\n%s: makefile locore.o ${OBJS} ioconf.o swap%s.o\n",
		fl->f_needs, fl->f_fn);
	fprintf(f, "\t@echo loading %s\n\t@rm -f %s\n\t",
		fl->f_needs, fl->f_needs);
	fprintf(f,
	    "@ld -n -o %s -e start -x -T 80000000 locore.o ${OBJS} ioconf.o swap%s.o\n",
	    fl->f_needs, fl->f_fn);
	fprintf(f, "\t@echo rearranging symbols\n");
	fprintf(f, "\t@-symorder ../sys/symbols.sort %s\n", fl->f_needs);
	fprintf(f, "\t@size %s\n", fl->f_needs);
	fprintf(f, "\t@chmod 755 %s\n", fl->f_needs);
    }
    for (fl = conf_list; fl != NULL; fl = fl->f_next)
    {
	fprintf(f, "\nswap%s.o: ../dev/swap%s.c\n", fl->f_fn, fl->f_fn);
	fprintf(f, "\t${CC} -I. -c -S ${COPTS} ../dev/swap%s.c\n", fl->f_fn);
	fprintf(f,
	    "\t${C2} swap%s.s | sed -f ../sys/asm.sed | ${AS} -o swap%s.o\n",
	    fl->f_fn, fl->f_fn);
	fprintf(f, "\trm -f swap%s.s\n", fl->f_fn);
    }
}

raise(str)
register char *str;
{
    register char *cp = str;

    while(*str)
    {
	if (islower(*str))
	    *str = toupper(*str);
	str++;
    }
    return cp;
}
