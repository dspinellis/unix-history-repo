static	char *sccsid = "@(#)dir.c 4.2 %G%";

#include "sh.h"
#include "sh.dir.h"

/*
 * C Shell - directory management
 */

struct	directory *dfind();
char	*dfollow();
struct	directory dhead;		/* "head" of loop */
int	printd;				/* force name to be printed */
static	char *fakev[] = { "dirs", NOSTR };

/*
 * dinit - initialize current working directory
 */
dinit(hp)
	char *hp;
{
	register char *cp;
	register struct directory *dp;
	char path[BUFSIZ];

	if (loginsh && hp)
		cp = hp;
	else {
		cp = getwd(path);
		if (cp == NULL) {
			write(2, path, strlen(path));
			exit(1);
		}
	}
	dp = (struct directory *)calloc(sizeof (struct directory), 1);
	dp->di_name = savestr(cp);
	dp->di_count = 0;
	dhead.di_next = dhead.di_prev = dp;
	dp->di_next = dp->di_prev = &dhead;
	printd = 0;
	dnewcwd(dp);
}

/*
 * dodirs - list all directories in directory loop
 */
dodirs(v)
	char **v;
{
	register struct directory *dp;
	bool lflag;
	char *hp = value("home");

	if (*hp == '\0')
		hp = NOSTR;
	if (*++v != NOSTR)
		if (eq(*v, "-l") && *++v == NOSTR)
			lflag = 1;
		else
			error("Usage: dirs [ -l ]");
	else
		lflag = 0;
	dp = dcwd;
	do {
		if (dp == &dhead)
			continue;
		if (!lflag && hp != NOSTR) {
			dtildepr(hp, dp->di_name);
		} else
			printf("%s", dp->di_name);
		printf(" ");
	} while ((dp = dp->di_prev) != dcwd);
	printf("\n");
}

dtildepr(home, dir)
	register char *home, *dir;
{

	if (!eq(home, "/") && prefix(home, dir))
		printf("~%s", dir + strlen(home));
	else
		printf("%s", dir);
}

/*
 * dochngd - implement chdir command.
 */
dochngd(v)
	char **v;
{
	register char *cp;
	register struct directory *dp;

	printd = 0;
	if (*++v == NOSTR) {
		if ((cp = value("home")) == NOSTR || *cp == 0)
			bferr("No home directory");
		if (chdir(cp) < 0)
			bferr("Can't change to home directory");
		cp = savestr(cp);
	} else if ((dp = dfind(*v)) != 0) {
		printd = 1;
		if (chdir(dp->di_name) < 0)
			Perror(dp->di_name);
		dcwd->di_prev->di_next = dcwd->di_next;
		dcwd->di_next->di_prev = dcwd->di_prev;
		goto flushcwd;
	} else
		cp = dfollow(*v);
	dp = (struct directory *)calloc(sizeof (struct directory), 1);
	dp->di_name = cp;
	dp->di_count = 0;
	dp->di_next = dcwd->di_next;
	dp->di_prev = dcwd->di_prev;
	dp->di_prev->di_next = dp;
	dp->di_next->di_prev = dp;
flushcwd:
	dfree(dcwd);
	dnewcwd(dp);
}

/*
 * dfollow - change to arg directory; fall back on cdpath if not valid
 */
char *
dfollow(cp)
	register char *cp;
{
	register char **cdp;
	struct varent *c;
	
	cp = globone(cp);
	if (chdir(cp) == 0)
		goto gotcha;
	if (cp[0] != '/' && !prefix("./", cp) && !prefix("../", cp)
	    && (c = adrof("cdpath"))) {
		for (cdp = c->vec; *cdp; cdp++) {
			char buf[BUFSIZ];

			strcpy(buf, *cdp);
			strcat(buf, "/");
			strcat(buf, cp);
			if (chdir(buf) >= 0) {
				printd = 1;
				xfree(cp);
				cp = savestr(buf);
				goto gotcha;
			}
		}
	}
	if (adrof(cp)) {
		char *dp = value(cp);

		if (dp[0] == '/' || dp[0] == '.')
			if (chdir(dp) >= 0) {
				xfree(cp);
				cp = savestr(dp);
				printd = 1;
				goto gotcha;
			}
	}
	xfree(cp);
	Perror(cp);

gotcha:
	if (*cp != '/') {
		char *dp = calloc(strlen(cp) + strlen(dcwd->di_name) + 2, 1);
		strcpy(dp, dcwd->di_name);
		strcat(dp, "/");
		strcat(dp, cp);
		xfree(cp);
		cp = dp;
	}
	dcanon(cp);
	return (cp);
}

/*
 * dopushd - push new directory onto directory stack.
 *	with no arguments exchange top and second.
 *	with numeric argument (+n) bring it to top.
 */
dopushd(v)
	char **v;
{
	register struct directory *dp;

	printd = 1;
	if (*++v == NOSTR) {
		if ((dp = dcwd->di_prev) == &dhead)
			dp = dhead.di_prev;
		if (dp == dcwd)
			bferr("No other directory");
		if (chdir(dp->di_name) < 0)
			Perror(dp->di_name);
		dp->di_prev->di_next = dp->di_next;
		dp->di_next->di_prev = dp->di_prev;
		dp->di_next = dcwd->di_next;
		dp->di_prev = dcwd;
		dcwd->di_next->di_prev = dp;
		dcwd->di_next = dp;
	} else if (dp = dfind(*v)) {
		if (chdir(dp->di_name) < 0)
			Perror(dp->di_name);
	} else {
		register char *cp;

		cp = dfollow(*v);
		dp = (struct directory *)calloc(sizeof (struct directory), 1);
		dp->di_name = cp;
		dp->di_count = 0;
		dp->di_prev = dcwd;
		dp->di_next = dcwd->di_next;
		dcwd->di_next = dp;
		dp->di_next->di_prev = dp;
	}
	dnewcwd(dp);
}

/*
 * dfind - find a directory if specified by numeric (+n) argument
 */
struct directory *
dfind(cp)
	register char *cp;
{
	register struct directory *dp;
	register int i;
	register char *ep;

	if (*cp++ != '+')
		return (0);
	for (ep = cp; digit(*ep); ep++)
		continue;
	if (*ep)
		return (0);
	i = getn(cp);
	if (i <= 0)
		return (0);
	for (dp = dcwd; i != 0; i--) {
		if ((dp = dp->di_prev) == &dhead)
			dp = dp->di_prev;
		if (dp == dcwd)
			bferr("Directory stack not that deep");
	}
	return (dp);
}

/*
 * dopopd - pop a directory out of the directory stack
 *	with a numeric argument just discard it.
 */
dopopd(v)
	char **v;
{
	register struct directory *dp, *p;

	printd = 1;
	if (*++v == NOSTR)
		dp = dcwd;
	else if ((dp = dfind(*v)) == 0)
		bferr("Bad directory");
	if (dp->di_prev == &dhead && dp->di_next == &dhead)
		bferr("Directory stack empty");
	if (dp == dcwd) {
		if ((p = dp->di_prev) == &dhead)
			p = dhead.di_prev;
		if (chdir(p->di_name) < 0)
			Perror(p->di_name);
	}
	dp->di_prev->di_next = dp->di_next;
	dp->di_next->di_prev = dp->di_prev;
	if (dp == dcwd)
		dnewcwd(p);
	else
		dodirs(fakev);
	dfree(dp);
}

/*
 * dfree - free the directory (or keep it if it still has ref count)
 */
dfree(dp)
	register struct directory *dp;
{

	if (dp->di_count != 0)
		dp->di_next = dp->di_prev = 0;
	else
		xfree(dp->di_name), xfree((char *)dp);
}

/*
 * dcanon - canonicalize the pathname, removing excess ./ and ../ etc.
 *	we are of course assuming that the file system is standardly
 *	constructed (always have ..'s, directories have links)
 */
dcanon(cp)
	char *cp;
{
	register char *p, *sp;
	register bool slash;

	if (*cp != '/')
		abort();
	for (p = cp; *p; ) {		/* for each component */
		sp = p;			/* save slash address */
		while(*++p == '/')	/* flush extra slashes */
			;
		if (p != ++sp)
			strcpy(sp, p);
		p = sp;			/* save start of component */
		slash = 0;
		while(*++p)		/* find next slash or end of path */
			if (*p == '/') {
				slash = 1;
				*p = 0;
				break;
			}
		if (*sp == '\0')	/* if component is null */
			if (--sp == cp)	/* if path is one char (i.e. /) */
				break;
			else
				*sp = '\0';
		else if (eq(".", sp)) {
			if (slash) {
				strcpy(sp, ++p);
				p = --sp;
			} else if (--sp != cp)
				*sp = '\0';
		} else if (eq("..", sp)) {
			if (--sp != cp)
				while (*--sp != '/')
					;
			if (slash) {
				strcpy(++sp, ++p);
				p = --sp;
			} else if (cp == sp)
				*++sp = '\0';
			else
				*sp = '\0';
		} else if (slash)
			*p = '/';
	}
}

/*
 * dnewcwd - make a new directory in the loop the current one
 */
dnewcwd(dp)
	register struct directory *dp;
{

	dcwd = dp;
	set("cwd", savestr(dcwd->di_name));
	if (printd)
		dodirs(fakev);
}
