#include "mh.h"
#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>

struct  msgs *mp;

struct msgs *m_gmsg(name)
char *name;
{
	register int i, j;
	register char *cp;
	register struct msgs *msgp;
	register struct direct *dir;
	DIR *dirp;
	int  curfil;
	struct stat statb;
	char buf[132];

	struct {
		int xhghmsg,
		    xnummsg,
		    xlowmsg,
		    xcurmsg;
		char xselist,
		     xflags,
		     xfiller,
		     xothers;
		char xmsgs[1000];
	} msgbuf;

	if ((dirp = opendir(".")) == 0)
		return (0);
	for(j = 0; j < 1000; j++)
		msgbuf.xmsgs[j] = 0;
	msgbuf.xcurmsg = 0;
	msgbuf.xnummsg = 0;
	msgbuf.xselist = 0;
	msgbuf.xothers = 0;
	msgbuf.xlowmsg = 5000;
	msgbuf.xhghmsg = 0;
	msgbuf.xflags  = (access(".",2) == -1)? READONLY:0;

	/*
	 * The following hack is that if the directory is writable
	 * and the cur file is not, we consider it to be read only
	 * folder.
	 */

	if (stat("cur", &statb) >= 0 && access("cur", 2) < 0)
		msgbuf.xflags |= READONLY;
	curfil = 0;
	while (dir = readdir(dirp)) {
		cp = dir->d_name;
		if (j = mu_atoi(cp)) {
			if (j > msgbuf.xhghmsg)
				msgbuf.xhghmsg = j;
			msgbuf.xnummsg++;
			if (j < msgbuf.xlowmsg)
				msgbuf.xlowmsg = j;
			msgbuf.xmsgs[j] = EXISTS;
		} else if (*cp != ',' && *cp != '.' && *cp != '#')
			if (strcmp(cp, current) == 0)
				curfil++;
			else if (strcmp(cp, listname) == 0)
				msgbuf.xselist++;
			else
				msgbuf.xothers++;
	}
	if(!msgbuf.xhghmsg)
		msgbuf.xlowmsg = 0;
	closedir(dirp);
	if(msgbuf.xflags&READONLY) {
		sprintf(buf, "cur-%s", name);
/***            copy(name, copy("cur-", buf));          ***/
		if((cp = m_find(buf)) != NULL)
			if(j = mu_atoi(cp))
				msgbuf.xcurmsg = j;
	} else if(curfil && (i = open(current, 0)) >= 0) {
		if((j = read(i, buf, sizeof (buf))) >= 2) {
			buf[j-1] = '\0';    /* Zap <lf> */
			if (j = mu_atoi(buf))
				msgbuf.xcurmsg = j;
		}
		close(i);
	}
	if( (int) (msgp = (struct msgs *) malloc(sizeof *mp + msgbuf.xhghmsg + 2)) == -1)
		return(0);
	msgp->hghmsg   = msgbuf.xhghmsg;
	msgp->nummsg   = msgbuf.xnummsg;
	msgp->lowmsg   = msgbuf.xlowmsg;
	msgp->curmsg   = msgbuf.xcurmsg;
	msgp->selist   = msgbuf.xselist;
	msgp->msgflags = msgbuf.xflags;
	msgp->others   = msgbuf.xothers;
	msgp->foldpath = name;
	msgp->lowsel   = 5000;
	msgp->hghsel   = 0;
	msgp->numsel   = 0;
	for(j = 0; j <= msgbuf.xhghmsg; j++)
		msgp->msgstats[j] = msgbuf.xmsgs[j];
	return(msgp);
}


mu_atoi(str)
char *str;
{
	register char *cp;
	register int i;

	i = 0;
	cp = str;
	while(*cp) {
		if(*cp < '0' || *cp > '9' || i > 99)
			return(0);
		i *= 10;
		i += *cp++ - '0';
	}
	return(i);
}
