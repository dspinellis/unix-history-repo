#ifndef lint
static char sccsid[] = "@(#)annotate.c	4.1 2/23/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/* annotate file component data
 *
 * prepends   Component: data
 *              date stamp
 */

annotate(file, comp, text, inplace)
	char *file, *comp, *text;
{
	register int src;
	register char *cp;
	register FILE *tmp;
	int cnt, fd;
	char buf[BUFSIZ], *sp, tmpfil[128];
	long now;
	struct stat stbuf;
	char *cdate();

	if((src = open((cp = file), 2)) == -1) { /* this should be an X-open*/
		fprintf(stderr, "Can't open ");
		perror(cp);
		return(1);
	}
	copy(cp, buf);
	sp = r1bindex(buf);
	if(sp != buf) {
		*sp = 0;
		cp = copy(buf, tmpfil);
	} else
		cp = tmpfil;
	copy(makename("ano",".tmp"), cp);
	fstat(src, &stbuf);
	if((tmp = fopen(tmpfil, "w")) == NULL) {
		fprintf(stderr, "Can't create ");
		perror(tmpfil);
		return(1);
	}
	chmod(tmpfil, stbuf.st_mode&0777);
	cp = comp;
	if(*cp >= 'a' && *cp <= 'z') *cp -= 040;
	time(&now);
	cp = cdate(&now);
	cp[9] = ' ';  cp[15] = 0;
	if(*cp == ' ') cp++;
	fprintf(tmp, "%s: <<%s>>\n", comp, cp);
	cp = text;
	do {
		if(*cp == ' ' || *cp == '\t') cp++;
		sp = cp;
		while(*cp && *cp++ != '\n') ;
		if(cp - sp)
			fprintf(tmp, "%s: %*s", comp, cp-sp, sp);
	} while(*cp);
	if(cp[-1] != '\n' && cp != text) putc('\n', tmp);
	do
		if((cnt = read(src, buf, sizeof buf)) > 0)
			fwrite(buf, cnt, 1, tmp);
	while(cnt == sizeof buf);
	fclose(tmp);
	if(inplace) {
		fd = open(tmpfil, 0);          /* reopen for reading */
		lseek(src, 0l, 0);
		do
			if((cnt = read(fd, buf, sizeof buf)) > 0)
				write(src, buf, cnt);
		while(cnt == sizeof buf);
		close(fd);
	} else {
	   /*   cp = copy(file, buf);           */
	   /*   *--cp =| 0200;                  */
	   /*   copy(".bak", copy(file, buf));  */
		cp = copy(file, buf);
		cp[1] = 0;
		do
			*cp = cp[-1];
		while(--cp >= buf && *cp != '/');
		*++cp =
#ifdef UCB
			'#';
#else
			',';
#endif
		unlink(buf);
		if(link(file, buf) == -1) {
			fprintf(stderr, "Can't rename %s to bak file.\n", file);
			return(1);
		}
		if(unlink(file) == -1) {
			fprintf(stderr, "Can't unlink %s\n", file);
			return(1);
		}
		if(link(tmpfil, file) == -1) {
			fprintf(stderr, "Can't lnk temp file \"%s\" to %s\n",
			  tmpfil, file);
			return(1);
		}
	}
	close(src);
	unlink(tmpfil);
	return(0);
}
