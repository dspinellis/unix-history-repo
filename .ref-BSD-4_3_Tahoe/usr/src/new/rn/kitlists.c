/* $Header: kitlists.c,v 4.3 85/05/01 11:42:08 lwall Exp $
 *
 * $Log:	kitlists.c,v $
 * Revision 4.3  85/05/01  11:42:08  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include <stdio.h>

#define MAXKIT 100
#define MAXKITSIZE 63000
#define KITOVERHEAD 700
#define FILEOVERHEAD 80

long tot[MAXKIT];
FILE *outfp[MAXKIT];		/* of course, not this many file descriptors */

main(argc,argv)
int argc;
char **argv;
{
    FILE *inp, *popen();
    char buf[1024], filnam[128];
    char *index();
    register char *s;
    register int i, newtot;
    
    sprintf(buf,"\
ls -l `awk '{print $1}' <%s'` | awk '{print $8 \" \" $4}' | sort +1nr\
", argc > 1 ? argv[1] : "MANIFEST.new");
    inp = popen(buf,"r");

    while (fgets(buf,1024,inp) != Nullch) {
	s = index(buf,' ');
	*s++ = '\0';
	for (i=1;
	  (newtot = tot[i] + atol(s) + FILEOVERHEAD) > MAXKITSIZE-KITOVERHEAD;
	  i++) 
	    ;
	if (!tot[i]) {
	    sprintf(filnam,"kit%d.list",i);
	    outfp[i] = fopen(filnam,"w");
	}
	tot[i] = newtot;
	printf("Adding %s to kit %d giving %d bytes\n",buf,i,newtot);
	fprintf(outfp[i],"%s\n",buf);
    }
}
