#include "mh.h"
#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <strings.h>

#define NFOLDERS 100

int all, hdrflag, foldp;
struct msgs *mp;
char folder[128], *folds[NFOLDERS];
int msgtot, foldtot, totonly, fshort;
int fpack;
struct swit switches[] = {
	"all",          0,      /* 0 */
	"down",         0,      /* 1 */
	"fast",         0,      /* 2 */
	"nofast",       0,      /* 3 */
	"header",       0,      /* 4 */
	"noheader",     0,      /* 5 */
	"pack",         0,      /* 6 */
	"nopack",       0,      /* 7 */
	"short",       -1,      /* 8 */
	"total",        0,      /* 9 */
	"nototal",      0,      /*10 */
	"up",           0,      /*11 */
	"help",         4,      /*12 */
	"debug",	5,	/*13 */
	0,              0
};

int	debug;

main(argc, argv)
char *argv[];
{
	register char *cp, *curm;
	register int i;
	char *argfolder;
	int up, down, j, def_short;
	char *arguments[50], **argp, **ap;
	struct stat stbf;
	struct node *np;
	extern char _sobuf[];
	register struct direct *dir;
	DIR *dirp;

	setbuf(stdout, _sobuf);
#ifdef NEWS
	m_news();
#endif
	up = down = 0;
	argfolder = NULL;
	curm = 0;
	if(argv[0][strlen(argv[0])-1] == 's')   /* Plural name?? */
		all++;
	cp = r1bindex(argv[0], '/');
	if((cp = m_find(cp)) != NULL) {
		ap = brkstring(cp = getcpy(cp), " ", "\n");
		ap = copyip(ap, arguments);
	} else
		ap = arguments;
	copyip(argv+1, ap);
	argp = arguments;
	while(cp = *argp++) {
		if(*cp == '-')
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);     /* ambiguous */
				goto leave;
							   /* unknown */
			case -1:fprintf(stderr, "folder: -%s unknown\n", cp);
				goto leave;
			case 0: all++;  continue;          /* -all      */
			case 1: down++;  continue;         /* -down     */
			case 2:                            /* -fast     */
			case 3: fshort = 0; continue;      /* -nofast   */
			case 4: hdrflag = -1;  continue;   /* -header   */
			case 5: hdrflag = 0;  continue;    /* -noheader */
			case 6: fpack = 1; continue;       /* -pack     */
			case 7: fpack = 0; continue;       /* -nopack   */
			case 8: fshort = 1; continue;      /* -short    */
			case 9: all++; totonly = 1;        /* -total    */
				continue;
			case 10:if(totonly) all--;         /* -nototal  */
				totonly =0;  continue;
			case 11:up++;  continue;           /* -up       */
							   /* -help     */
			case 12:help("folder [+folder]  [msg] [switches]",
				     switches);
				goto leave;
			case 13:debug++;		   /* -debug    */
				continue;
			}
		if(*cp == '+') {
			if(argfolder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				argfolder = cp + 1;
		} else if(curm) {
			fprintf(stderr, "Only one current may be given.\n");
			goto leave;
		} else
			curm = cp;
	}
	if(all) {
		hdrflag = 0;
		cp = m_maildir("");
		m_getdefs();
		for(np = m_defs; np; np = np->n_next) {
			if(!ssequal("cur-", np->n_name))
				continue;
			if(fshort) {
				def_short++;
				printf("%s\n", np->n_name+4);
			} else
				addfold(np->n_name+4);
		}
		if(def_short)
			putchar('\n');
		if(fshort) {
			m_update();
			fflush(stdout);
			execl(lsproc, "mh-ls", "-x", cp, 0);
			fprintf(stderr, "Can't exec: ");
			perror(lsproc);
			goto leave;
		}
		if(chdir(cp) < 0) {
			fprintf(stderr, "Can't chdir to: ");
			perror(cp);
			goto leave;
		}
		if((cp = m_find(pfolder)) == NULL)
			*folder = 0;
		else
			copy(cp, folder);
		dirp = opendir(".");
		(void) readdir(dirp);
		(void) readdir(dirp);
		while (dir = readdir(dirp)) {
			if (stat(dir->d_name, &stbf) < 0 ||
			    (stbf.st_mode & S_IFMT) != S_IFDIR)
				continue;
			addfold(dir->d_name);
		}
		closedir(dirp);
		for(i = 0; i < foldp; i++) {
			pfold(folds[i], 0); fflush(stdout);
		}
		if(!totonly)
			printf("\n\t\t     ");
		printf("TOTAL= %3d message%c in %d Folder%s.\n",
			msgtot, msgtot!=1? 's':' ',
			foldtot, foldtot!=1? "s":"");
	} else  {
		hdrflag++;
		if(argfolder)
			cp = copy(argfolder, folder);
		else
			cp = copy(m_getfolder(), folder);
		if(up) {
			while(cp > folder && *cp != '/') --cp;
			if(cp > folder)
				*cp = 0;
			argfolder = folder;
		} else if(down) {
			copy(listname, copy("/", cp));
			argfolder = folder;
		}
		if(pfold(folder, curm) && argfolder)
			m_replace(pfolder, argfolder);
	}

 leave:
	m_update();
	done(0);
}


addfold(fold)
char *fold;
{
	register int i,j;
	register char *cp;

	if(foldp >= NFOLDERS) {
		fprintf(stderr, "More than %d folders!!\n", NFOLDERS);
		return(1);
	}
	cp = getcpy(fold);
	for(i = 0; i < foldp; i++)
		if(compare(cp, folds[i]) < 0) {
			for(j = foldp - 1; j >= i; j--)
				folds[j+1] = folds[j];
			foldp++;
			folds[i] = cp;
			return(0);
		}
	folds[foldp++] = cp;
	return(0);
}


pfold(fold, curm)
char *fold;
{
	register char *mailfile;
	register int msgnum, hole;
	char newmsg[8], oldmsg[8];

	mailfile = m_maildir(fold);
	if(chdir(mailfile) < 0) {
		fprintf(stderr, "Can't chdir to: ");
		perror(mailfile);
		return(0);
	}
	if(fshort) {
		printf("%s\n", fold);
		return(0);
	}
	mp = m_gmsg(fold);
	foldtot++;
	msgtot += mp->nummsg;
	if (debug)
		printf("pfold: %s message flags: 0%o\n", fold, mp->msgflags);
	if(fpack && (mp->msgflags & READONLY) == 0) {
	    if (debug) {
		printf("Would normally be packing folder %s\n", fold);
		return;
	    }
	    for(msgnum = mp->lowmsg, hole = 1; msgnum <= mp->hghmsg; msgnum++) {
		if(mp->msgstats[msgnum]&EXISTS) {
			if(msgnum != hole) {
				copy(m_name(hole), newmsg);
				copy(m_name(msgnum), oldmsg);
				if(link(oldmsg, newmsg) == -1 ||
				   unlink(oldmsg) == -1) {
					fprintf(stderr, "Error moving %s to ", oldmsg);
					perror(newmsg);
					done(1);
				}
				mp->msgstats[hole] = mp->msgstats[msgnum];
				if(msgnum == mp->lowsel)
					mp->lowsel = hole;
				if(msgnum == mp->hghsel)
					mp->hghsel = hole;
			}
			hole++;
		}
	    }
	    if(mp->nummsg > 0) {
		mp->lowmsg = 1;
		mp->hghmsg = hole - 1;
	    }
	}
	if(totonly)
		goto out;
	if(curm) {
		if(!m_convert(curm))
			return(0);
		if(mp->numsel > 1) {
			fprintf(stderr, "Can't set current msg to range: %s\n", curm);
			return(0);
		}
		m_setcur(mp->curmsg = mp->hghsel);
	}
	if(!hdrflag++)
  printf("\t\tFolder   # of messages   ( range ); cur msg (other files)\n");
	printf("%22s", fold);
	if(strcmp(folder, fold) == 0)
		printf("+ ");
	else
		printf("  ");
	if(mp->hghmsg == 0)
		printf("has  no messages");
	else {
		printf("has %3d message%s (%3d-%3d)",
			mp->nummsg, (mp->nummsg==1)?" ":"s",
			mp->lowmsg, mp->hghmsg);
		if(mp->curmsg >= mp->lowmsg && mp->curmsg <= mp->hghmsg)
			printf("; cur=%3s", m_name(mp->curmsg));
	}
	if(mp->selist || mp->others) {
		printf("; (");
		if(mp->selist) {
			printf("%s", listname);
			if(mp->others)
				printf(", ");
		}
		if(mp->others)
			printf("others");
		putchar(')');
	}
	putchar('.');
	putchar('\n');
out:
	free(mp);
	mp = 0;
	return(1);
}


compare(s1, s2)
char *s1, *s2;
{
	register char *c1, *c2;
	register int i;

	c1 = s1; c2 = s2;
	while(*c1 || *c2)
		if(i = *c1++ - *c2++)
			return(i);
	return(0);
}
