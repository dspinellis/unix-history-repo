#ifndef lint
static char sccsid[] = "@(#)rmf.c	1.4 7/7/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/dir.h>
#include "strings.h"

char *anoyes[];         /* Std no/yes gans array        */

int     subf;

struct msgs *mp;

struct swit switches[] = {
	"help",         4,      /* 0 */
	0,              0
};

main(argc, argv)
char *argv[];
{
	register char *cp, **ap;
	char *folder, buf[128];
	int i, def_fold;
	char *arguments[50], **argp;

#ifdef NEWS
	m_news();
#endif
	folder = 0;
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
			case -2:ambigsw(cp, switches);       /* ambiguous */
				goto leave;
							/* unknown */
			case -1:fprintf(stderr, "rmf: -%s unknown\n", cp);
				goto leave;
							/* -help */
			case 0: help("rmf [+folder]  [switches]", switches);
				goto leave;
			}
		if(*cp == '+')
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		else {
			fprintf(stderr, "Usage: rmf [+folder]\n");
			goto leave;
		}
	}
	if(!folder) {
		folder = m_getfolder();
		def_fold++;
	}
	subf = !((!index(folder, '/')) | (*folder == '/') | (*folder == '.'));
	if(def_fold && !subf) {
		cp = concat("Remove folder \"", folder, "\" ?? ", 0);
		if(!gans(cp, anoyes))
			goto leave;
		free(cp);
	}
	if(rmfold(folder))
		goto leave;
	if(subf) {                      /* make parent "current" */
		cp = copy(folder, buf);
		while(cp > buf && *cp != '/') --cp;
		if(cp > buf) {
			*cp = 0;
			if(strcmp(m_find(pfolder), buf) != 0) {
				printf("[+%s now current]\n", buf);
				m_replace(pfolder, buf);
			}
		}
	}
 leave:
	m_update();
	done(0);
}

rmfold(fold)
char *fold;
{
	register char *maildir;
	struct direct *ent;
	int i, leftover, cd;
	register char *cp, *sp;
	char nambuf[10];
	register DIR *dirp;

	leftover = 0;
	if(!subf && strcmp(m_find(pfolder), fold) == 0) /* make default "current"*/
		if(strcmp(m_find(pfolder), defalt) != 0) {
			printf("[+%s now current]\n", defalt);
			fflush(stdout);                          /*??*/
			m_replace(pfolder, defalt);
		}
	maildir = m_maildir(fold);
	if((cd = chdir(maildir)) < 0)
		goto funnyfold;
	if(access(".", 2) == -1) {
 funnyfold:     if(!m_delete(concat("cur-", fold, 0)))
			printf("[Folder %s de-referenced]\n", fold);
		else
			fprintf(stderr, "You have no profile entry for the %s folder %s\n",
			  cd < 0 ? "unreadable" : "read-only", fold);
		return(1);
	}
	dirp = opendir(".");
	ent = readdir(dirp);		/* move pointer past "." */
	ent = readdir(dirp);		/* move pointer past ".." */
	while(ent = readdir(dirp)) {
		if (ent->d_ino==0) continue;
		switch (ent->d_name[0]) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '#':
			case ',':
				if(unlink(ent->d_name) == -1) {
					fprintf(stderr, "Can't unlink %s:%s\n", fold,ent->d_name);
					leftover++;
			    	}
				break;
			default:
				if (strcmp(ent->d_name, "cur") == 0 ||
				    strcmp(ent->d_name, "@") == 0) {
			    		if(unlink(ent->d_name) == -1) {
						fprintf(stderr, "Can't unlink %s:%s\n", fold,ent->d_name);
						leftover++;
			    		}
				} else {
					fprintf(stderr, "File \"%s/%s\" not deleted!\n", fold, ent->d_name);
					leftover++;
				}
		}
	}
	closedir(dirp);
	chdir("..");            /* Move out of dir to be deleted */
	if (leftover) {
		fprintf(stderr, "Folder %s not removed!\n", fold);
		return(1);
	}
	if(rmdir(maildir))
		fprintf(stderr, "rmdir failed!\n");
}
