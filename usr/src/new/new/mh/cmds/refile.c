#ifndef lint
static char sccsid[] = "@(#)refile.c	1.1 5/26/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <strings.h>

#define NFOLD 20                /* Allow 20 folder specs */

/* file [-src folder] [msgs] +folder [+folder ...]
 *
 * moves messages from src folder (or current) to other one(s).
 *
 *  all = 1-999 for a message sequence
 *  -preserve says preserve msg numbers
 *  -link says don't delete old msg
 */

char *anoyes[];         /* Std no/yes gans array        */

int vecp, foldp, prsrvf;
char **vec, maildir[128], *folder, *file;
struct msgs *mp;

struct st_fold {
	char *f_name;
	struct msgs *f_mp;
} folders[NFOLD];

char   *files[NFOLD + 1];       /* Vec of files to process--starts at 1! */
int     filec = 1;
char	*bellfile = "/usr/bin/file";
extern	int errno;
char	*rindex();

struct swit switches[] = {
	"all",           -3,      /* 0 */
	"link",           0,      /* 1 */
	"nolink",         0,      /* 2 */
	"preserve",       0,      /* 3 */
	"nopreserve",     0,      /* 4 */
	"src +folder",    0,      /* 5 */
	"file",           0,      /* 6 */
	"help",           4,      /* 7 */
	0,                0
};
main(argc, argv)
char *argv[];
{
	register int i, msgnum;
	register char *cp;
	char *msgs[128];
	int msgp, linkf;
	int ismhfile = 0;
	char **ap;
	char *arguments[50], **argp;
	char *pwd(), *pwds;

	/*
	 * Rand has committed the sin of picking a name that already is
	 * used (file).  We fix this here since we can tell the difference
	 * between the two (mh file has -'s and +'s always, bell file never
	 * has them.)
	 */
	for (i=1; i<argc; i++)
		if (argv[i][0] == '-' || argv[i][0]=='+')
			ismhfile++;
	if (!ismhfile) {
		execv(bellfile, argv);
		fprintf(stderr, "Cannot find %s\n", bellfile);
		exit(1);
	}

#ifdef NEWS
	m_news();
#endif
	folder = 0; msgp = 0; linkf = 0;
	cp = r1bindex(argv[0], '/');
	if((cp = m_find(cp)) != NULL) {
		ap = brkstring(cp = getcpy(cp), " ", "\n");
		ap = copyip(ap, arguments);
	} else
		ap = arguments;
	copyip(argv+1, ap);
	argp = arguments;
	while(cp = *argp++) {
		if(*cp == '-') {
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);       /* ambiguous */
				goto leave;
							/* unknown */
			case -1:fprintf(stderr, "file: -%s unknown\n", cp);
				goto leave;
						       /* -all */
			case 0: fprintf(stderr, "\"-all\" changed to \"all\"\n");
				goto leave;
			case 1: linkf = 1;  continue;  /* -link */
			case 2: linkf = 0;  continue;  /* -nolink */
			case 3: prsrvf = 1;  continue; /* -preserve */
			case 4: prsrvf = 0;  continue; /* -nopreserve */
			case 5: if(folder) {           /* -src */
					fprintf(stderr, "Only one src folder.\n");
					goto leave;
				}
				if(!(folder = *argp++) || *folder == '-') {
missing:        fprintf(stderr, "file: Missing argument for %s switch\n", argp[-2]);
					goto leave;
				}
				if(*folder == '+')
					folder++;
				continue;
							/* -help */
			case 6:
				if(filec >= NFOLD) {
					fprintf(stderr, "Too many src files.\n");
					goto leave;
				}
				if(!(files[filec++] = *argp) || **argp++ == '-')
					goto missing;
				continue;

			case 7: help("file   [msgs] [switches]  +folder ...",
				     switches);
				goto leave;
			}
		}
		if(*cp == '+')  {
			if(foldp < NFOLD)
				folders[foldp++].f_name = cp + 1;
			else {
				fprintf(stderr, "Only %d folders allowed.\n", NFOLD);
				goto leave;
			}
		} else
			msgs[msgp++] = cp;
	}
	if(!foldp) {
		fprintf(stderr, "No folder specified.\n");
fprintf(stderr, "Usage: file [-src folder] [msg ...] [switches] +folder [+folder]\n");
		goto leave;
	}
	if(filec > 1) {
		if(msgp) {
			fprintf(stderr, "File: Can't mix files and messages.\n");
			goto leave;
		}
		for(i = 1; i < filec; i++)
			if(*files[i] != '/') {
				if(!pwds)
					pwds = pwd();
				files[i] = concat(pwds, "/", files[i], 0);
			}
		if(opnfolds())
			goto leave;
		for(i = 1; i < filec; i++) {
			if(process(files[i]))
				goto leave;
		}
		if(!linkf) {
			if((cp = m_find("delete-prog")) != NULL) {
				files[0] = cp;
				execvp(cp, files);
				fprintf(stderr, "Can't exec deletion-prog--");
				perror(cp);
			} else for(i = 1; i < filec; i++) {
				if(unlink(files[i]) == -1) {
					fprintf(stderr, "Can't unlink ");
					perror(files[i]);
				}
			}
		}
		goto leave;
	}
	if(!msgp)
		msgs[msgp++] = "cur";
	if(!folder)
		folder = m_getfolder();
	copy(m_maildir(folder), maildir);
	if(chdir(maildir) < 0) {
		fprintf(stderr, "Can't chdir to: ");
		perror(maildir);
		goto leave;
	}
	if(!(mp = m_gmsg(folder))) {
		fprintf(stderr, "Can't read folder %s!?\n",folder);
		goto leave;
	}
	if(mp->hghmsg == 0) {
		fprintf(stderr, "No messages in \"%s\".\n", folder);
		goto leave;
	}
	for(msgnum = 0; msgnum < msgp; msgnum++)
		if(!m_convert((cp = msgs[msgnum])))
			goto leave;
	if(mp->numsel == 0) {
		fprintf(stderr, "file:  ham 'n cheese\n");       /* never get here */
		goto leave;
	}
	m_replace(pfolder, folder);
	if(mp->hghsel != mp->curmsg && ((mp->numsel != mp->nummsg) || linkf))
		m_setcur(mp->hghsel);
	if(opnfolds())
		goto leave;
	for(msgnum = mp->lowsel; msgnum <= mp->hghsel; msgnum++)
		if(mp->msgstats[msgnum] & SELECTED)
			if(process(cp = getcpy(m_name(msgnum))))
				goto leave;
			else
				cndfree(cp);
	if(!linkf) {
		if((cp = m_find("delete-prog")) != NULL) {
			if(mp->numsel > MAXARGS-2) {
	  fprintf(stderr, "file: more than %d messages for deletion-prog\n",MAXARGS-2);
				printf("[messages not unlinked]\n");
				goto leave;
			}
			vecp = 1;
			vec = (char **) calloc(MAXARGS + 2, sizeof *vec);
			for(msgnum= mp->lowsel; msgnum<= mp->hghsel; msgnum++)
				if(mp->msgstats[msgnum]&SELECTED)
					vec[vecp++] = getcpy(m_name(msgnum));
			vec[vecp] = 0;
			m_update();
			fflush(stdout);
			vec[0] = cp;
			execv(vec[0], vec);
			fprintf(stderr, "Can't exec deletion-prog--");
			perror(cp);
		} else {
			for(msgnum = mp->lowsel; msgnum <= mp->hghsel; msgnum++)
				if(mp->msgstats[msgnum] & SELECTED)
					if(unlink(cp = m_name(msgnum))== -1) {
						fprintf(stderr, "Can't unlink %s:",folder);
						perror(cp);
					}
		}
	}
leave:
	m_update();
	done(0);
}


opnfolds()
{
	register int i;
	register char *cp;
	char nmaildir[128];

	for(i = 0; i < foldp; i++) {
		copy(m_maildir(folders[i].f_name), nmaildir);
		if(access(nmaildir, 5) < 0) {
			cp = concat("Create folder \"", nmaildir, "\"? ", 0);
			if(!gans(cp, anoyes))
				goto bad;
			free(cp);
			if(!makedir(nmaildir)) {
				fprintf(stderr, "Can't create folder.\n");
				goto bad;
			}
		}
		if(chdir(nmaildir) < 0) {
			fprintf(stderr, "Can't chdir to: ");
			perror(nmaildir);
			goto bad;
		}
		if(!(folders[i].f_mp = m_gmsg())) {
			fprintf(stderr, "Can't read folder %s\n", folders[i].f_name);
			goto bad;
		}
	}
	chdir(maildir);         /* return to src folder */
	return(0);
bad:
	return(1);
}


process(msg)
char *msg;
{
	char newmsg[256], buf[BUFSIZ];
	register int i;
	register char *nmsg;
	register struct st_fold *fp;
	struct stat stbuf, stbf1;
	int n, o, linkerr;

    for(fp = folders; fp < &folders[foldp]; fp++) {
	if(prsrvf)
		nmsg = msg;
	else
		nmsg = m_name(fp->f_mp->hghmsg++ + 1);
	copy(nmsg, copy("/", copy(m_maildir(fp->f_name), newmsg)));
	if(link(msg, newmsg) < 0) {
		linkerr = errno;
		if(linkerr == EEXIST ||
		  (linkerr == EXDEV && stat(newmsg, &stbuf) != -1)) {
			if(linkerr != EEXIST || stat(msg, &stbf1) < 0 ||
			   stat(newmsg, &stbuf) < 0 ||
			   stbf1.st_ino != stbuf.st_ino) {
				fprintf(stderr, "Message %s:%s already exists.\n",
				     fp->f_name, msg);
				return(1);
			}
			continue;
		}
		if(linkerr == EXDEV) {
			if((o = open(msg, 0)) == -1) {
				fprintf(stderr, "Can't open %s:%s.\n",
					folder, msg);
				return(1);
			}
			fstat(o, &stbuf);
			if((n = creat(newmsg, stbuf.st_mode&0777)) == -1) {
				fprintf(stderr, "Can't create %s:%s.\n",
					fp->f_name, nmsg);
				close(o);
				return(1);
			}
			do
				if((i=read(o, buf, sizeof buf)) < 0 ||
				  write(n, buf, i) == -1) {
				    fprintf(stderr, "Copy error on %s:%s to %s:%s!\n",
					    folder, msg, fp->f_name, nmsg);
				    close(o); close(n);
				    return(1);
				}
			while(i == sizeof buf);
			close(n); close(o);
		} else {
			fprintf(stderr, "Error on link %s:%s to %s:",
			    folder, msg, fp->f_name);
			perror(nmsg);
			return(1);
		}
	}
cont:   ;
    }
    return(0);
}


char *
pwd()
{
	register FILE *pp;
	static char curpath[128];
	register int i;
	FILE *popen();

	if((pp = popen("pwd", "r")) == NULL ||
	    fgets(curpath, sizeof curpath, pp) == NULL ||
	    pclose(pp) != 0) {
		fprintf(stderr, "Can't find current directory!\n");
		done(1);
	}
	*rindex(curpath, '\n') = 0;     /* Zap the lf */
	return curpath;
}
