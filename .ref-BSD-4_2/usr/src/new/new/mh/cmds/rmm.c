#ifndef lint
static char sccsid[] = "@(#)rmm.c	4.1 2/23/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <strings.h>

int     vecp;
char    **vec;
struct msgs *mp;

struct swit switches[] = {
	"all",         -3,      /* 0 */
	"help",         4,      /* 1 */
	0,              0
};

main(argc, argv)
char *argv[];
{
	char *folder, *nfolder, *maildir, *msgs[100], buf[32];
	register int msgnum;
	register char *cp, *sp;
	int msgp;
	char **ap;
	char *arguments[50], **argp;

#ifdef NEWS
	m_news();
#endif
	folder = 0; msgp = 0;
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
			case -1:fprintf(stderr, "rmm: -%s unknown\n", cp);
				goto leave;
							 /* -all */
			case 0: fprintf(stderr, "\"-all\" changed to \"all\"\n");
				goto leave;
							/* -help */
			case 1: help("rmm [+folder]  [msgs] [switches]",
				     switches);
				goto leave;
			}
		if(*cp == '+')  {
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		} else
			msgs[msgp++] = cp;
	}
	if(!msgp)
		msgs[msgp++] = "cur";
	if(!folder)
		folder = m_getfolder();
	maildir = m_maildir(folder);
	if(chdir(maildir) < 0) {
		fprintf(stderr, "Can't chdir to: ");
		perror(maildir);
		goto leave;
	}
	if(!(mp = m_gmsg(folder))) {
		fprintf(stderr, "Can't read folder!?\n");
		goto leave;
	}
	if(mp->hghmsg == 0) {
		fprintf(stderr, "No messages in \"%s\".\n", folder);
		goto leave;
	}
	for(msgnum = 0; msgnum < msgp; msgnum++)
		if(!m_convert(msgs[msgnum]))
			goto leave;
	if(mp->numsel == 0) {
		fprintf(stderr, "rmm: lasagne 'n sausage\n");     /* never get here */
		goto leave;
	}
	m_replace(pfolder, folder);
	if((cp = m_find("delete-prog")) == NULL) {
		for(msgnum = mp->lowsel; msgnum <= mp->hghsel; msgnum++)
			if(mp->msgstats[msgnum] & SELECTED) {
				sp = getcpy(m_name(msgnum));
				cp = copy(sp, buf);
				cp[1] = 0;
				do
					*cp = cp[-1];
				while(--cp >= buf && *cp != '/');
#ifdef UCB
				*++cp = '#';
#else
				*++cp = ',';
#endif
				unlink(buf);
				if(link(sp, buf) == -1 || unlink(sp) == -1)
					fprintf(stderr, "Can't rename %s to %s.\n", sp, buf);
			}
	} else {
		if(mp->numsel > MAXARGS-2) {
  fprintf(stderr, "rmm: more than %d messages for deletion-prog\n",MAXARGS-2);
			goto leave;
		}
		vec = (char **) calloc(MAXARGS +2, sizeof *vec);
		vecp = 1;
		for(msgnum= mp->lowsel; msgnum<= mp->hghsel; msgnum++)
			if(mp->msgstats[msgnum]&SELECTED)
				vec[vecp++] = getcpy(m_name(msgnum));
		vec[vecp] = 0;
		vec[0] = cp;
		m_update();
		fflush(stdout);
		execv(vec[0], vec);
		fprintf(stderr, "Can't exec deletion prog--");
		perror(cp);
	}
leave:
	m_update();
	done(0);
}
