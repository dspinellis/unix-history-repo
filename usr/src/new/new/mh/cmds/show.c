#include "mh.h"
#include <stdio.h>
#include <strings.h>

int vecp;
char *vec[MAXARGS];
struct msgs *mp;
struct swit switches[] = {
	"all",         -3,      /* 0 */
	"draft",        2,      /* 1 */
	"pr",           2,      /* 2 */
	"nopr",         2,      /* 3 */
	"help",         4,      /* 4 */
	0,              0
};

main(argc, argv)
char *argv[];
{
	char *folder, *maildir, *msgs[100];
	register int msgnum;
	register char *cp, **ap;
	int msgp, all, drft, pr;
	char *arguments[50], **argp;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
#ifdef NEWS
	m_news();
#endif
	folder = (char *) 0;
	pr = all = msgp = 0;
	vecp = 1;
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
			case -1:vec[vecp++] = --cp;  continue;
							     /* -all   */
			case 0: fprintf(stderr, "\"-all\" changed to \"all\"\n");
				goto leave;
			case 1: drft = 1;  continue;         /* -draft */
			case 2: pr = 1;  continue;           /* -pr    */
			case 3: pr = 0;  vecp = 1;  continue;/* -nopr  */
			case 4:                              /* -help  */
  help("show [+folder]  [msgs] [switches] [switches for \"type\" or \"pr\" ]",
				     switches);
				goto leave;
			}
		if(*cp == '+') {
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		} else
			msgs[msgp++] = cp;
	}
	if(drft)
		maildir = m_maildir("");
	else {
		if(!msgp)
			msgs[msgp++] = "cur";
		if(!folder)
			folder = m_getfolder();
		maildir = m_maildir(folder);
	}
	if(chdir(maildir) < 0) {
		fprintf(stderr, "Can't chdir to: ");
		perror(maildir);
		goto leave;
	}
	if(drft) {
		vec[vecp++] = draft;
		goto doit;
	}
	if(!(mp = m_gmsg(folder))) {
		fprintf(stderr, "Can't read folder!?\n");
		goto leave;
	}
	if(mp->hghmsg == 0) {
		fprintf(stderr, "No messages in \"%s\".\n", folder);
		goto leave;
	}
	if(msgp)
		for(msgnum = 0; msgnum < msgp; msgnum++)
			if(!m_convert(msgs[msgnum]))
				goto leave;
	if(mp->numsel == 0) {
		fprintf(stderr, "show: potato pancakes.\n");     /* never get here */
		goto leave;
	}
	if(mp->numsel > MAXARGS-2) {
  fprintf(stderr, "show: more than %d messages for show-exec\n", MAXARGS-2);
		goto leave;
	}
	for(msgnum= mp->lowsel; msgnum<= mp->hghsel; msgnum++)
		if(mp->msgstats[msgnum]&SELECTED)
			vec[vecp++] = getcpy(m_name(msgnum));
	m_replace(pfolder, folder);
	if(mp->hghsel != mp->curmsg)
		m_setcur(mp->hghsel);
	if(vecp == 2 ) {
		printf("(Message %s:%s)\n", folder, vec[1]);
	}
doit:   m_update();
	fflush(stdout);
	vec[vecp] = 0;
	if(!pr) {
		vec[0] = "c:mh-type";
		execv(showproc, vec);
	} else {
		vec[0] = "mh-pr";
		execv(prproc, vec);
	}
	perror(pr ? prproc : showproc);
 leave:
	m_update();
	done(0);
}


