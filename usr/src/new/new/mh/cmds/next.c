#include "mh.h"
#include <stdio.h>
#include <strings.h>

int     glbtype;
struct msgs *mp;

struct swit switches[] = {
	"help",         4,      /* 0 */
	0,              0
};

main(argc, argv)
char *argv[];
{
	char *maildir, *vec[20], *folder, *nfolder;
	register int msgnum;
	register char *cp;
	int next;
	int vecp;
	char **ap;
	char *arguments[50], **argp;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
#ifdef NEWS
	m_news();
#endif
	next = glbtype;
	folder = 0; vecp = 2;
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
			case 0: if(next > 0)                 /* -help  */
       help("next [+folder]   [switches] [switches for \"type\" ]", switches);
				else
       help("prev [+folder]   [switches] [switches for \"type\" ]", switches);
				goto leave;
			}
		if(*cp == '+') {
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		} else {
			fprintf(stderr, "Bad arg: %s\n", cp);
			fprintf(stderr, "Usage: %s [+folder] [-l.switches]\n",
			     next>0? "next" : "prev");
			goto leave;
		}
	}
	vec[vecp] = 0;
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
	if(!m_convert(next > 0 ? "next" : "prev"))
		goto leave;
	m_replace(pfolder, folder);
	if(mp->lowsel != mp->curmsg)
		m_setcur(mp->lowsel);
	vec[1] = m_name(mp->lowsel);
	printf("(Message %s:%s)\n", folder, vec[1]);
	fflush(stdout);
	vec[0] = "mh-type";
	m_update();
	execv(showproc, vec);
	perror("Can't exec type");
 leave:
	m_update();
	done(0);
}
