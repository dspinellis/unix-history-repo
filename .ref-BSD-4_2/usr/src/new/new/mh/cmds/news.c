#include "mh.h"
#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <strings.h>
#define max(a,b) (a > b ? a : b)

#define NEWSP   "/usr/spool/newnews"
#define MAXTOPICS       20      /* Enough for a while */

int     vecp, topicp;
char    *topics[MAXTOPICS+1];
char    *vec[MAXARGS];
struct msgs *mp;
int     fdisplay, fcheck, fupdate, fsend, freview, fbody;  /* flags */
int     frevback;
int     hit;

struct nts {
	char    *t_name;
	int     t_num;
} nts[MAXTOPICS+1], *check();

struct swit switches[] = {
	"add",          -1,     /* 0 */
	"body",         -1,     /* 1 */
	"check",        0,      /* 2 */
	"display",      0,      /* 3 */
	"review [#]",   0,      /* 4 */
	"send topic ...",0,     /* 5 */
	"topics",       0,      /* 6 */
	"update",       0,      /* 7 */
	"help",         4,      /* 8 */
	0,              0,
};

main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	register char *cp, **ap;
	char *arguments[50], **argp;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
#ifdef NEWS
	m_news();
#endif
	vecp = 2;
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
				done(0);
							     /* unknown */
			case -1:vec[vecp++] = --cp; continue;
			case 0:
			case 5: fsend++; continue;
			case 1: fbody++;
				vec[vecp++] = --cp; continue;
			case 2: fcheck++; continue;
			case 3: fdisplay++; continue;
			case 4: freview++;
				if(**argp >= '0' && **argp <= '9')
					frevback = atoi(*argp++);
				continue;
			case 7: fupdate++; continue;

			case 8:
  help("news [topic ...] [switches] [switches for \"c\" or \"mail\" ]",
				     switches);
				putchar('\n');
			case 6:
				printf("Topic    items\n\n"); fflush(stdout);
				getnts();
				for(i = 0; nts[i].t_name[0]; i++)
					printf("%-9s%3d\n", nts[i].t_name,
						nts[i].t_num);
				done(0);
			}
		else
			if(vecp == 2)
				topics[topicp++] = cp;
			else
				vec[vecp++] = cp;
	}
	getnts();
	if(fsend) {
		if(topicp == 0) {
			fprintf(stderr, "Usage: news -send topic [mail switches]\n");
			done(1);
		}
		for(i = 0; i < topicp; i++)
			if(check(topics[i]))
				send(topics[i]);
			else
				printf("Unknown topic: %s\n", topics[i]);
		done(0);
	}
	if(!topicp)
		for(i = 0; nts[i].t_name[0]; i++)
			disp(nts[i].t_name, 0);
	else
		for(i = 0; i < topicp; i++)
			if(check(topics[i]) == NULL)
				printf("Topic: %s unknown.\n", topics[i]);
			else
				disp(topics[i], 1);
	if(fcheck) {
		if(hit)
			printf(".\n");
	} else if(!hit && !topicp && !fupdate && !fdisplay)
		nonews();
	m_update();
	done(0);
}


getnts()
{
	struct direct *dir;
	struct stat st;
	register struct nts *t;
	register DIR *d;
	char tbuf[16 + 16];

	t = nts;
	if((d = opendir(NEWSP)) == 00) {
		fprintf(stderr, "Can't open ");
		perror(NEWSP);
		done(1);
	}
	while (dir = readdir(d))
		if(dir->d_ino && dir->d_name[0] != '.') {
			t->t_name = (char *)malloc(dir->d_namlen + 1);
			if (t->t_name == 0)
				break;
			strcpy(t->t_name, dir->d_name);
			t++;
		}
	closedir(d);
	for(t = nts; t->t_name[0]; t++) {
		sprintf(tbuf, "%s/.%.14s", NEWSP, t->t_name);
		if(stat(tbuf, &st) != -1)
			t->t_num = st.st_size;
	}
}

struct nts *
check(topic)
	char *topic;
{
	register struct nts *t;

	for(t = nts; t->t_name[0]; t++)
		if(strcmp(topic, t->t_name) == 0)
			return t;
	return 0;
}


disp(topic, argflg)
	register char *topic;
	int argflg;
{
	register struct nts *t;
	register char *cp, *np;
	register int msgnum;
	int high;
	char buf[128];

	if((t = check(topic)) == 0)
		fprintf(stderr, "HUH?\n");
	if((cp = m_find(np = concat("news-", topic, 0))) == NULL)
		cp = "0";
	high = atoi(cp);
	if(fcheck) {
		if(t->t_num > high) {
			if(!hit++)
				printf("Unread news in");
			if(hit > 1)
				printf(",");
			printf(" %s", topic);
		}
		return;
	}
	if(fupdate) {
		if(t->t_num > high) {
			m_replace(np, getcpy(m_name(t->t_num)));
			printf("Skipping %d items in %s.\n",
				t->t_num - high, topic);
		}
		return;
	}
	if(freview)
		if(frevback)
			msgnum = max(high - frevback, 0);
		else
			msgnum = 0;
	else
		msgnum = high;
/***    msgnum = freview? frevback? high - frevback : 0 : high; */
	if(msgnum >= t->t_num) {
		if(argflg)
			printf("%s: no new news.\n", topic);
		return;
	}
	sprintf(buf, "%s/%s", NEWSP, topic);
	if(chdir(buf) == -1) {
		perror(buf);
		return;
	}
	vec[1] = showproc;
	for( ; msgnum < t->t_num;) {
		cp = m_name(++msgnum);
		if(hit) {
			printf("\nPress <return> for %s:%s...", topic, cp);
			fflush(stdout);
			read(0, buf, sizeof buf);
		} else
			printf("News item %s:%s\n", topic, cp);
		if(msgnum > high) {
			m_replace(np, getcpy(cp));
			m_update();
		}
		vec[vecp] = cp;
		putchar('\n');
		call(vec + 1);
		hit = 1;
	}
}


call(vector)
	char **vector;
{
	register int pid, child;
	char path[32];
	int status;

	fflush(stdout);
	while((child = fork()) == -1) {
		printf("No forks...\n"); fflush(stdout);
		sleep(2);
	}
	if(child == 0) {
		execv(vector[0], vector);
		perror(vector[0]);
		done(1);
	}
	while((pid = wait(&status)) != -1 && pid != child) ;
	if(pid == -1 || status) {
		fprintf(stderr, "Abnormal termination from %s\n", vector[0]);
		done(1);
	}
}

char    *mailproc;

send(topic)
	register char *topic;
{
	vec[0] = mailproc;
	vec[1] = concat("news.", topic, 0);
	if(!fbody)
		printf("Enter text for %s\n", topic);
	call(vec);
	free(vec[1]);
}


char    *nons[] = {
	"No new news",
	"No news to peruse",
	"Only old news",
	"News shortage",
	"News reporters on strike",
	"Report your own news",
	"News presses broken"
};
#define NONS    (sizeof nons/ sizeof nons[0])

nonews()
{
#include <sys/timeb.h>
	struct timeb tb;

	ftime(&tb);
	printf("%s.\n", nons[tb.millitm % NONS]);
}
