/*
* diction -- print all sentences containing one of default phrases
 *
 *	status returns:
 *		0 - ok, and some matches
 *		1 - ok, but no matches
 *		2 - some error
 */

#include <stdio.h>
#include <ctype.h>

#define	MAXSIZ 6500
#define QSIZE 650
struct words {
	char 	inp;
	char	out;
	struct	words *nst;
	struct	words *link;
	struct	words *fail;
} w[MAXSIZ], *smax, *q;

int fflag;
int nflag	= 1; /*use default file*/
char *filename;
int	nfile;
int	nsucc;
long nsent = 0;
long nhits = 0;
char *nlp;
char *begp, *endp;
int oct = 0;
FILE	*wordf;
char	*argptr;

main(argc, argv)
char **argv;
{
	while (--argc > 0 && (++argv)[0][0]=='-')
		switch (argv[0][1]) {

		case 'f':
			fflag++;
			filename = ++argv;
			argc--;
			continue;

		case 'n':
			nflag = 0;
			continue;
		case 'd':
			continue;
		default:
			fprintf(stderr, "diction: unknown flag\n");
			continue;
		}
out:
	if(nflag){
		wordf = fopen(DICT,"r");
		if(wordf == NULL){
			fprintf(stderr,"diction: can't open default dictionary\n");
			exit(2);
		}
	}
	else {
		wordf = fopen(*filename,"r");
		if(wordf == NULL){
			fprintf(stderr,"diction: can't open %s\n",filename);
			exit(2);
		}
	}

	cgotofn();
	cfail();
	nfile = argc;
	if (argc<=0) {
		execute((char *)NULL);
	}
	else while (--argc >= 0) {
		execute(*argv);
		argv++;
	}
	printf("number of sentences %ld number of hits %ld\n",nsent,nhits);
	exit(nsucc == 0);
}

execute(file)
char *file;
{
	register char *p;
	register struct words *c;
	register ccount;
	struct words *savc;
	char *savp;
	int savct;
	int scr;
	char buf[1024];
	int f;
	int hit;
	if (file) {
		if ((f = open(file, 0)) < 0) {
			fprintf(stderr, "diction: can't open %s\n", file);
			exit(2);
		}
	}
	else f = 0;
	ccount = 0;
	p = buf;
	nlp = p;
	c = w;
	oct = hit = 0;
	savc = savp = 0;
	for (;;) {
		if (--ccount <= 0) {
			if (p == &buf[1024]) p = buf;
			if (p > &buf[512]) {
				if ((ccount = read(f, p, &buf[1024] - p)) <= 0) break;
			}
			else if ((ccount = read(f, p, 512)) <= 0) break;
			convert(p,ccount);
		}
		if(p == &buf[1024])p=buf;
		nstate:
			if (c->inp == *p) {
				c = c->nst;
			}
			else if (c->link != 0) {
				c = c->link;
				goto nstate;
			}
			else {
				if(savp != 0){
					c=savc;
					p=savp;
					if(ccount > savct)ccount += savct;
					else ccount = savct;
					savc=savp=0;
					goto hadone;
				}
				c = c->fail;
				if (c==0) {
					c = w;
					istate:
					if (c->inp == *p) {
						c = c->nst;
					}
					else if (c->link != 0) {
						c = c->link;
						goto istate;
					}
				}
				else goto nstate;
			}
		if(c->out){
			if((c->inp == *(p+1)) && (c->nst != 0)){
				savp=p;
				savc=c;
				savct=ccount;
				goto cont;
			}
			else if(c->link != 0){
				savc=c;
				while((savc=savc->link)!= 0){
					if(savc->inp == *(p+1)){
						savp=p;
						savc=c;
						savct=ccount;
						goto cont;
					}
				}
			}
		hadone:
			savc=savp=0;
			if(c->out == (char)(0377)){
				c=w;
				goto nstate;
			}
			begp = p - (c->out);
			if(begp < &buf[0])begp = &buf[1024] - (&buf[0]-begp);
			endp=p;
			hit = 1;
			nhits++;
			if (*p++ == '.') {
				if (--ccount <= 0) {
					if (p == &buf[1024]) p = buf;
					if (p > &buf[512]) {
						if ((ccount = read(f, p, &buf[1024] - p)) <= 0) break;
					}
					else if ((ccount = read(f, p, 512)) <= 0) break;
					convert(p,ccount);
				}
			}
	succeed:	nsucc = 1;
			{
				if (p <= nlp) {
					outc(&buf[1024]);
					nlp = buf;
				}
				outc(p);
			}
	nomatch:
			nlp = p;
			c = w;
			begp = endp = 0;
			continue;
		}
	cont:
		if (*p++ == '.'){
				if(hit){
					if(p <= nlp){
						outc(&buf[1024]);
						nlp = buf;
					}
					outc(p);
					putchar('\n'); putchar('\n');
					}
				hit = 0;
				oct = 0;
				nlp = p;
				c = w;
				begp = endp = 0;
			}
	}
	close(f);
}

getargc()
{
	register c;
	if (wordf){
		if((c=getc(wordf))==EOF){
			fclose(wordf);
			if(nflag && fflag){
				nflag=0;
				wordf=fopen(*filename,"r");
				if(wordf == NULL){
					fprintf("can't open %s\n",filename);
					exit(2);
				}
				return(getc(wordf));
			}
			else return(EOF);
		}
		else return(c);
	}
	if ((c = *argptr++) == '\0')
		return(EOF);
	return(c);
}

cgotofn() {
	register c;
	register struct words *s;
	register ct;
	int neg;

	s = smax = w;
	neg = ct = 0;
nword:	for(;;) {
		c = getargc();
		if(c == '~'){
			neg++;
			c = getargc();
		}
		if (c==EOF)
			return;
		if (c == '\n') {
			if(neg)s->out = 0377;
			else s->out = ct-1;
			neg = ct = 0;
			s = w;
		} else {
		loop:	if (s->inp == c) {
				s = s->nst;
				ct++;
				continue;
			}
			if (s->inp == 0) goto enter;
			if (s->link == 0) {
				if (smax >= &w[MAXSIZ - 1]) overflo();
				s->link = ++smax;
				s = smax;
				goto enter;
			}
			s = s->link;
			goto loop;
		}
	}

	enter:
	do {
		s->inp = c;
		ct++;
		if (smax >= &w[MAXSIZ - 1]) overflo();
		s->nst = ++smax;
		s = smax;
	} while ((c = getargc()) != '\n' && c!=EOF);
	if(neg)smax->out = 0377;
	else smax->out = ct-1;
	neg = ct = 0;
	s = w;
	if (c != EOF)
		goto nword;
}

overflo() {
	fprintf(stderr, "wordlist too large\n");
	exit(2);
}
cfail() {
	struct words *queue[QSIZE];
	struct words **front, **rear;
	struct words *state;
	int bstart;
	register char c;
	register struct words *s;
	s = w;
	front = rear = queue;
init:	if ((s->inp) != 0) {
		*rear++ = s->nst;
		if (rear >= &queue[QSIZE - 1]) overflo();
	}
	if ((s = s->link) != 0) {
		goto init;
	}

	while (rear!=front) {
		s = *front;
		if (front == &queue[QSIZE-1])
			front = queue;
		else front++;
	cloop:	if ((c = s->inp) != 0) {
			bstart=0;
			*rear = (q = s->nst);
			if (front < rear)
				if (rear >= &queue[QSIZE-1])
					if (front == queue) overflo();
					else rear = queue;
				else rear++;
			else
				if (++rear == front) overflo();
			state = s->fail;
		floop:	if (state == 0){ state = w;bstart=1;}
			if (state->inp == c) {
			qloop:	q->fail = state->nst;
				if ((state->nst)->out != 0 && q->out == 0) q->out = (state->nst)->out;
				if((q=q->link) != 0)goto qloop;
			}
			else if ((state = state->link) != 0)
				goto floop;
			else if(bstart==0){state=0; goto floop;}
		}
		if ((s = s->link) != 0)
			goto cloop;
	}
/*	for(s=w;s<=smax;s++)
		printf("s %d ch %c out %d nst %d link %d fail %d\n",s,
			s->inp,s->out,s->nst,s->link,s->fail);
*/
}
convert(p,ccount)
char *p;
{
	int ct;
	char *pt;
	for(pt=p,ct=ccount;--ct>=0;pt++){
		if(isupper(*pt))*pt=tolower(*pt);
		else if(isspace(*pt))*pt=' ';
		else if(*pt=='.' || *pt=='?'||*pt=='!'){
			*pt='.';
			nsent++;
		}
		else if(ispunct(*pt))*pt=' ';
	}
}
outc(addr)
char *addr;
{

	while(nlp < addr){
		if(oct++ > 70 && *nlp == ' ' && nlp != begp && nlp != endp){
			oct=0;
			putchar('\n');
		}
		if(nlp == begp){
			putchar('[');
		}
		putchar(*nlp);
		if(nlp == endp){
			putchar(']');
		}
		nlp++;
	}
}
