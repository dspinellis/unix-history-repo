/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dprog.c	4.5 (Berkeley) %G%";
#endif /* not lint */

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
#include "pathnames.h"

#define	MAXSIZ 6500
#define QSIZE 650
int linemsg;
long olcount;
long lcount;
struct words {
	char 	inp;
	char	out;
	struct	words *nst;
	struct	words *link;
	struct	words *fail;
} w[MAXSIZ], *smax, *q;

char table[128] = {
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, ' ', 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	' ', '.', ' ', ' ', ' ', ' ', ' ', ' ',
	' ', ' ', ' ', ' ', ' ', ' ', '.', ' ',
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', ' ', ' ', ' ', ' ', ' ', '.',
	' ', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
	'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
	'x', 'y', 'z', ' ', ' ', ' ', ' ', ' ',
	' ', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
	'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
	'x', 'y', 'z', ' ', ' ', ' ', ' ', ' '
	};
int	caps = 0;
int	lineno = 0;
int fflag;
int nflag	= 1; /*use default file*/
char *filename;
int	mflg	= 0;	/*don't catch output*/
int	nfile;
int	nsucc;
long nsent = 0;
long nhits = 0;
char *nlp;
char *begp, *endp;
int beg, last;
char *myst;
int myct = 0;
int oct = 0;
FILE	*wordf;
FILE *mine;
char	*argptr;
long tl = 0;
long th = 0;

main(argc, argv)
char *argv[];
{
	int sv;
	while (--argc > 0 && (++argv)[0][0]=='-')
		switch (argv[0][1]) {

		case 'f':
			fflag++;
			filename = (++argv)[0];
			argc--;
			continue;

		case 'n':
			nflag = 0;
			continue;
		case 'd':
			mflg=0;
			continue;
		case 'c':
			caps++;
			continue;
		case 'l':
			lineno++;
			continue;
		default:
			fprintf(stderr, "diction: unknown flag\n");
			continue;
		}
out:
	if(nflag){
		wordf = fopen(_PATH_DICT,"r");
		if(wordf == NULL){
			fprintf(stderr,"diction: can't open default dictionary\n");
			exit(2);
		}
	}
	else {
		wordf = fopen(filename,"r");
		if(wordf == NULL){
			fprintf(stderr,"diction: can't open %s\n",filename);
			exit(2);
		}
	}

#ifdef CATCH
	if(fopen(CATCH,"r") != NULL)
		if((mine=fopen(CATCH,"a"))!=NULL)mflg=1;
#endif
#ifdef MACS
	if(caps){
		printf(".so ");
		printf(MACS);
		printf("\n");
	}
#endif
	cgotofn();
	cfail();
	nfile = argc;
	if (argc<=0) {
		execute((char *)NULL);
	}
	else while (--argc >= 0) {
		execute(*argv);
		if(lineno){
			printf("file %s: number of lines %ld number of phrases found %ld\n",
				*argv, lcount-1, nhits);
			tl += lcount-1;
			th += nhits;
			sv = lcount-1;
			lcount = nhits = 0;
		}
		argv++;
	}
	if(mflg)fprintf(mine,"number of sentences %ld %ld number of hits %ld %ld\n",nsent,tl,nhits,th);
	if(!caps&& !lineno)printf("number of sentences %ld number of phrases found %ld\n",nsent,nhits);
	else if(tl != sv)
		 if(!caps)printf("totals: number of lines %ld number of phrases found %ld\n",tl,th);
	exit(nsucc == 0);
}

execute(file)
char *file;
{
	register char *p;
	register struct words *c;
	register ccount;
	int count1;
	char *beg1;
	struct words *savc;
	char *savp;
	int savct;
	int scr;
	char buf[1024];
	int f;
	int hit;
	last = 0;
	if (file) {
		if ((f = open(file, 0)) < 0) {
			fprintf(stderr, "diction: can't open %s\n", file);
			exit(2);
		}
	}
	else f = 0;
	lcount = olcount = 1;
	linemsg = 1;
	ccount = 0;
	count1 = -1;
	p = buf;
	nlp = p;
	c = w;
	oct = hit = 0;
	savc = (struct words *) 0;
	savp = (char *) 0;
	for (;;) {
		if(--ccount <= 0) {
			if (p == &buf[1024]) p = buf;
			if (p > &buf[512]) {
				if ((ccount = read(f, p, &buf[1024] - p)) <= 0) break;
			}
			else if ((ccount = read(f, p, 512)) <= 0) break;
			if(caps && (count1 > 0))
				fwrite(beg1,sizeof(*beg1),count1,stdout);
			count1 = ccount;
			beg1 = p;
		}
		if(p == &buf[1024])p=buf;
		nstate:
			if (c->inp == table[*p]) {
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
					savc = (struct words *) 0;
					savp = (char *) 0;
					goto hadone;
				}
				c = c->fail;
				if (c==0) {
					c = w;
					istate:
					if (c->inp == table[*p]) {
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
			if((c->inp == table[*(p+1)]) && (c->nst != 0)){
				savp=p;
				savc=c;
				savct=ccount;
				goto cont;
			}
			else if(c->link != 0){
				savc=c;
				while((savc=savc->link)!= 0){
					if(savc->inp == table[*(p+1)]){
						savp=p;
						savc=c;
						savct=ccount;
						goto cont;
					}
				}
			}
		hadone:
			savc = (struct words *) 0;
			savp = (char *) 0;
			if(c->out == (char)(0377)){
				c=w;
				goto nstate;
			}
			begp = p - (c->out);
			if(begp < &buf[0])begp = &buf[1024] - (&buf[0]-begp);
			endp=p;
			if(mflg){
				if(begp-20 < &buf[0]){
					myst = &buf[1024]-20;
					if(nlp < &buf[512])myst=nlp;
				}
				else myst = begp-20;
				if(myst < nlp)myst = nlp;
				beg = 0;
			}
			hit = 1;
			nhits++;
			if(*p == '\n')lcount++;
			if (table[*p++] == '.') {
				linemsg = 1;
				if (--ccount <= 0) {
					if (p == &buf[1024]) p = buf;
					if (p > &buf[512]) {
						if ((ccount = read(f, p, &buf[1024] - p)) <= 0) break;
					}
					else if ((ccount = read(f, p, 512)) <= 0) break;
					if(caps && (count1 > 0))
						fwrite(beg1,sizeof(*beg1),count1,stdout);
					count1=ccount;
					beg1=p;
				}
			}
	succeed:	nsucc = 1;
			{
				if (p <= nlp) {
					outc(&buf[1024],file);
					nlp = buf;
				}
				outc(p,file);
			}
			if(mflg)last=1;
	nomatch:
			nlp = p;
			c = w;
			begp = endp = 0;
			continue;
		}
	cont:
		if(*p == '\n')lcount++;
		if (table[*p++] == '.'){
				if(hit){
					if(p <= nlp){
						outc(&buf[1024],file);
						nlp = buf;
					}
					outc(p,file);
					if(!caps)printf("\n\n");
					if(mflg && last){putc('\n',mine);myct = 0;}
					}
				linemsg = 1;
				if(*p == '\n')olcount = lcount+1;
				else
					olcount=lcount;
				last = 0;
				hit = 0;
				oct = 0;
				nlp = p;
				c = w;
				begp = endp = 0;
				nsent++;
			}
	}
	if(caps && (count1 > 0))
		fwrite(beg1,sizeof(*beg1),count1,stdout);
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
				wordf=fopen(filename,"r");
				if(wordf == NULL){
					fprintf(stderr,
					    "diction can't open %s\n",filename);
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
outc(addr,file)
char *addr;
char *file;
{
	int inside;

	inside = 0;
	if(!caps && lineno && linemsg){
		printf("beginning line %ld",olcount);
		if(file != (char *)NULL)printf(" %s\n",file);
		else printf("\n");
		linemsg = 0;
	}
	while(nlp < addr){
		if(!caps && oct > 60 && table[*nlp] == ' ' && nlp != begp && nlp != endp){
			oct=0;
			putchar('\n');
		}
		if(nlp == begp){
			if(caps)inside++;
			else {
				if( oct >45){putchar('\n');
					oct=0;
				}
				if( oct==0 || table[*nlp] != ' '){
					printf("*[");
					oct+=2;
				}
				else {printf(" *[");;
					oct+=3;
				}
			}
			if(mflg)putc('[',mine);
		}
		if(inside){
			if(islower(*nlp))*nlp = toupper(*nlp);
		}
		else {
			if(!caps && *nlp == '\n')*nlp = ' ';
			if(*nlp == ' ' && oct==0);
			else if(!caps) {putchar(*nlp); oct++;}
		}
		if(nlp == endp){
			if(caps)
				inside= 0;
			else {
				if(*(nlp) != ' '){printf("]*");
					oct+=2;
				}
				else {printf("]* ");
					oct+=3;
				}
				if(oct >60){putchar('\n');
					oct=0;
				}
			}
			if(mflg)putc(']',mine);
			beg = 0;
		}
		if(mflg){
			if(nlp == myst)beg = 1;
			if(beg || last){
				putc(*nlp,mine);
				if(myct++ >= 72 || last == 20){
					putc('\n',mine);
					if(last == 20)last=myct=0;
					else myct=0;
				}
				if(last)last++;
			}
		}
		nlp++;
	}
}
