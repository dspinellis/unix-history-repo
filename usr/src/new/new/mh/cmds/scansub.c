#include "mh.h"
#include <stdio.h>

#define _FROM    1
#define _NOTFROM 0


#define FROM    13              /* Start of From field          */
#define SFROM   16              /* Length of "        "         */
#define DATE     5              /* Start of Date field          */
#define SDATE    7              /* Length                       */
#define SUBJ    31              /* Start of Subject field       */
#define SSUBJ   (79-SUBJ)       /* Size of Subject field        */
#define BSUBJ   20              /* Room needed in Sub field to  */
				/* add stuff from the body      */
#define MSGN     0              /* Start of msg name field      */
#define SMSGN    3              /* Length                       */
#define FLGS     3              /* Start of flags field         */
#define SFLGS    2              /* Width of flag field          */

FILE  *scnout;
char scanl[82];
int local;
int hostseen;
char *frmtok();

scan(inb, innum, outnum, curflg)
struct iobuf *inb;
int outnum;
{

	char buf[BUFSIZ], name[NAMESZ], tobuf[32], frombuf[32];
	register char *cp, **tok1;
	int state, subsz, first, compnum;
	static char *myname;

	local = 0; hostseen = 0;
	if(!myname)
		myname = getenv("USER");
	tobuf[0] = 0; frombuf[0] = 0;
	first = 0;
	state = FLD;
	compnum = 1;

	for(;;) {

		state = m_getfld(state, name, buf, sizeof buf, inb);
		if(!first++ && state != FILEEOF) {      /*##*/
		    if(outnum) {
			if((scnout = fopen(cp = m_name(outnum), "w")) == NULL) {
				fprintf(stderr, "Error creating msg ");
				perror(cp); done(-1);
			}
			chmod(cp, m_gmprot());
		    }
		    sfill(scanl, sizeof scanl);
		    scanl[sizeof scanl - 1] = 0;
		    subsz = 0;
		    tobuf[0] = 0;
		}

		switch(state) {

		case FLD:
		case FLDEOF:
		case FLDPLUS:
			compnum++;
			if(uleq(name, "from"))
				frombuf[
				   cpyfrm(buf,frombuf,sizeof frombuf,_FROM)]=0;
			else if(uleq(name, "date"))
				cpydat(buf, scanl+DATE, SDATE);
			else if(uleq(name, "subject") && scanl[SUBJ] == ' ')
				subsz = cpy(buf, scanl+SUBJ, SSUBJ);
			else if(uleq(name, "to") && !tobuf[0])
				tobuf[
				   cpyfrm(buf,tobuf,sizeof tobuf-1,_NOTFROM)]=0;
			else if(uleq(name, "replied"))
				cpy("-", scanl+FLGS+1, 1);
			put(name, buf, scnout);
			while(state == FLDPLUS) {
				state=m_getfld(state,name,buf,sizeof buf,inb);
				if(scnout)
					fputs(buf, scnout);
			}
			if(state == FLDEOF)
				goto putscan;
			continue;

		case BODY:
		case BODYEOF:
			compnum = -1;
			if(buf[0] && subsz < SSUBJ - BSUBJ) {
				scanl[SUBJ+subsz+1] = '<';
				scanl[SUBJ+subsz+2] = '<';
				cpy(buf, scanl+SUBJ+subsz+3, SSUBJ-subsz-3);
				subsz = SSUBJ;
			}
			if(buf[0] && scnout) {
				putc('\n', scnout);
				fputs(buf, scnout);
				if(ferror(scnout)) {
					fprintf(stderr, "Write error on ");
					perror(m_name(outnum));done(-1);
				}
			}
	body:           while(state == BODY) {
				state=m_getfld(state,name,buf,sizeof buf,inb);
				if(scnout)
					fputs(buf, scnout);
			}
			if(state == BODYEOF) {
		  putscan:      cpymsgn(m_name(innum), scanl+MSGN, SMSGN);
				tok1= brkstring(getcpy(frombuf), " ", "\n");
				if(!frombuf[0] || uleq(frombuf, myname) ||
				  (local && uleq(*tok1, myname))) {
					cpy("To:", scanl+FROM, 3);
					cpy(tobuf, scanl+FROM+3, SFROM-3);
				} else
					cpy(frombuf, scanl+FROM, SFROM);
				if(curflg)
					cpy("+", scanl+FLGS, SFLGS);
				trim(scanl);
				fputs(scanl, stdout);

				if(scnout) {
					fflush(scnout);
					if(ferror(scnout)) {
						perror("Write error on ");
						perror(m_name(outnum));
						done(-1);
					}
					fclose(scnout);
					scnout = NULL;
				}
				return(1);
			}
			break;

		case LENERR:
		case FMTERR:
			fprintf(stderr, "??Message Format Error ");
			fprintf(stderr, "(Message %d) ", outnum ? outnum :innum);/*##*/
			if(compnum < 0) fprintf(stderr, "in the Body.\n");
			else fprintf(stderr, "in Component #%d.\n", compnum);
			fprintf(stderr, "-----------------------------------------");
			fprintf(stderr, "-------------------------------------\n");
			goto badret;
		default:
			fprintf(stderr, "Getfld returned %d\n", state);


	badret:         if(outnum) {
				fputs("\n\nBAD MSG:\n", scnout);
				if(compnum < 0)
					fputs(buf, scnout);
				else
					fputs(name, scnout);
			/***    ungetc(inb);    ***/
				state = BODY;
				goto  body;

			}
			if(scnout)
				fflush(scnout);
			return(-1);
		case FILEEOF:
			return(0);

		}

	}
}


trim(str)
char *str;
{
	register char *cp;

	cp = str;
	while(*cp) cp++;
	while(*--cp == ' ') ;
	cp++;
	*cp++ = '\n';
	*cp++ = 0;
}

sfill(str, cnt)
char *str;
{
	register char *cp;
	register int i;

	cp = str;  i = cnt;
	do
		*cp++ = ' ';
	while(--i);
}


put(name, buf, ip)
register FILE *ip;
{
	if(ip) {
		fputs(name, ip);
		putc(':', ip);
		fputs(buf, ip);
		if(ferror(ip)) { perror("Write error");done(-1);}
	}
}


cpy(from, to, cnt)
register char *from, *to;
register int cnt;
{
	register int c;
	char *sfrom;

	sfrom = from;
	while(*from == ' ' || *from == '\t' || *from == '\n')
		from++;
	while(cnt--)
		if(c = *from) {
			if(c == '\t' || c == ' ' || c == '\n') {
				*to++ = ' ';
				do 
					from++;
				while((c= *from)==' '||c=='\t'||c=='\n');
				continue;
			} else
				*to++ = c;
			from++;
		} else
			break;
	return(from - sfrom - 1);
}

int *localtime();
char *findmonth();

cpydat(sfrom, sto, cnt)
char *sfrom, *sto;
{
	register char *from, *cp;
	register int c;
	static int *locvec;
	long now;
	char *to;

	if(!locvec) {
		time(&now);
		locvec = localtime(&now);
	}
	to = sto;
	for(from = sfrom; (c = *from) < '0' || c > '9'; from++)
		if(!c)
			return;
	c = cnt;
	for(cp = from; (*cp >= '0' && *cp <= '9') || *cp == ' '; cp++);
	if(cp = findmonth(cp)) {
		if(!cp[1]) {
			*to++ = ' ';
			c--;
		}
		while(*cp && c--)
			*to++ = *cp++;
		c--;  *to++ = '/';
		if(from[1] == ' ') {
			*to++ = ' ';
			c--;
		}
		while(*from >= '0' && *from <= '9' && c--)
			*to++ = *from++;
		if(c >= 2) {
			while(*from < '0' || *from > '9') from++;
			if(((c = atoi(from)) > 1970 && c-1900 < locvec[5])
			    || c < locvec[5])  {
				*to++ = '/';
				*to++ = (c < 100) ? (c - 70 + '0')
						  : (c - 1970 + '0');
			}
		}
		return;
	}
	if(from[1] == ' ') {
		*to++ = ' ';
		c--;
	}
	while(*from && c--)
		*to++ = *from++;
}


char    *fromp, fromdlm, pfromdlm;

cpyfrm(sfrom, sto, cnt, fromcall)
char *sfrom, *sto;
{
	register char *to, *cp;
	register int c;

	fromdlm = ' ';
	fromp = sfrom; to = sto;
	cp = frmtok();
	do
		if(c = *cp++)
			*to++ = c;
		else
			break;
	while(--cnt);
	for(;;) {
		if(cnt < 3) break;
		if(*(cp = frmtok()) == 0) break;
		if(*cp == '@' || uleq(cp, "at")) {
			cp = frmtok();
			if(uleq(cp, "berkeley")) {
				/* if the first "From:" host is local */
				if(fromcall && !hostseen++)
					local++;
			} else {
				*to++ = '@';
				cnt--;
				do
					if(c = *cp++)
						*to++ = c;
					else
						break;
				while(--cnt);
			}
		} else if(cnt > 4) {
			cnt--; *to++ = pfromdlm;
			do
				if(c = *cp++)
					*to++ = c;
				else
					break;
			while(--cnt);
		}
	}
	if(fromcall)
		hostseen++;
	return(to - sto);
}


char *frmtok()
{
	static char tokbuf[64];
	register char *cp;
	register int c;

	pfromdlm = fromdlm;
	cp = tokbuf; *cp = 0;
	while(c = *fromp++) {
		if(c == '\t')
			c = ' ';
		if(c == ' ' && cp == tokbuf)
			continue;
		if(c == ' ' || c == '\n' || c == ',')
			break;
		*cp++ = c;
		*cp = 0;
		if(c == '@' || *fromp == '@' || cp == &tokbuf[63])
			break;
	}
	fromdlm = c;
	return(tokbuf);
}


/*      num specific!         */

cpymsgn(msgnam, addr, len)
char *msgnam, *addr;
{
	register char *cp, *sp;

	sp = msgnam;
	cp = addr + (len - strlen(sp));
	while(*sp)
		*cp++ = *sp++;
}

char *monthtab[] = {
	"jan", "feb", "mar", "apr", "may", "jun",
	"jul", "aug", "sep", "oct", "nov", "dec",
};

char *findmonth(str)
char *str;
{
	register char *cp, *sp;
	register int i;
	static char buf[4];
	char *locv();

	for(cp=str, sp=buf; (*sp++ = *cp++) && sp < &buf[3] && *cp != ' '; );
	*sp = 0;
	for(i = 0; i < 12; i++)
		if(uleq(buf, monthtab[i])) {
			sprintf(buf, "%2d", i+1);
			return buf;
		}
	return(0);
}
