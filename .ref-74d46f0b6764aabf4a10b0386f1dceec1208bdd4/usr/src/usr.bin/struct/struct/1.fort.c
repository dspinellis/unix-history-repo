/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)1.fort.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include "1.incl.h"
#include  "1.defs.h"
#include "def.h"


act(k,c,bufptr)
int k,bufptr;
char c;
	{
	long ftemp;
	struct lablist *makelab();
	switch(k)
		/*handle labels */
		{case 1:
			if (c != ' ')
				{
			ftemp = c - '0';
				newlab->labelt = 10L * newlab->labelt + ftemp;

				if (newlab->labelt > 99999L)
					{
				error("in syntax:\n","","");
					fprintf(stderr,"line %d: label beginning %D too long\n%s\n",
						begline,newlab->labelt,buffer);
					fprintf(stderr,"treating line as straight line code\n");
					return(ABORT);
					}
				}
			break;

		case 3:  nlabs++;
			newlab = newlab->nxtlab = makelab(0L);
			break;

		/* handle labsw- switches and labels */
		/* handle if statements */
		case 30:  counter++;  break;

		case 31:
			counter--;
			if (counter)  return(_if1);
			else
				{
				pred = remtilda(stralloc(&buffer[p1],bufptr - p1));
				p3 = bufptr + 1;	/* p3 pts. to 1st symbol after ) */
				flag = 1;
				return(_if2);  }

		case 45:			/* set p1 to pt.to 1st symbol of pred */
			p1 = bufptr + 1;
			act(30,c,bufptr);  break;

		/* handle do loops */
		case 61:  p1 = bufptr;  break;   /* p1 pts. to 1st symbol of increment  string */

		case 62:  counter ++;  break;

		case 63:  counter --; break;

		case 64: 
			if (counter != 0) break;
			act(162,c,bufptr);
			return(ABORT);

		case 70:  if (counter)  return(_rwp);
			r1 = bufptr;
			return(_rwlab);

		case 72:	exp = remtilda( stralloc(&buffer[r1+1],bufptr - r1 - 1));  break;

		case 73:  endlab = newlab;  
			break;

		case 74:  errlab = newlab;  
			break;

		case 75:  reflab = newlab;
			act(3,c,bufptr);
			break;

		case 76:  r1 = bufptr;  break;

		case 77:
			if (!counter)
			{
				act(111,c,bufptr);
				return(ABORT);
				}
			counter--;
			break;
		/* generate nodes of all types */
		case 111:		/* st. line code */
			stcode = remtilda(stralloc(&buffer[p3],endbuf - p3));
			recognize(STLNVX,flag);
			return(ABORT);

		case 122:			/* uncond. goto */
			recognize(ungo,flag);
			break;

		case 123:			/* assigned goto */
			act(72,c,bufptr);
			faterr("in parsing:\n","assigned goto must have list of labels","");

		case 124:			/* ass. goto, labels */
			recognize(ASGOVX, flag);
			break;

		case 125:			/* computed goto*/
			exp = remtilda( stralloc(&buffer[r1+1],bufptr - r1 - 1));
			recognize(COMPVX, flag);
			return(ABORT);

		case 133:			/* if() =  is a simple statement, so reset flag to 0 */
			flag = 0;
			act(111,c,bufptr);
			return(ABORT);

		case 141:			/* arith. if */
			recognize(arithif, 0);
			break;

		case 150:			/* label assignment */
			exp = remtilda( stralloc(&buffer[r1+1],bufptr - r1 - 1));
			recognize(ASVX, flag);
			break;

		case 162:			/*  do node */
			inc = remtilda(stralloc(&buffer[p1],endbuf - p1));
			recognize(DOVX, 0);
			break;

		case 180:			/* continue statement */
			recognize(contst, 0);
			break;

		case 200:		/* function or subroutine statement */
			progtype = sub;
			nameline = begline;
			recognize(STLNVX,0);
			break;


		case 210:		/* block data statement */
			progtype = blockdata;
			act(111,c,bufptr);
			return(ABORT);

		case 300:			/* return statement */
			recognize(RETVX,flag);
			break;


		case 350:			/* stop statement */
			recognize(STOPVX, flag);
			break;


		case 400:			/* end statement */
			if (progtype == sub)
				act(300, c, bufptr);
			else
				act(350, c, bufptr);
			return(endrt);

		case 500:
			prerw = remtilda(stralloc(&buffer[p3],r1 - p3 + 1));
			postrw = remtilda(stralloc(&buffer[r2],endbuf - r2));
			if (reflab || endlab || errlab)  recognize(IOVX,flag);
			else recognize(STLNVX,flag);
			return(ABORT);

		case 510:  r2 = bufptr;
			act(3,c,bufptr);
			act(500,c,bufptr);
			return(ABORT);

		case 520:		r2 = bufptr;
			reflab = newlab;
			act(3,c,bufptr);
			act(500,c,bufptr);
			return(ABORT);


		case 600:
			recognize(FMTVX,0);  return(ABORT);

		case 700:
			stcode = remtilda(stralloc(&buffer[p3],endbuf - p3));
			recognize(entry,0);  return(ABORT);
		/* error */
		case 999:
			printf("error: symbol '%c' should not occur as %d'th symbol of: \n%s\n",
				c,bufptr, buffer);
			return(ABORT);
		}
	return(nulls);
	}



struct lablist *makelab(x)
long x;
	{
	struct lablist *p;
	p = challoc (sizeof(*p));
	p->labelt = x;
	p->nxtlab = 0;
	return(p);
	}


long label(i)
int i;
	{
	struct lablist *j;
	for (j = linelabs; i > 0; i--)
		{
		if (j == 0) return(0L);
		j = j->nxtlab;
		}
	if (j)
		return(j->labelt);
	else
		return(0L);
	}


freelabs()
	{
	struct lablist *j,*k;
	j = linelabs;
	while(j != 0)
		{
		k = j->nxtlab;
		chfree(j,sizeof(*j));
		j = k;
		}
	}


stralloc(ad,n)			/* allocate space, copy n chars from address ad, add '0' */
int n; char *ad;
	{
	char *cp;
	cp = galloc(n+1);
	copycs(ad,cp,n);
	return(cp);
	}


remtilda(s)			/* change ~ to blank */
char *s;
	{
	int i;
	for (i = 0; s[i] != '\0'; i++)
		if (s[i] == '~') s[i] = ' ';
	return(s);
	}
