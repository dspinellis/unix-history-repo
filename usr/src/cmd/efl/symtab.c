#include "defs"

#ifdef HASHEDTABLE
/* Basic symbol table maintainer.  Action depends on t:
t = -1	Remove name from table
t =  0	Put name in table if not there.  Copy name string
t =  1	Find name in table if there, otherwise return 0.
t =  2	Put name in table if not there.  Do not copy name
*/

struct stentry *hashtab[MAXEFLNAMES+1];
struct stentry **hashend	= hashtab+MAXEFLNAMES+1;

#define NEXT(x) (++x<hashend ? x : hashtab )

struct stentry *name(s,t)
char *s;
int t;
{
int hash;
register struct stentry *p, **hp;
char *copys();

hash = hashfunct(s);

for(hp = hashtab + hash; (p = *hp) ; hp = NEXT(hp) )
	if(hash==p->hashval && equals(s,p->namep))
		switch(t)
		{
		case -1:
			cfree(p->namep);
			cfree(p);
			delhash(hp);
			--neflnames;
			return(0);

		case 0:
		case 1:
		case 2:
			return(p);

		default:
			fatal("name: illegal argument");
		}

/* not in table */
switch(t)
	{
	case -1:
		fatal1("cannot delete nonexistent name %s from symbol table", s);

	case 1:
		return(0);

	case 0:
	case 2:
		if(++neflnames >= MAXEFLNAMES)
			fatal("hash table full");

		*hp = p = ALLOC(stentry);
		p->namep = (t==0 ? copys(s) : s);
		p->hashval = hash;
		return(p);

	default:
		fatal("illegal call to name");
	}
}



hashfunct(s)
register char *s;
{
register int h;

h = 0;
while(*s)
	h += *s++;

return( h % (MAXEFLNAMES+1) );
}


delhash(hp)
struct stentry **hp;
{
struct stentry **hq, **hvp;

for ( ; ; )
	{
	*hp = 0;
	hq = hp;
	for(hp = NEXT(hp) ; *hp &&
		( (hq < (hvp = hashtab + (*hp)->hashval) && hvp<=hp)
		|| (hp<hq && hq<hvp) || (hvp<=hp && hp<hq) ) ;
		hp = NEXT(hp) )
			;
	if(*hp == 0)
		return;
	*hq = *hp;
	}
}
#endif

#ifndef HASHEDTABLE
/* Basic symbol table maintainer.  Action depends on t:
t = -1	Remove name from table
t =  0	Put name in table if not there.  Copy name string
t =  1	Find name in table if there, otherwise return 0.
t =  2	Put name in table if not there.  Do not copy name
*/

struct stentry *hashtab[MAXEFLNAMES];
struct stentry **hashend hashtab;

name(s,t)
char *s;
int t;
{
int hash;
register struct stentry *p, **hp;
char *copys();

hash = hashfunct(s);

for(hp = hashtab ; hp<hashend ; ++hp)
	if( (p = *hp) && hash==p->hashval &&  equals(s,p->namep))
		switch(t)
		{
		case -1:
			cfree(p->namep);
			cfree(p);
			*hp = 0;
			return(0);

		case 0:
		case 1:
		case 2:
			return(p);

		default:
			fatal("name: illegal argument");
		}

/* not in table */
switch(t)
	{
	case -1:
		fatal1("cannot delete nonexistent name %s from symbol table", s);

	case 1:
		return(0);

	case 0:
	case 2:
		/* look for an empty slot */
		for(hp = hashtab ; hp<hashend && *hp!=0 ; ++hp)
			;

		if(hp == hashend)
			if(++neflnames >= MAXEFLNAMES)
				fatal("hash table full");
			else ++hashend;

		*hp = p = ALLOC(stentry);
		p->namep = (t==0 ? copys(s) : s);
		p->hashval = hash;
		return(p);

	default:
		fatal("illegal call to name");
	}
}



hashfunct(s)
register char *s;
{
register int h;

h = 0;
while(*s)
	h = *s++;

return(h);
}
#endif
