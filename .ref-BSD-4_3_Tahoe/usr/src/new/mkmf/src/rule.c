/* $Header: rule.c,v 1.2 85/05/16 12:49:56 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */
#include "Mkmf.h"
#include "null.h"
#include "rule.h"
#include "slist.h"
#include "suffix.h"
#include "system.h"
#include "yesno.h"

#define MAXNAMLEN	255

static RULEBLK *Ruletab[RULETABSIZE];	/* rules table */
static SLIST *Rulelist = NULL;		/* transformation rule list */

/*
 * buildruletable() converts a list of transformation rules into a hash table
 * for fast lookup. Returns YES if successful, otherwise NO.
 */
buildruletable()
{
	extern char *DEFRULE[];		/* default preprocessor rules */
	int i;				/* default rule list counter */
	int instalrule();		/* instale rule in hash table */
	SLBLK *rblk;			/* singly-linked rulename block */

	/* process default rules */
	for (i = 0; DEFRULE[i] != NULL; i++)
		{
		if (instalrule(DEFRULE[i]) == NO)
			{
			warn("out of memory");
			return(NO);
			}
		}

	/* process rules found in makefile */
	if (Rulelist != NULL)
		{
		for (rblk = Rulelist->head; rblk != NULL; rblk = rblk->next)
			{
			if (instalrule(rblk->key) == NO)
				{
				warn("out of memory");
				return(NO);
				}
			}
		}
	return(YES);
}



/*
 * findrule() searchs a line for a transformation rule. Returns the
 * name of the transformation rule, or NULL if not found.
 */
char *
findrule(rulename, bp)
	char *rulename;			/* transformation rule buffer */
	register char *bp;		/* I/O buffer pointer */
{ 
	register char *rp;		/* rule name pointer */
	int dotcount = 0;		/* number of '.'s in rule */

	for (rp = rulename; *bp != ':' && *bp != ' ' && *bp != '\t'; rp++, bp++)
		{
		if ((*rp = *bp) == '.')
			dotcount++;
		}
	*rp = '\0';

	/* eat up white space between rule and ':' */
	if (*bp != ':')
		{
		while (*bp == ' ' || *bp == '\t')
			bp++;
		if (*bp != ':')
			return(NULL);
		}

	return((dotcount == 2) ? rulename : NULL);
}



/*
 * instalrule() installs a source transformation rule in the rule lookup
 * table. The rule table consists of a set of singly-linked lists, indexed
 * by the first character of the suffix of the target file. The index of
 * the target file is used by lookuprule() to find out the name of the file
 * from which it was derived. Returns YES if successful, otherwise NO.
 */
instalrule(rule)
	char *rule;			/* rule to be installed in Rule table */
{
	char *malloc();			/* memory allocator */
	char *rindex();			/* find last occurrence of character */
	char *strsav();			/* save a string somewhere */
	char *target;			/* target suffix */
	int lookupsfx();		/* get suffix type */
	int ruleindex;			/* index into rule table */
	RULEBLK *rblk;			/* rule list block */

	target = rindex(rule, '.') + 1;
	if (lookupsfx(target) == SFXSRC)
		{
		ruleindex = target[0];
		if ((rblk = (RULEBLK *) malloc(sizeof(RULEBLK))) == NULL)
			return(NO);
		if ((rblk->r_rule = strsav(rule)) == NULL)
			return(NO);
		rblk->r_next = Ruletab[ruleindex];
		Ruletab[ruleindex] = rblk;
		}
	return(YES);
}



/*
 * lookuprule() applies successive transformation rules to filename, and
 * checks to see if the file exists. Returns YES if filename exists,
 * otherwise NO.
 */
lookuprule(target, source)
	char *target;			/* name of (transformed) file */
	char *source;			/* name of source file */
{
	register char *r;		/* rule pointer */
	register char *s;		/* source buffer pointer */
	char *rindex();			/* find last occurrence of character */
	char *sourcesuffix;		/* source file suffix */
	char *strcpy();			/* string copy */
	char *rulesuffix;		/* target suffix in each rule */
	char *targetsuffix;		/* transformed file suffix string */
	int ruleindex;			/* index into rule table */
	int strcmp();			/* string comparison */
	RULEBLK *rblk;			/* rule list block */

	if ((targetsuffix = rindex(target, '.')) == NULL)
		return(NO);
	ruleindex = targetsuffix[1];
	if (Ruletab[ruleindex] != NULL)
		{
		strcpy(source, target);
		sourcesuffix = rindex(source, '.');
		for (rblk=Ruletab[ruleindex]; rblk != NULL; rblk=rblk->r_next)
			{
			rulesuffix = rindex(rblk->r_rule, '.');
			if (strcmp(rulesuffix, targetsuffix) == 0)
				{
				r = rblk->r_rule;
				s = sourcesuffix;
				while (*++s = *++r)
					if (*s == '.')
						{
						*s = '\0';
						break;
						}
				if (FILEXIST(source))
					return(YES);
				}
			}
		}
	return(NO);
}



/*
 * storerule() appends a transformation rule to the end of a singly-linked
 * list. Returns integer NO if out of memory, otherwise YES.
 */
storerule(rulename)
	char *rulename;			/* transformation rule name */
{
	char *slappend();		/* append rule to list */
	SLIST *slinit();		/* initialize transformation list */

	if (Rulelist == NULL)
		Rulelist = slinit();
	if (slappend(rulename, Rulelist) == NULL)
		return(NO);
	return(YES);
}
