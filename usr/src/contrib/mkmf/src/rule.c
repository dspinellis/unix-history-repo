/*
 * Copyright (c) 1985, 1991 Peter J. Nicklin.
 * Copyright (c) 1991 Version Technology.
 * All Rights Reserved.
 *
 * $License: VT.1.1 $
 * Redistribution and use in source and binary forms,  with or without
 * modification,  are permitted provided that the following conditions
 * are met:  (1) Redistributions of source code must retain the  above
 * copyright  notice,  this  list  of  conditions  and  the  following
 * disclaimer.  (2) Redistributions in binary form must reproduce  the
 * above  copyright notice,  this list of conditions and the following
 * disclaimer in the  documentation  and/or other  materials  provided
 * with  the  distribution.  (3) All advertising materials  mentioning
 * features or  use  of  this  software  must  display  the  following
 * acknowledgement:  ``This  product  includes  software  developed by
 * Version Technology.''  Neither the name of Version  Technology  nor
 * the  name  of  Peter J. Nicklin  may  be used to endorse or promote
 * products derived from this software without specific prior  written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
 * EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
 * IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
 * LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Report problems and direct questions to nicklin@netcom.com
 *
 * $Header: rule.c,v 4.5 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include "Mkmf.h"
#include "null.h"
#include "rule.h"
#include "slist.h"
#include "stringx.h"
#include "suffix.h"
#include "yesno.h"

static RULEBLK *Ruletab[RULETABSIZE];	/* rules table */
static SLIST *Rulelist = NULL;		/* transformation rule list */

/*
 * applyrule() applies successive transformation rules to filename, and
 * checks to see if the file exists. Returns YES if filename exists,
 * otherwise NO.
 */
applyrule(target, source)
	char *target;			/* name of (transformed) file */
	char *source;			/* name of source file */
{
	register char *r;		/* rule pointer */
	register char *s;		/* source buffer pointer */
	char *sourcesuffix;		/* source file suffix */
	char *rulesuffix;		/* target suffix in each rule */
	char *targetsuffix;		/* transformed file suffix string */
	int ruleindex;			/* index into rule table */
	RULEBLK *rblk;			/* rule list block */

	if ((targetsuffix = strrchr(target, '.')) == NULL)
		return(NO);
	ruleindex = targetsuffix[1];
	if (Ruletab[ruleindex] != NULL)
		{
		strcpy(source, target);
		sourcesuffix = strrchr(source, '.');
		for (rblk=Ruletab[ruleindex]; rblk != NULL; rblk=rblk->r_next)
			{
			rulesuffix = strrchr(rblk->r_rule, '.');
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
 * buildruletable() converts a list of transformation rules into a hash table
 * for fast lookup. Returns YES if successful, otherwise NO.
 */
buildruletable()
{
	extern char *DEFRULE[];		/* default preprocessor rules */
	int i;				/* default rule list counter */
	int instalrule();		/* install rule in hash table */
	SLBLK *rblk;			/* singly-linked rulename block */

	/* process default rules */
	for (i = 0; DEFRULE[i] != NULL; i++)
		{
		if (instalrule(DEFRULE[i]) == NO)
			{
			nocore();
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
				nocore();
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
 * the target file is used by applyrule() to find out the name of the file
 * from which it was derived. Returns YES if successful, otherwise NO.
 */
instalrule(rule)
	char *rule;			/* rule to be installed in Rule table */
{
	char *malloc();			/* memory allocator */
	char *strsav();			/* save a string somewhere */
	char *target;			/* target suffix */
	int lookupsfx();		/* get suffix type */
	int ruleindex;			/* index into rule table */
	RULEBLK *rblk;			/* rule list block */

	target = strrchr(rule, '.') + 1;
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
 * lookuprule() returns YES if rule exists, otherwise NO.
 */
lookuprule(rule)
	char *rule;			/* .x.y rule to find */
{
	char *targetsuffix;		/* transformed file suffix string */
	int ruleindex;			/* index into rule table */
	RULEBLK *rblk;			/* rule list block */

	if ((targetsuffix = strrchr(rule, '.')) == NULL)
		return(NO);
	ruleindex = targetsuffix[1];
	if (Ruletab[ruleindex] != NULL)
		{
		for (rblk=Ruletab[ruleindex]; rblk != NULL; rblk=rblk->r_next)
			{
			if (strcmp(rule, rblk->r_rule) == 0)
				return(YES);
			}
		}
	return(NO);
}



/*
 * makerule() creates a rule from the suffixes of two file names.
 */
void
makerule(rule, source, target)
	char *rule;			/* buffer to hold rule */
	char *source;			/* source file name */
	char *target;			/* target file name */
{
	strcpy(rule, strrchr(source, '.'));
	strcat(rule, strrchr(target, '.'));
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
