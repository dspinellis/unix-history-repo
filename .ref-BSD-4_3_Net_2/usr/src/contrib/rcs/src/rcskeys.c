/*
 *                     RCS keyword table and match operation
 */
#ifndef lint
static char rcsid[]= "$Id: rcskeys.c,v 4.3 89/05/01 15:13:02 narten Exp $ Purdue CS";
#endif

/* Copyright (C) 1982, 1988, 1989 Walter Tichy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Walter Tichy.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Report all problems and direct all questions to:
 *   rcs-bugs@cs.purdue.edu
 * 







*/



/* $Log:	rcskeys.c,v $
 * Revision 4.3  89/05/01  15:13:02  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.2  87/10/18  10:36:33  narten
 * Updating version numbers. Changes relative to 1.1 actuallyt
 * relative to 4.1
 * 
 * Revision 1.2  87/09/24  14:00:10  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.1  84/01/23  14:50:32  kcs
 * Initial revision
 * 
 * Revision 4.1  83/05/04  10:06:53  wft
 * Initial revision.
 * 
 */


#include "rcsbase.h"



struct { char * keyword; enum markers marker;} markertable[] =
        {{AUTHOR,   Author  },
         {DATE,     Date    },
         {HEADER,   Header  },
         {IDH,      Id      },
         {LOCKER,   Locker  },
         {LOG,      Log     },
         {RCSFILE,  RCSfile },
         {REVISION, Revision},
         {SOURCE,   Source  },
         {STATE,    State   },
         {nil,      Nomatch }};



enum markers trymatch(string,onlyvdelim)
char * string;
/* function: Checks whether string starts with a keyword followed
 * by a KDELIM or a VDELIM. If onlyvdelim==true, only a VDELIM
 * may follow the keyword.
 * If successful, returns the appropriate marker, otherwise Nomatch.
 */
{
        register int j;
	register char * p, * s;
        for (j=0; markertable[j].keyword!=nil; j++ ) {
		/* try next keyword */
		p = markertable[j].keyword; s = string;
		while (*p!='\0' && *s!='\0' && *p == *s) {
			p++; s++;
		}
		if (*p != '\0') continue; /* no match */
		if ((*s == VDELIM) || (!onlyvdelim && (*s == KDELIM)))
			return(markertable[j].marker);
        }
        return(Nomatch);
}

