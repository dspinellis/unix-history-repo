/*
 *                     RCS keyword table and match operation
 */
#ifndef lint
static char rcsid[]= "$Id: rcskeys.c,v 4.2 87/10/18 10:36:33 narten Exp $ Purdue CS";
#endif
/**********************************************************************************
 **********************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 * Report problems and direct all inquiries to Tichy@purdue (ARPA net).
 */


/* $Log:	rcskeys.c,v $
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

