/* $Id: search.h,v 3.0 1991/09/09 20:27:37 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#ifndef NBRA
#define	NBRA	10		/* the maximum number of meta-brackets in an
				   RE -- \( \) */
#define NALTS	10		/* the maximum number of \|'s */
 
typedef struct {	
    char *expbuf;		/* The compiled search string */
    int eblen;			/* Length of above buffer */
    char *alternatives[NALTS];	/* The list of \| seperated alternatives */
    char *braslist[NBRA];	/* RE meta-bracket start list */
    char *braelist[NBRA];	/* RE meta-bracket end list */
    char *brastr;		/* saved match string after execute() */
    char nbra;			/* The number of meta-brackets int the most
				   recenlty compiled RE */
    bool do_folding;		/* fold upper and lower case? */
} COMPEX;

void	search_init _((void));
void	init_compex _((COMPEX*));
void	free_compex _((COMPEX*));
char	*getbracket _((COMPEX*,int));
void	case_fold _((int));
char	*compile _((COMPEX*,char*,int,int)); 
char	*grow_eb _((COMPEX*,char*,char**));
char	*execute _((COMPEX*,char*)); 
bool	advance _((COMPEX*,char*,char*));
bool	backref _((COMPEX*,int,char*)); 
bool	cclass _((char*,int,int));
#endif
