/* $Id: rt-util.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/

char *extract_name _((char*));
char *compress_from _((ARTICLE*,int));
char *compress_name _((char*,int));
char *compress_subj _((ARTICLE*,int));
char *get_subject_start _((char*));
#ifndef HAS_STRCASECMP
int strCASEcmp _((char*,char*));
int strnCASEcmp _((char*,char*,int));
#endif

EXT char spin_char INIT(' ');	/* char to put back when we're done spinning */

#define SPIN_OFF	0
#define SPIN_POP	1
#define SPIN_FOREGROUND	2
#define SPIN_BACKGROUND 3

void setspin _((int));
void spin _((int));
