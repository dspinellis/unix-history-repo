/* $Id: rt-ov.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/

bool ov_init _((void));
bool ov_data _((ART_NUM,ART_NUM,bool_int));
void ov_close _((void));

/* Stuff internal to rt-ov.c */

#ifdef DOINIT

static ARTICLE *ov_parse _((char*, ART_NUM));
#ifndef USE_XOVER
static char *ov_name _((char*));
#endif

#endif
