/* $Id: rt-process.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/

int msgid_cmp _((char*,int,HASHDATUM));
ARTICLE *allocate_article _((ART_NUM));
bool valid_article _((ARTICLE*));
ARTICLE *get_article _((char*));
void thread_article _((ARTICLE*));
