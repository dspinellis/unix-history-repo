/* $Id: rt-wumpus.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/

void init_tree _((void));
void entire_tree _((ARTICLE*));
int tree_puts _((char*,ART_LINE,int));
int finish_tree _((ART_LINE));

#ifdef DOINIT

static void find_depth _((ARTICLE*,int));
static void cache_tree _((ARTICLE*,int,char*));
static void display_tree _((ARTICLE*,char*));
static char letter _((ARTICLE*));
static int check_page_line _((void));

#endif
