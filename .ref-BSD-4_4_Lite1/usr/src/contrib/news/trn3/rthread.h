/* $Id: rthread.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/

EXT ART_NUM article_count INIT(0);
EXT int subject_count INIT(0);
EXT bool output_chase_phrase;
EXT char *references;
EXT bool ov_opened INIT(FALSE);

void thread_init _((void));
void thread_open _((void));
void thread_grow _((void));
void thread_close _((void));

void top_article _((void));
ARTICLE *find_article _((ART_NUM));
ARTICLE *first_art _((SUBJECT*));
ARTICLE *last_art _((SUBJECT*));
ARTICLE *bump_art _((ARTICLE*));
ARTICLE *next_art _((ARTICLE*));
ARTICLE *prev_art _((ARTICLE*));
void inc_art _((bool_int,bool_int));
void dec_art _((bool_int,bool_int));
bool next_art_with_subj _((void));
bool prev_art_with_subj _((void));

void select_article _((ARTICLE*,int));
void select_subject _((SUBJECT*,int));
void select_thread _((ARTICLE*,int));
void select_subthread _((ARTICLE*,int));
void deselect_article _((ARTICLE*));
void deselect_subject _((SUBJECT*));
void deselect_thread _((ARTICLE*));
void deselect_all _((void));
void kill_subject _((SUBJECT*,int));
void kill_thread _((ARTICLE*,int));
void kill_subthread _((ARTICLE*,int));
void unkill_subject _((SUBJECT*));
void unkill_thread _((ARTICLE*));
void unkill_subthread _((ARTICLE*));
void clear_subject _((SUBJECT*));
void clear_thread _((ARTICLE*));
void clear_subthread _((ARTICLE*));
#define KF_UNSELECTED	0
#define KF_ALL		1
#define KF_KILLFILE	2

ARTICLE *subj_art _((SUBJECT*));
void next_subject _((void));
void prev_subject _((void));

bool find_parent _((bool_int));
bool find_leaf _((bool_int));
bool find_prev_sib _((void));
bool find_next_sib _((void));

void sort_subjects _((void));
void count_subjects _((int));
#define CS_NORM        0
#define CS_RESELECT    1
#define CS_UNSELECT    2
#define CS_UNSEL_STORE 3

int subjorder_date _((SUBJECT**, SUBJECT**));
int subjorder_str _((SUBJECT**, SUBJECT**));
int threadorder_date _((SUBJECT**, SUBJECT**));
int threadorder_str _((SUBJECT**, SUBJECT**));

void sort_articles _((void));

int artorder_date _((ARTICLE**, ARTICLE**));
int artorder_str _((ARTICLE**, ARTICLE**));

time_t parsedate _((char*));

/* Stuff local to rthread.c. */

#ifdef DOINIT

static void build_artptrs _((void));

#endif
