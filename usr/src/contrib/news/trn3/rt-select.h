/* $Id: rt-select.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/

#define MAX_SEL 64

EXT bool sel_rereading INIT(0);
EXT int sel_mode INIT(0);
EXT int sel_threadmode INIT(0);
#define SM_THREAD	0
#define SM_SUBJECT	1
#define SM_ARTICLE	2
EXT char *sel_mode_string;
EXT int sel_sort INIT(0);
EXT int sel_artsort INIT(4);
EXT int sel_threadsort INIT(0);
#define SS_DATE		0
#define SS_SUBJECT	1
#define SS_AUTHOR	2
#define SS_COUNT	3
#define SS_GROUPS	4
EXT char *sel_sort_string;
EXT int sel_direction INIT(1);
EXT bool sel_exclusive INIT(FALSE);
EXT int sel_mask INIT(1);

EXT bool selected_only INIT(FALSE);
EXT ART_UNREAD selected_count INIT(0);
EXT int selected_subj_cnt INIT(0);
EXT int added_articles INIT(0);

struct sel_item {
    void *ptr;
    int line;
    int sel;
};

EXT char *sel_chars;
EXT struct sel_item sel_items[MAX_SEL];
EXT int sel_item_index;
EXT int sel_item_cnt;
EXT int sel_total_arts;
EXT int sel_prior_arts;
EXT int sel_page_arts;
EXT int sel_page_cnt;
EXT int sel_max_cnt;
EXT int sel_line;
EXT int sel_last_line;
EXT bool sel_at_end;
EXT ARTICLE **sel_page_app;
EXT SUBJECT *sel_page_sp;
EXT ARTICLE **sel_next_app;
EXT SUBJECT *sel_next_sp;
EXT ARTICLE *sel_last_ap;
EXT SUBJECT *sel_last_sp;

char do_selector _((char_int));
#define DS_POS  	0
#define DS_ASK  	1
#define DS_UPDATE	2
#define DS_DISPLAY	3
#define DS_RESTART	4
#define DS_STATUS	5
#define DS_QUIT 	6

#ifdef DOINIT

static void empty_complaint _((void));
static void sel_cleanup _((void));
static int sel_command _((char_int));

#endif
