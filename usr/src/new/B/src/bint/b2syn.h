/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2syn.h,v 1.4 85/08/22 16:43:04 timo Exp $
*/

/* General parsing routines */

#define Eotc '\0'

#define Char(tx)	(*(tx))
#define Eol(tx)		(Char(tx) == '\n')
#define Ceol(tx)	(Char(tx) == '\\' || Eol(tx))
#define Text(q) 	(tx < q)

#define Space(c)	((c) == ' ' || (c) == '\t')

#define Letter(c)	('a'<=c&&c<='z')
#define Cap(c)		('A'<=c&&c<='Z')
#define Dig(c)		('0'<=c&&c<='9')

/* Procedure skipsp(); */
/* Procedure upto(); */
/* Procedure need(); */
/* Procedure findceol(); */
/* Procedure req(); */
/* Procedure veli(); */

bool keymark();

txptr fcol();
bool nothing(); 
bool ateol();
bool findkw(); 
value keyword();
value tag();
bool find();

extern txptr tx, ceol, first_col;
extern intlet cur_ilev;
intlet ilev();

extern value kwlist;

value cr_text();
bool is_keyword();
bool is_tag();
bool findrel();
extern string textsign;

bool is_expr();

#ifdef NOT_USED
bool colon_sign();
#endif
bool comment_sign();
bool nwl_sign();
bool open_sign();
#ifdef NOT_USED
bool close_sign();
bool comma_sign();
#endif
bool point_sign();
bool apostrophe_sign();
bool quote_sign();
bool conv_sign();
bool curlyopen_sign();
bool curlyclose_sign();
bool sub_sign();
#ifdef NOT_USED
bool bus_sign();
#endif
bool behead_sign();
bool curtl_sign();
bool about_sign();
bool plus_sign();
bool minus_sign();
bool times_sign();
bool over_sign();
bool power_sign();
bool numtor_sign();
bool denomtor_sign();
bool join_sign();
bool reptext_sign();
bool leftadj_sign();
bool center_sign();
bool rightadj_sign();
bool number_sign();
bool less_than_sign();
bool at_most_sign();
bool equals_sign();
bool unequal_sign();
bool at_least_sign();
bool greater_than_sign();

bool dyamon_sign();
bool dya_sign();
bool mon_sign();
bool trim_sign();

bool check_keyword();
bool choose_keyword();
bool delete_keyword();
bool draw_keyword();
bool insert_keyword();
bool put_keyword();
bool read_keyword();
bool remove_keyword();
bool setrandom_keyword();
bool write_keyword();
bool fail_keyword();
bool quit_keyword();
bool return_keyword();
bool report_keyword();
bool succeed_keyword();
bool if_keyword();
bool select_keyword();
bool while_keyword();
bool for_keyword();
bool else_keyword();
#ifdef NOT_USED
bool and_keyword();
bool or_keyword();
#endif
bool not_keyword();
bool some_keyword();
bool each_keyword();
bool no_keyword();
bool how_to_keyword();
bool yield_keyword();
bool test_keyword();
bool share_keyword();
