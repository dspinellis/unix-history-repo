#include "quipu/util.h"
#include "quipu/common.h"
#include "quipu/entry.h"
#include <sys/types.h>
#include "usr.dirent.h"
#include "tailor.h"
#include "sequence.h"
#include "filt.h"
#include "y.tab.h"

void user_tailor(), main_help(), main_bind(), cnnct_quit (), cnnct_bind();
void rd_start(), back_start(), widen(), set_default_type(), list_start();
void rdn2str(), srch_start(), dn2buf(), read_print(), quipu_print();
void quipu_error(), returnmain(), get_listed_object(), scrollbar();
void make_friendly(), goto_addr(), entry2str(), rfc2jnt();

int isleafnode(), issubstr(), indexstring();

struct attrcomp *sort_attrs();

str_seq SortList();
