char	xstr[];
#line 1 "trade.c"

#line 1 "./monop.ext"

#line 1 "./monop.h"

#line 1 "/usr/include/stdio.h"




extern	struct	_iobuf {
	int	_cnt;
	char	*_ptr;
	char	*_base;
	int	_bufsiz;
	short	_flag;
	char	_file;
} _iob[20];


























struct _iobuf	*fopen();
struct _iobuf	*fdopen();
struct _iobuf	*freopen();
long	ftell();
char	*fgets();

char	*sprintf();		

#line 2 "./monop.h"

















				



















struct sqr_st {			
	char	*name;			
	char	owner;			
	char	type;			
	char	*desc;			
	int	cost;			
};

typedef struct sqr_st	SQUARE;

struct mon_st {			
	char	*name;			
	char	owner;			
	char	num_in;			
	char	num_own;		
	char	h_cost;			
	char	*not_m;			
	char	*mon_n;			
	SQUARE	*sq[3];			
};

typedef struct mon_st	MON;

struct prp_st {			
	char	morg;			
	char	monop;			
	char	square;			
	char	houses;			
	MON	*mon_desc;		
	int	rent[6];		
};

struct own_st {			
	SQUARE	*sqr;			
	struct own_st	*next;		
};

typedef struct own_st	OWN;

struct plr_st {			
	char	*name;			
	char	num_gojf;		
	char	num_rr;			
	char	num_util;		
	char	loc;			
	char	in_jail;		
	int	money;			
	OWN	*own_list;		
};

struct rr_st {			
	char	morg;			
};

typedef struct plr_st	PLAY;
typedef struct prp_st	PROP;
typedef struct rr_st	RR_S;
typedef struct rr_st	UTIL_S;

int	cc(), chance(), lux_tax(), goto_jail(), inc_tax();
#line 2 "./monop.ext"

#line 1 "./deck.h"





struct dk_st {			
	int	num_cards;		
	int	last_card;		
	char	gojf_used;		
	long	*offsets;		
};

typedef struct dk_st	DECK;
#line 3 "./monop.ext"

extern char	trading, spec, fixing, told_em;

extern char	*yn[], *comlist[], *name_list[], *lucky_mes[];

extern int	num_play, player, num_doub, num_luck, (*func[])();

extern DECK	deck[2];

extern MON	mon[];

extern PLAY	*play, *cur_p;

extern PROP	prop[];

extern RR_S	rr[];

extern SQUARE	board[];

extern UTIL_S	util[];
#line 2 "trade.c"

static struct	trd_st {	
	int	trader;			
	int	cash;			
	int	gojf;			
	OWN	*prop_list;		
};

typedef	struct trd_st	TRADE;

static char	*list[(22	+4	+2	) +2];

static int	used[(22	+4	+2	) ];

static TRADE	trades[2];

trade() {

	register int	tradee, i;

	trading = (1);
	for (i = 0; i < 2; i++) {
		trades[i].cash = 0;
		trades[i].gojf = (0);
		trades[i].prop_list = 0;
	}
over:
	if (num_play == 1) {
		printf((&xstr[4876]));
		return;
	}
	if (num_play > 2) {
		tradee = getinp((&xstr[4919]),
		    name_list);
		if (tradee == num_play)
			return;
		if (tradee == player) {
			printf((&xstr[4960]));
			goto over;
		}
	}
	else
		tradee = 1 - player;
	get_list(0, player);
	get_list(1, tradee);
	if (getyn((&xstr[4992])) == 0)
		summate();
	if (getyn((&xstr[5016])) == 0)
		do_trade();
}




get_list(struct_no, play_no)
int	struct_no, play_no; {

	register int		sn, pn;
	register PLAY	*pp;
	int		numin, prop, num_prp;
	OWN		*op;
	TRADE		*tp;

	for (numin = 0; numin < (22	+4	+2	) ; numin++)
		used[numin] = (0);
	sn = struct_no, pn = play_no;
	pp = &play[pn];
	tp = &trades[sn];
	tp->trader = pn;
	printf((&xstr[5034]), pp->name, pn+1);
	if (pp->own_list) {
		numin = set_list(pp->own_list);
		for (num_prp = numin; num_prp; ) {
			prop = getinp((&xstr[5051]),
			    list);
			if (prop == numin)
				break;
			else if (used[prop])
				printf((&xstr[5089]));
			else {
				num_prp--;
				used[prop] = (1);
				for (op = pp->own_list; prop--; op = op->next)
					continue;
				add_list(pn, &(tp->prop_list), 	(op->sqr - board));
			}
		}
	}
	if (pp->money > 0) {
		printf((&xstr[5121]), pp->money);
		tp->cash = get_int((&xstr[5137]));
	}
	if (pp->num_gojf > 0) {
once_more:
		printf((&xstr[5164]),pp->num_gojf);
		tp->gojf = get_int((&xstr[5205]));
		if (tp->gojf > pp->num_gojf) {
			printf((&xstr[5232]));
			goto once_more;
		}
	}
}



set_list(the_list)
register OWN	*the_list; {

	register int	i;
	register OWN	*op;

	i = 0;
	for (op = the_list; op; op = op->next)
		if (!used[i])
			list[i++] = op->sqr->name;
	list[i++] = (&xstr[1245]);
	list[i--] = 0;
	return i;
}



summate() {

	register char	some;
	register int		i;
	register TRADE	*tp;
	OWN	*op;

	for (i = 0; i < 2; i++) {
		tp = &trades[i];
		some = (0);
		printf((&xstr[5271]), play[tp->trader].name,
			tp->trader+1);
		if (tp->cash > 0)
			printf((&xstr[5294]), tp->cash), some++;
		if (tp->gojf > 0)
			printf((&xstr[5300]), tp->gojf),
			some++;
		if (tp->prop_list) {
			for (op = tp->prop_list; op; op = op->next)
					 (--((&_iob[1]))->_cnt>=0? ((int)(*((&_iob[1]))->_ptr++=(unsigned)('\t'))):_flsbuf((unsigned)('\t'),(&_iob[1]))), printsq(	(op->sqr - board), (1));
			some++;
		}
		if (!some)
			printf((&xstr[5334]));
	}
}



do_trade() {

	move_em(&trades[0], &trades[1]);
	move_em(&trades[1], &trades[0]);
}



move_em(from, to)
TRADE	*from, *to; {

	register PLAY	*pl_fr, *pl_to;
	register OWN		*op;

	pl_fr = &play[from->trader];
	pl_to = &play[to->trader];

	pl_fr->money -= from->cash;
	pl_to->money += from->cash;
	pl_fr->num_gojf -= from->gojf;
	pl_to->num_gojf += from->gojf;
	for (op = from->prop_list; op; op = op->next) {
		add_list(to->trader, &(pl_to->own_list), 	(op->sqr - board));
		op->sqr->owner = to->trader;
		del_list(from->trader, &(pl_fr->own_list), 	(op->sqr - board));
	}
	set_ownlist(to->trader);
}



resign() {

	register int	i, new_own;
	register OWN	*op;
	SQUARE	*sqp;

	if (cur_p->money <= 0) {
		switch (board[cur_p->loc].type) {
		  case 2	:
		  case 1	:
		  case 0	:
			new_own = board[cur_p->loc].owner;
			break;
		  case 6	:
		  case 4	:
		  case 5	:
			new_own = num_play;
			break;
		}
		if (new_own == num_play)
			printf((&xstr[5350]));
		else
			printf((&xstr[5380]), name_list[new_own]);
	}
	else if (num_play == 1) {
		new_own = num_play;
		printf((&xstr[5350]));
	}
	else {
		name_list[num_play] = (&xstr[5404]);
		do {
			new_own = getinp((&xstr[5409]),
			    name_list);
			if (new_own == player)
				printf((&xstr[5440]));
		} while (new_own == player);
		name_list[num_play] = (&xstr[1245]);
	}
	if (getyn((&xstr[5472]), yn) != 0)
		return;
	if (num_play == 1) {
		printf((&xstr[5503]));
		exit(0);
	}
	if (new_own < num_play) {	
		printf((&xstr[5537]));
		trades[0].trader = new_own;
		trades[0].cash = trades[0].gojf = 0;
		trades[0].prop_list = 0;
		trades[1].trader = player;
		trades[1].cash = cur_p->money > 0 ? cur_p->money : 0;
		trades[1].gojf = cur_p->num_gojf;
		trades[1].prop_list = cur_p->own_list;
		do_trade();
	}
	else {				
		printf((&xstr[5558]));
		for (op = cur_p->own_list; op; op = op->next) {
			sqp = op->sqr;
			sqp->owner = -1;
			sqp->desc->morg = (0);
			if (op->type == 0	) {
				isnot_monop(sqp->desc->mon_desc);
				sqp->desc->houses = 0;
			}
		}
		if (cur_p->num_gojf)
			ret_card(cur_p);
	}
	for (i = player; i < num_play; i++) {
		name_list[i] = name_list[i+1];
		if (i + 1 < num_play)
			cpy_st(&play[i], &play[i+1], sizeof (PLAY));
	}
	name_list[num_play--] = 0;
	for (i = 0; i < 40	; i++)
		if (board[i].owner > player)
			--board[i].owner;
	player = --player < 0 ? num_play - 1 : player;
	next_play();
	if (num_play < 2) {
		printf((&xstr[5577]), play[0].name);
		printhold(0);
		printf((&xstr[5597]),
			play[0].money+prop_worth(&play[0]));
		exit(0);
	}
}
