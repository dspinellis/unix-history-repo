/*
 *	a bid from a machine
 */
#define NOBID -1.0
struct bidmsg {
    double	bm_bid;			/* highest is best */
    char	bm_host[HOSTNAMESIZE];
    char	bm_dir[PATHSIZE];	/* the directory to use */
};

/*
 *	the bids
 */
struct bid {
    struct hostdef	*b_host;
    struct bid		*b_next;
    double		b_bid;
};
