struct	node {
	struct node *p1;
	struct node *p2;
	char typ;
	char ch;
};

int	freesize;
struct	node *lookf;
struct	node *looks;
struct	node *lookend;
struct	node *lookstart;
struct	node *lookdef;
struct	node *lookret;
struct	node *lookfret;
int	cfail;
int	rfail;
struct	node *freelist;
struct	node *namelist;
int	lc;
struct	node *schar;
