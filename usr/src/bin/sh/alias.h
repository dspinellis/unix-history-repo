#define ALIASINUSE	1

struct alias {
	struct alias *next;
	char *name;
	char *val;
	int flag;
};

struct alias *lookupalias();
