#
/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	Bell Telephone Laboratories
 *
 */


#define N_RDONLY 0100000
#define N_EXPORT 0040000
#define N_ENVNAM 0020000
#define N_ENVPOS 0007777

#define N_DEFAULT 0

struct namnod {
	NAMPTR	namlft;
	NAMPTR	namrgt;
	STRING	namid;
	STRING	namval;
	STRING	namenv;
	INT	namflg;
};
