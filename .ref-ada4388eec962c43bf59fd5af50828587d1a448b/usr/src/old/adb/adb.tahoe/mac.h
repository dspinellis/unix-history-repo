/*	mac.h	1.1	86/02/25	*/

/*
 *	UNIX debugger
 */

#define TYPE	typedef
#define STRUCT	struct
#define UNION	union
#define REG	register

#define BEGIN	{
#define END	}

#define IF	if(
#define THEN	){
#define ELSE	} else {
#define ELIF	} else if (
#define FI	}

#define FOR	for(
#define WHILE	while(
#define DO	){
#define OD	}
#define REP	do{
#define PER	}while(
#define DONE	);
#define LOOP	for(;;){
#define POOL	}

#define ANDF	&&
#define ORF	||

#define TRUE	 (-1)
#define FALSE	0
#define LOBYTE	0377
#define STRIP	0177

#define SP	' '
#define TB	'\t'
#define EOR	'\n'

#define	eqstr(a,b)	(strcmp(a,b)==0)
