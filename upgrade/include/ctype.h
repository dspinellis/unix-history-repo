
#define	_U	01
#define	_L	02
#define	_A	03
#define	_N	04
#define	_S	010

extern	char	_ctype[];

#define	isalpha(c)	(_ctype[c]&_A)
#define	isupper(c)	(_ctype[c]&_U)
#define	islower(c)	(_ctype[c]&_L)
#define	isdigit(c)	(_ctype[c]&_N)
#define	isspace(c)	(_ctype[c]&_S)
#define	toupper(c)	((c)-'a'+'A')
#define	tolower(c)	((c)-'A'+'a')
