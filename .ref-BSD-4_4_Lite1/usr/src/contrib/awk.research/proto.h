/****************************************************************
Copyright (C) AT&T 1993
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of AT&T or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

AT&T DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL AT&T OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
****************************************************************/

extern	void	setfname(Cell *);
extern	int	constnode(Node *);
extern	uchar	*strnode(Node *);
extern	Node	*notnull(Node *);
extern	int	yyparse(void);

extern	int	yylex(void);
extern	void	startreg(void);
extern	int	lex_input(void);
extern	void	unputstr(char *);

extern	fa	*makedfa(uchar *, int);
extern	fa	*mkdfa(uchar *, int);
extern	int	makeinit(fa *, int);
extern	void	penter(Node *);
extern	void	freetr(Node *);
extern	int	hexstr(char **);
extern	int	quoted(char **);
extern	uchar	*cclenter(uchar *);
extern	void	overflo(uchar *);
extern	void	cfoll(fa *, Node *);
extern	int	first(Node *);
extern	void	follow(Node *);
extern	int	member(int, uchar *);
extern	int	match(fa *, uchar *);
extern	int	pmatch(fa *, uchar *);
extern	int	nematch(fa *, uchar *);
extern	Node	*reparse(uchar *);
extern	Node	*regexp(void);
extern	Node	*primary(void);
extern	Node	*concat(Node *);
extern	Node	*alt(Node *);
extern	Node	*unary(Node *);
extern	int	relex(void);
extern	int	cgoto(fa *, int, int);
extern	void	freefa(fa *);

extern	int	main(int, uchar **);
extern	int	pgetc(void);
extern	void	init_input_source(void);

extern	Node	*nodealloc(int);
extern	Node	*exptostat(Node *);
extern	Node	*node1(int, Node *);
extern	Node	*node2(int, Node *, Node *);
extern	Node	*node3(int, Node *, Node *, Node *);
extern	Node	*node4(int, Node *, Node *, Node *, Node *);
extern	Node	*stat3(int, Node *, Node *, Node *);
extern	Node	*op2(int, Node *, Node *);
extern	Node	*op1(int, Node *);
extern	Node	*stat1(int, Node *);
extern	Node	*op3(int, Node *, Node *, Node *);
extern	Node	*op4(int, Node *, Node *, Node *, Node *);
extern	Node	*stat2(int, Node *, Node *);
extern	Node	*stat4(int, Node *, Node *, Node *, Node *);
extern	Node	*valtonode(Cell *, int);
extern	Node	*rectonode(void);
extern	Node	*makearr(Node *);
extern	Node	*pa2stat(Node *, Node *, Node *);
extern	Node	*linkum(Node *, Node *);
extern	void	defn(Cell *, Node *, Node *);
extern	int	isarg(uchar *);
extern	uchar	*tokname(int);
extern	Cell *(*proctab[])(Node **, int);

extern	void	syminit(void);
extern	void	arginit(int, uchar **);
extern	void	envinit(uchar **);
extern	Array	*makesymtab(int);
extern	void	freesymtab(Cell *);
extern	void	freeelem(Cell *, uchar *);
extern	Cell	*setsymtab(uchar *, uchar *, double, unsigned int, Array *);
extern	int	hash(uchar *, int);
extern	void	rehash(Array *);
extern	Cell	*lookup(uchar *, Array *);
extern	double	setfval(Cell *, double);
extern	void	funnyvar(Cell *, char *);
extern	uchar	*setsval(Cell *, uchar *);
extern	double	r_getfval(Cell *);
extern	uchar	*r_getsval(Cell *);
extern	uchar	*tostring(uchar *);
extern	uchar	*qstring(uchar *, int);

extern	void	recinit(unsigned int);
extern	void	initgetrec(void);
extern	int	getrec(uchar *);
extern	int	readrec(uchar *buf, int bufsize, FILE *inf);
extern	uchar	*getargv(int);
extern	void	setclvar(uchar *);
extern	void	fldbld(void);
extern	void	cleanfld(int, int);
extern	void	newfld(int);
extern	int	refldbld(uchar *, uchar *);
extern	void	recbld(void);
extern	Cell	*fieldadr(int);
extern	void	yyerror(char *);
extern	void	fpecatch(int);
extern	void	bracecheck(void);
extern	void	bcheck2(int, int, int);
extern	void	error(int, char *);
extern	void	eprint(void);
extern	void	bclass(int);
extern	double	errcheck(double, uchar *);
extern	int	isclvar(uchar *);
extern	int	is_a_number(uchar *);

extern	void	run(Node *);
extern	Cell	*r_execute(Node *);
extern	Cell	*program(Node **, int);
extern	Cell	*call(Node **, int);
extern	Cell	*copycell(Cell *);
extern	Cell	*arg(Node **, int);
extern	Cell	*jump(Node **, int);
extern	Cell	*getline(Node **, int);
extern	Cell	*getnf(Node **, int);
extern	Cell	*array(Node **, int);
extern	Cell	*adelete(Node **, int);
extern	Cell	*intest(Node **, int);
extern	Cell	*matchop(Node **, int);
extern	Cell	*boolop(Node **, int);
extern	Cell	*relop(Node **, int);
extern	void	tfree(Cell *);
extern	Cell	*gettemp(void);
extern	Cell	*field(Node **, int);
extern	Cell	*indirect(Node **, int);
extern	Cell	*substr(Node **, int);
extern	Cell	*sindex(Node **, int);
extern	int	format(uchar *, int, uchar *, Node *);
extern	Cell	*asprintf(Node **, int);
extern	Cell	*aprintf(Node **, int);
extern	Cell	*arith(Node **, int);
extern	double	ipow(double, int);
extern	Cell	*incrdecr(Node **, int);
extern	Cell	*assign(Node **, int);
extern	Cell	*cat(Node **, int);
extern	Cell	*pastat(Node **, int);
extern	Cell	*dopa2(Node **, int);
extern	Cell	*split(Node **, int);
extern	Cell	*condexpr(Node **, int);
extern	Cell	*ifstat(Node **, int);
extern	Cell	*whilestat(Node **, int);
extern	Cell	*dostat(Node **, int);
extern	Cell	*forstat(Node **, int);
extern	Cell	*instat(Node **, int);
extern	Cell	*bltin(Node **, int);
extern	Cell	*printstat(Node **, int);
extern	Cell	*nullproc(Node **, int);
extern	FILE	*redirect(int, Node *);
extern	FILE	*openfile(int, uchar *);
extern	uchar	*filename(FILE *);
extern	Cell	*closefile(Node **, int);
extern	void	closeall(void);
extern	Cell	*sub(Node **, int);
extern	Cell	*gsub(Node **, int);

extern	FILE	*popen(const char *, const char *);
extern	int	pclose(FILE *);
