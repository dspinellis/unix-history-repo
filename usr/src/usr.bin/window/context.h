/*
 *	@(#)context.h	3.1 83/11/22
 */

struct context {
	struct context *x_link;		/* nested contexts */
	char x_type;
	union {				/* input info */
		struct {
			char *X_filename;
			FILE *X_fp;
			char X_bol;
			int X_lineno;
			int X_errlineno;
			struct ww *X_errwin;
			char X_baderr;
		} x_f;
		struct {
			char *X_buf;
			char *X_bufp;
		} x_b;
	} x_un;
	int x_token;			/* holding place for token */
	struct value x_val;
	unsigned x_erred :1;		/* parser error flags */
	unsigned x_synerred :1;
	unsigned x_abort :1;
};
#define x_buf		x_un.x_b.X_buf
#define x_bufp		x_un.x_b.X_bufp
#define x_filename	x_un.x_f.X_filename
#define x_fp		x_un.x_f.X_fp
#define x_lineno	x_un.x_f.X_lineno
#define x_errlineno	x_un.x_f.X_errlineno
#define x_bol		x_un.x_f.X_bol
#define x_errwin	x_un.x_f.X_errwin
#define x_baderr	x_un.x_f.X_baderr

#define X_FILE		1
#define X_BUF		2

struct context cx;

struct context *x_alloc();
