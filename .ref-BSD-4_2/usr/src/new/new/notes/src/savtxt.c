static char *sccsid = "%W%";

/*
 * append a note or response to a text stream
 */

#include "parms.h"
#include "structs.h"

savresp(io, txtfile, respp, roffset)
struct io_f *io;
FILE *txtfile;
struct resp_f *respp;
int roffset;
{
	char *nfname;
	struct auth_f *author;
	struct id_f *unique;
	struct when_f *date;
	struct daddr_f *txtwhere;

	nfname = io->nf;
	author = &respp->r_auth[roffset];
	unique = &respp->r_id[roffset];
	date = &respp->r_when[roffset];
	txtwhere = &respp->r_addr[roffset];
	return(preptxt(io, txtfile, nfname, author, unique, date, txtwhere));
}

savnote(io, txtfile, notep)
struct io_f *io;
FILE *txtfile;
struct note_f *notep;
{
	char *nfname;
	struct auth_f *author;
	struct id_f *unique;
	struct when_f *date;
	struct daddr_f *txtwhere;

	nfname = io->nf;
	author = &notep->n_auth;
	unique = &notep->n_id;
	date = &notep->n_date;
	txtwhere = &notep->n_addr;
	return(preptxt(io, txtfile, nfname, author, unique, date, txtwhere));
}
