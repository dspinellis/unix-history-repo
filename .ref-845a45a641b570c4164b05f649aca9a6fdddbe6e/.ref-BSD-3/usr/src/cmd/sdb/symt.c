#include "head.h"
#include <a.out.h>
#include <sys/stat.h>

struct user u;
int compar();
char *symfil;

/* initialize file and procedure tables */
initfp() {
	struct nlist stentry;
	register struct proct *procp;
	register struct filet *filep;
	struct stat stbuf;

	long soffset;
	int i;
	char class;
	register char *p, *q;
	
	sbuf.fd = txtmap.ufd;
	soffset = ststart;
	blseek(&sbuf,ststart,0);
	filep = files = badfile = (struct filet *) sbrk(sizeof filep[0]);
	procp = procs = badproc = (struct proct *) sbrk(sizeof procp[0]);
	
	for(;;) {
		if (bread(&sbuf, &stentry, sizeof stentry) <
				sizeof stentry) break;
		class = stentry.n_type & STABMASK;
		switch (class & STABMASK) {
		case N_SO:
		case N_SOL:
			if (filep == badfile) {
				p = sbrk(FILEINCR*sizeof filep[0]);
				if (p < 0) {
					perror("sdb");
					exit(4);
				}
				q = p + FILEINCR*sizeof filep[0];
				while (p > (char *) procs)
					*--q = *--p;
				badfile += FILEINCR;
				procp = (struct proct *)
				    ((char *) procp +
						FILEINCR*sizeof filep[0]);
				procs = (struct proct *)
				    ((char *) procs +
						FILEINCR*sizeof filep[0]);
				badproc = (struct proct *)
				    ((char *)badproc +
						FILEINCR*sizeof filep[0]);
			}
			filep->faddr = stentry.n_value;
			filep->lineflag = (class == N_SOL);
			filep->stf_offset = soffset;
			p = filep->sfilename;
			for (;;) {
				for (i=0; i<8; i++) *p++ = stentry.n_name[i];
				if (*(p-1) == '\0') break;
				if (bread(&sbuf, &stentry, sizeof stentry) 
						< sizeof stentry)
					error("Bad N_SO entry (1)");
				if ((stentry.n_type & STABMASK) !=
						(unsigned char) class)
					error("Bad N_SO entry (2)");
				soffset += sizeof stentry;
			}
			q = filep->sfilename;
			for (p=fp; *q; *p++ = *q++) ;
			*p = 0;
			if (stat(filework, &stbuf) == -1)
				printf("Warning: `%s' not found\n",
					filep->sfilename);
			else if (stbuf.st_mtime > symtime)
				printf("Warning: `%s' newer than `%s'\n",
					filep->sfilename,
					symfil);
			filep++;
			break;
			
		case N_TEXT:
			if (stentry.n_name[0] != '_') break;
		case N_FUN:
		case N_ENTRY:
			if (procp == badproc) {
				if (sbrk(PROCINCR*sizeof procp[0]) < 0) {
					perror("sdb");
					exit(4);
				}
				badproc += PROCINCR;
			}
			for(i=0; i<8; i++)
				procp->pname[i] = stentry.n_name[i];
			procp->paddr = stentry.n_value;
			procp->st_offset = soffset;
			procp->sfptr = (class != N_TEXT) ? filep - 1 : badfile;
			procp->lineno = (class != N_TEXT) ? stentry.n_desc : 0;
			procp->entrypt = (class & STABMASK) == N_ENTRY;
			procp++;
			break;
		}
		if (stentry.n_type & N_EXT  &&  !extstart) {
			extstart = soffset;
		}
		soffset += sizeof stentry;
	}
	qsort(procs, procp-procs, sizeof procs[0], compar);
	badproc->st_offset = soffset;
	badproc->sfptr = procp->sfptr = badfile;
	badproc->pname[0] = badfile->sfilename[0]=
		procp->pname[0] = filep->sfilename[0] = '\0';

	setcur(1);
}

/* returns current procedure from state (curfile, fline) */
struct proct *
curproc() {
	register ADDR addr;

	addr = getaddr("", fline);
	if (addr == -1) return(badproc);
	return(adrtoprocp(addr));

}

/* returns procedure s, uses curproc() if s == NULL */

struct proct *
findproc(s)
char *s; {
	register struct proct *p;
	
	if (s[0] == '\0') return(curproc());
	
	for(p=procs; p->pname[0]; p++)
		if (eqstr(p->pname, s)) return(p);
		
	if (debug) printf("%s(): unknown name\n", s);
	return(badproc);
}

/* returns file s containing filename */
struct filet *
findfile(s)
char *s; {
	register struct filet *f;
	for (f=files; f->sfilename[0]; f++) {
		if (eqstr(f->sfilename, s)) { 
			for( ; f->lineflag; f--) ;
			if (f < files) error("Bad file array");
			return(f);
		}
	}
	return(f);
}

/*
 * slookup():
 * looks up variable matching pat starting at offset in a.out, searching
 * backwards, ignoring nested blocks to beginning to procedure.
 * Returns its offset and symbol table entries decoded in sl_*
 *
 * If comblk == "*" then match both within and outside common blocks,
 * if comblk == ""  then match only outside common blocks,
 *                  else match only within comblk.
 */
 
long
slookup(pat, poffset, stelt)
long poffset; char *pat; {
	slookinit();
	slooknext(pat, poffset, stelt, "*");
}

int clevel, level, fnameflag, comfound, incomm;

slookinit() {
	clevel = level = fnameflag = comfound = incomm = 0;
}

long
slooknext(pat, poffset, stelt, comblk)
long poffset; char *pat, *comblk; {
	register int i;
	register long offset;
	char class, *q;
	struct nlist stentry;
	struct proct *procp, *p;
	
	offset = poffset + sizeof stentry;
	if (debug) printf("slookup(%s,%d)\n",pat,offset);
	blseek(&sbuf, offset, 0);
	
	for (;;) {
		offset -= sizeof stentry;
		if (offset < ststart) break;
		if (bread(&sbuf, &stentry+1, -sizeof stentry) 
			< sizeof stentry) break;
		class = stentry.n_type & STABMASK;
		switch (class & STABMASK) {
		case 0:
			break;
		case N_FUN:
			return(-1);
		case N_RBRAC:
			level++;
			break;
		case N_LBRAC:
			level--;
			break;
		case N_ECOMM:
			for (q = &stentry.n_name[7]; q>=stentry.n_name; q--) {
				if (*q == '_') {
					*q = '\0';
					break;
				}
			}
			if (eqpat(comblk, stentry.n_name))
				comfound = 1;
			incomm = 1;
		case N_ECOML:
			clevel++;
			break;
		case N_BCOMM:
			comfound = incomm = 0;
			clevel--;
			break;
		case N_FNAME:
			if (fnameflag)
				break;
			procp = findproc(stentry.n_name);
			for (p=procs; p->pname[0]; p++) {
				if (p->entrypt == 0 &&
					p->st_offset > procp->st_offset &&
					p->st_offset < offset)
						offset = p->st_offset;
			}
			clevel = level = 0;
			fnameflag++;
			blseek(&sbuf, offset, 0);
			break;
		default:
			if (level <= 0  &&  eqpat(pat, stentry.n_name) &&
				stentry.n_name[0] && class & STABTYPES &&
				(eqstr("*", comblk) ||
				 (comblk[0] == '\0' && incomm == 0) ||
				 comfound) &&
				(stelt == (class == N_SSYM))) {
				if (class == N_LENG) {
					sl_size = stentry.n_value;
					offset -= sizeof stentry;
					bread(&sbuf, &stentry+1,
							-sizeof stentry);
				}
				else sl_size = 0;
				sl_class = stentry.n_type & STABMASK;
				sl_type = stentry.n_desc;
				sl_addr = stentry.n_value;
				for (i=0; i<8; i++) sl_name[i] =
						stentry.n_name[i];
				if (clevel != 0) docomm(offset);
				return(offset - sizeof stentry);
			}
		}
	}
	return(-1);
}

/* 
 * Look up global variable matching pat
 * Return its offset and symbol table entries decoded in sl_*
 */
long
globallookup(pat, filestart, stelt)
char *pat; long filestart; {
	register int offset, i;
	struct nlist stentry;
	int class, clevel;
	
	if (debug) printf("globallookup(%s,%d)\n", pat,filestart);
	blseek(&sbuf, filestart, 0);
	offset = filestart - sizeof stentry;
	clevel = 0;
	do {
		if (bread(&sbuf, &stentry, sizeof stentry) <
				sizeof stentry) return(-1);
		offset += sizeof stentry;
	} while ((stentry.n_type & STABMASK) == N_SO);
	for (;;) {
		class = stentry.n_type & STABMASK;
		switch (class & STABMASK) {
		case N_SO:
			return(-1);
		case N_ECOMM:
			clevel--;
			break;
		case N_BCOMM:
			clevel++;
			break;
		default:
		if (eqpat(pat, stentry.n_name) 
				&& stentry.n_name[0] && class & STABTYPES) {
			sl_class = stentry.n_type & STABMASK;
			if (sl_class != N_GSYM && sl_class != N_SSYM && 
				sl_class != N_STSYM) goto g1;
			if (stelt != (sl_class == N_SSYM)) goto g1;
			sl_size = 0;
			sl_type = stentry.n_desc;
			sl_addr = stentry.n_value;
			for (i=0; i<8; i++) sl_name[i] = stentry.n_name[i];
			if (clevel != 0) docomm(offset);
			goto g2;
		}
		}
g1:		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry)
			return(-1);
		offset += sizeof stentry;
	}
g2:	bread(&sbuf, &stentry, sizeof stentry);
	if (((stentry.n_type & STABMASK) == N_LENG) &&
			(eqpat(sl_name, stentry.n_name)))
		sl_size = stentry.n_value;

	if (sl_class == N_GSYM && (clevel == 0)) {
		blseek(&sbuf, extstart, 0);
		for(;;) {
			if (bread(&sbuf, &stentry, sizeof stentry) 
					< sizeof stentry)
				return(-1);
			if (stentry.n_name[0] != '_') continue;
			if (eqpatr(sl_name, stentry.n_name+1, 1)) {
				sl_addr = stentry.n_value;
				break;
			}
		}
	}
	return(offset + sizeof stentry);
}

/* core address to procedure (pointer to proc array) */
struct proct *
adrtoprocp(addr) 
ADDR addr; {
	register struct proct *procp, *lastproc;
	lastproc = badproc;
	for (procp=procs; procp->pname[0]; procp++) {
		if (procp->paddr > addr) break;
		if (procp->entrypt == 0)
			lastproc = procp;
	}
	return (lastproc);
}
 

/* core address to file (pointer to file array) */
struct filet *
adrtofilep(addr) 
ADDR addr; {
	register struct filet *filep;
	for (filep=files; filep->sfilename[0]; filep++) {
		if (filep->faddr > addr) break;
	}
	return (filep != files ? filep-1 : badfile);
}
 
/* core address to linenumber */
long lastoffset;

adrtolineno(addr) 
ADDR addr; {
	register int lineno;
	long offset; 
	struct nlist stentry;
	
	lineno = lastoffset = -1;
	offset = adrtoproc(addr)->st_offset;
	blseek(&sbuf, offset, 0);
	for (;;) {
		if (bread(&sbuf, &stentry, sizeof stentry) 
				< sizeof stentry) break;
		if (stentry.n_type == N_SLINE) {
			if (stentry.n_value > addr) break;
			lastoffset = offset;
			lineno = stentry.n_desc;
		}
		offset += sizeof stentry;
	}
	return (lineno);
}


/* address to a.out offset */
long
adrtostoffset(addr) 
ADDR addr; {
	adrtolineno(addr);
	return(lastoffset);
}


/*
 * Set (curfile, lineno) from core image.
 * Returns 1 if there is a core image, 0 otherwise.
 *
 * Print the current line iff verbose is set.
 */
setcur(verbose) {
	register struct proct *procp;
	
	dot = *(ADDR *) (((ADDR) &u) + PC);
	
	if (dot == 0) {
		printf("No core image\n");
		goto setmain;
	}
	procp = adrtoprocp(dot);
	if ((procp->sfptr) != badfile) {
		finit(adrtofilep(procp->paddr)->sfilename);
		ffind(adrtolineno(dot));
		if (verbose) {
			printf("%.8s:", procp->pname);
			fprint();
		}
		return(1);
	}
	if (verbose) {
		if (procp->pname[0] == '_') 
			printf("%.7s: address 0x%x\n", procp->pname+1, dot);
		else
			printf("%.8s: address %d\n", procp->pname, dot);
	}
	
setmain:
	procp = findproc("MAIN_");
	if ((procp->pname[0] != 'M') || (procp->sfptr == badfile)) {
		procp = findproc("main");
		if ((procp->pname[0] != 'm') || (procp->sfptr == badfile)) {
			nolines = 1;
			printf("main not compiled with debug flag\n");
			return(0);
		}
	}
	finit(procp->sfptr->sfilename);
	ffind(procp->lineno);
	return(0);
}

compar(a, b)
struct proct *a, *b; {
	if (a->paddr == b->paddr) {
		if (a->pname[0] == '_') return(-1);
		if (b->pname[0] == '_') return(1);
		return(0);
	}
	return(a->paddr < b->paddr ? -1 : 1);
}

/* gets offset of file or procedure named s */
nametooffset(s)
char *s; {
	register struct filet *f;
	register struct proct *p;
	
	if (*s == '\0')
		return(-1);
	if (eqany('.', s)) {
		f = findfile(s);
		return(f->sfilename[0] ? f->stf_offset : -1);
	}
	p = findproc(s);
	return(p->pname[0] ? p->st_offset : -1);
}
/* returns s if its a filename, its file otherwise */
char *
nametofile(s)
char *s; {
	register struct proct *p;
	
	if (eqany('.', s)) {
		return(s);
	}
	p = findproc(s);
	return(adrtofilep(p->paddr)->sfilename);
}


/* line number to address, starting at offset in a.out */
/* assumes that offset is within file */
lntoaddr(lineno, offset, file) 
long offset; char *file; {
	struct nlist stentry;
	register int i, ignore = 0;
	register int bestln=BIGNUM;
	ADDR bestaddr;
	char *p;
	
	blseek(&sbuf, offset, 0);
	
	do {
		if (bread(&sbuf, &stentry, sizeof stentry) <
				sizeof stentry) return(-1);
	} while ((stentry.n_type & STABMASK) == N_SO);
	for (;;) {
		switch(stentry.n_type & STABMASK) {
		case N_SLINE:
			if (!ignore) {
				if (stentry.n_desc == lineno)
					return(stentry.n_value);
				if (stentry.n_desc > lineno &&
					stentry.n_desc < bestln) {
					bestln = stentry.n_desc;
					bestaddr = stentry.n_value;
				}
			}
			break;

		case N_SO:
			goto ret;

		case N_SOL:
			p = file;
			for (;;) {
				for (i=0; i<8; i++) {
					if (*p != stentry.n_name[i]) goto neq;
					if (*p++ == '\0') break;
				}
				if (stentry.n_name[7] == '\0')
					break;
				if (bread(&sbuf, &stentry, sizeof stentry) 
						< sizeof stentry)
					error("Bad N_SO entry (1)");
				if ((stentry.n_type & STABMASK) != 
						(unsigned char) N_SOL)
					error("Bad N_SO entry (2)");
			}
			ignore = 0;
			break;

neq:			ignore++;
			break;
		}
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) 
			break;
	}
ret:	return(bestln == BIGNUM ? -1 : bestaddr);
}

/* gets address of proc:number */
getaddr(proc,integ) 
char *proc; {
	register long offset;
	register char *s, *f;
	ADDR addr;
	
	s = proc[0] ? proc : curfile;
	if (*s == '\0')
		return(-1);
	offset = nametooffset(s);
	f = nametofile(s);
	if (debug) printf("getaddr() computed offset %d", offset);
	if (offset == -1) {
		addr = extaddr(proc);
		if (addr != -1) addr += 2;  /* MACHINE DEPENDENT */
		if (debug) printf(" extaddr computed %d\n", addr);
		return(addr);
	}
	if (integ)
		addr = lntoaddr(integ, offset, s);
	else {
		addr = findproc(proc)->paddr + 2;  /* MACHINE DEPENDENT */
		addr = lntoaddr(adrtolineno(addr)+1, offset, f);
	}
	if (debug) printf(" and addr %d\n", addr);
	if (addr == -1) return(-1);
	return(addr);
}

/* returns address of external */
ADDR
extaddr(name)
char *name; {
	struct nlist stentry;
	blseek(&sbuf, extstart, 0);
	
	for (;;) {
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry)
			return(-1);
		if (stentry.n_name[0] == '_' && 
			    eqpatr(name, stentry.n_name+1, 1)) 
			return(stentry.n_value);
	}
}

/* find enclosing common blocks and fix up addresses */
docomm(offset)
long offset; {
	struct nlist stentry;

	for (;;) {
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) {
			error("Bad common block");
			return;
		}
		sl_class = N_GSYM;
		if ((stentry.n_type & STABMASK) == N_ECOMM) {
			sl_addr += extaddr(stentry.n_name);
			blseek(&sbuf, offset, 0);
			return;
		}
		if ((stentry.n_type & STABMASK) == N_ECOML) {
			sl_addr += stentry.n_value;
			blseek(&sbuf, offset, 0);
			return;
		}
	}
}

/* determine if class is that of a variable */
char pctypes[] = {N_GSYM, N_STSYM, N_LCSYM, N_RSYM, N_SSYM, N_LSYM,
			N_PSYM, 0};
varclass(class)
char class; {
	char *p;

	for (p=pctypes; *p; p++) {
		if (class == *p)
			return(1);
	}
	return(0);
}
