#include "head.h"
#include <a.out.h>

struct user u;
int compar();

/* initialize file and procedure tables */
initfp() {
	struct nlist stentry;
	register struct proct *procp;
	register struct filet *filep;
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
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) break;
		class = stentry.n_type & STABMASK;
		switch (class & STABMASK) {
		case N_SO:
		case N_SOL:
			if (filep == badfile) {
				p = sbrk(FILEINCR*sizeof filep[0]);
				q = p + FILEINCR*sizeof filep[0];
				while (p > (char *) procs)
					*--q = *--p;
				badfile += FILEINCR;
				procp = (struct proct *)
				    ((char *) procp + FILEINCR*sizeof filep[0]);
				procs = (struct proct *)
				    ((char *) procs + FILEINCR*sizeof filep[0]);
				badproc = (struct proct *)
				    ((char *)badproc + FILEINCR*sizeof filep[0]);
			}
			filep->faddr = stentry.n_value;
			filep->lineflag = (class == N_SOL);
			filep->stf_offset = soffset;
			p = filep->sfilename;
			for (;;) {
				for (i=0; i<8; i++) *p++ = stentry.n_name[i];
				if (*(p-1) == '\0') break;
				if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry)
					error("Bad N_SO entry (1)");
				if ((stentry.n_type & STABMASK) != (unsigned char) class)
					error("Bad N_SO entry (2)");
				soffset += sizeof stentry;
			}
			filep++;
			break;
			
		case N_TEXT:
			if (stentry.n_name[0] != '_') break;
		case N_FUN:
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
			procp->sfptr = (class==N_FUN) ? filep - 1 : badfile;
			procp->lineno = (class == N_FUN) ? stentry.n_desc : 0;
			procp++;
			break;
		}
		if (stentry.n_type & N_EXT  &&  !extstart) {
			extstart = soffset;
		}
		soffset += sizeof stentry;
	}
	qsort(procs, procp-procs, sizeof procs[0], compar);
	badproc->sfptr = procp->sfptr = badfile;
	badproc->pname[0] = badfile->sfilename[0]=
		procp->pname[0] = filep->sfilename[0] = '\0';

	setcur();
}

/* returns current procedure from state (curfile, fline) */
struct proct
*curproc() {
	register int i;
	register ADDR addr;

/* The 'i' stuff is a kludge */
	for (i = 0; i<10000; i++) {
		addr = getaddr("", fline+i);
		if (addr != -1) break;
		}
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

/* looks up variable matching pat starting at offset in a.out, searching
 * backwards, ignoring nested blocks to beginning to procedure.
 * Returns its offset and symbol table entries decoded in sl_*
 */
 
slookup(pat, offset)
register long offset; char *pat; {
	register int level, i;
	char class; 
	struct nlist stentry;
	if (debug) printf("slookup(%s,%d)\n",pat,offset);
	
	offset += sizeof stentry;
	level = 0;
	blseek(&sbuf, offset, 0);
	
	for (;;) {
		offset -= sizeof stentry;
		if (offset < ststart) break;
		if (bread(&sbuf, &stentry+1, -sizeof stentry) < sizeof stentry) break;
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
		default:
			if (level <= 0  &&  eqpat(pat, stentry.n_name)) {
				if (class == N_LENG) {
					sl_size = stentry.n_value;
					offset -= sizeof stentry;
					bread(&sbuf, &stentry+1, -sizeof stentry);
				}
				else sl_size = 0;
				sl_class = stentry.n_type & STABMASK;
				sl_type = stentry.n_desc;
				sl_addr = stentry.n_value;
				for (i=0; i<8; i++) sl_name[i] = stentry.n_name[i];
				return(offset);
			}
		}
	}
	return(-1);
}

/* 
 * Look up global variable matching pat
 * Return its offset and symbol table entries decoded in sl_*
 */
globallookup(pat, filestart)
char *pat; long filestart; {
	register int offset, i;
	struct nlist stentry;
	
	if (debug) printf("globallookup(%s,%d)\n", pat,filestart);
	blseek(&sbuf, filestart, 0);
	offset = filestart - sizeof stentry;
	do {
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) return(-1);
		offset += sizeof stentry;
	} while ((stentry.n_type & STABMASK) == N_SO);
	for (;;) {
		if ((stentry.n_type & STABMASK) == N_SO) return(-1);
		if ((eqpat(pat, stentry.n_name))) {
			sl_class = stentry.n_type & STABMASK;
			if (sl_class != N_GSYM && sl_class != N_SSYM && 
				sl_class != N_STSYM) goto g1;
			sl_size = 0;
			sl_type = stentry.n_desc;
			sl_addr = stentry.n_value;
			for (i=0; i<8; i++) sl_name[i] = stentry.n_name[i];
			break;
		}
g1:		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry)
			return(-1);
		offset += sizeof stentry;
	}
	bread(&sbuf, &stentry, sizeof stentry);
	if (((stentry.n_type & STABMASK) == N_LENG) && (eqpat(pat, stentry.n_name)))
		sl_size = stentry.n_value;

	if (sl_class == N_GSYM) {
		blseek(&sbuf, extstart, 0);
		for(;;) {
			if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry)
				return(-1);
			if (stentry.n_name[0] != '_') continue;
			if (eqpatr(pat, stentry.n_name+1, 1)) {
				sl_addr = stentry.n_value;
				break;
			}
		}
	}
	return(offset);
}

/* core address to procedure (pointer to proc array) */
struct proct *
adrtoproc(addr) 
ADDR addr; {
	register struct proct *procp;
	for (procp=procs; procp->pname[0]; procp++) {
		if (procp->paddr > addr) break;
	}
	return (procp != procs ? procp-1 : badproc);
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
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) break;
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
 */
setcur() {
	register struct proct *procp;
	
	dot = *(ADDR *) (((ADDR) &u) + PC);
	
	if (dot == 0) {
		printf("No core image\n");
		goto setmain;
	}
	procp = adrtoprocp(dot);
	if ((procp->sfptr) != badfile) {
		finit(adrtofilep(dot)->sfilename);
		ffind(adrtolineno(dot));
		printf("%.8s:", procp->pname);
		fprint();
		return(1);
	}
	if (procp->pname[0] == '_') 
		printf("%.7s: address 0x%x\n", procp->pname+1, dot);
	else
		printf("%.8s: address %d\n", procp->pname, dot);
	
setmain:
	procp = findproc("main");
	if ((procp->pname[0] == 'm') && (procp->sfptr != badfile)) {
		finit(procp->sfptr->sfilename);
		ffind(procp->lineno);
/*
		printf("main() in \"%s\"\n", curfile);
*/
	}
	else printf("main not compiled with debug flag\n");
	return(0);
}

compar(a, b)
struct proct *a, *b; {
	if (a->paddr == b->paddr)
		return(a->pname[0] == '_' ? -1 : 1);
	return(a->paddr < b->paddr ? -1 : 1);
}

/* gets offset of file or procedure named s */
nametooffset(s)
char *s; {
	register struct filet *f;
	register struct proct *p;
	
	if (eqany('.', s)) {
		f = findfile(s);
		return(f->sfilename[0] ? f->stf_offset : -1);
	}
	p = findproc(s);
	return(p->pname[0] ? p->st_offset : -1);
}


/* line number to address, starting at offset in a.out */
/* THIS SHOULD BE FIXED TO KNOW ABOUT #line FILES */
lntoaddr(lineno, offset) 
long offset; {
	struct nlist stentry;
	
	blseek(&sbuf, offset, 0);
	
	do {
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) return(-1);
	} while ((stentry.n_type & STABMASK) == N_SO);
	for (;;) {
		switch(stentry.n_type & STABMASK) {
		case N_SLINE:
			if (stentry.n_desc == lineno) return(stentry.n_value);
			break;
		case N_SO:
			return(-1);
		}
		if (bread(&sbuf, &stentry, sizeof stentry) < sizeof stentry) break;
	}
	return(-1);
}

/* gets address of proc:number */
getaddr(proc,integ) 
char *proc; {
	register long offset;
	register char *s;
	ADDR addr;
	
	s = proc[0] ? proc : curfile;
	offset = nametooffset(s);
	if (debug) printf("getaddr() computed offset %d", offset);
	if (offset == -1) {
		addr = extaddr(proc);
		if (addr != -1) addr += 2;  /* MACHINE DEPENDENT */
		if (debug) printf(" extaddr computed %d\n", addr);
		return(addr);
	}
	if (integ)
		addr = lntoaddr(integ, offset);
	else {
		addr = findproc(proc)->paddr + 2;  /* MACHINE DEPENDENT */
		addr = lntoaddr(adrtolineno(addr)+1, offset);
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
