/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2sou.c,v 1.1 84/06/28 00:49:20 timo Exp $ */

/* Sources: maintaining units and values on external files */
#include "b.h"
#include "b0con.h"
#include "b1mem.h" /* shouldn't really */
#include "b1obj.h"
#include "b2env.h"
#include "b2scr.h"
#include "b2err.h"
#include "b2key.h"
#include "b2syn.h"
#include "b2sem.h"
#include "b2fil.h"
#include "b2sou.h"

/************************** UNITS ************************************/

value defunits, aster, global;

Hidden value* unit_defn(fn) value fn; {
	return envassoc(defunits, fn);
}

Visible Procedure def_unit(u, un, ut) value u, un; literal ut; {
	value fn= f_uname(un, ut);
	e_replace(u, &defunits, fn);
	release(fn);
}

Visible value unit_info(un, ut) value un; literal ut; {
	value fn= f_uname(un, ut);
	value *aa= unit_defn(fn);
	if (aa == Pnil) syserr("undefined function");
	release(fn);
	return *aa;
}

Hidden bool is_loaded(un, ut, aa) value un, **aa; literal ut; {
	value fn= f_uname(un, ut); txptr tx0, txstart0;
	*aa= unit_defn(fn);
	if (*aa != Pnil) { release(fn); return Yes; } /*already loaded*/
	release(iname);
	iname= fn;
	ifile= fopen(strval(iname), "r");
	if (ifile == NULL) {
		vs_ifile();
		return No;
	}
	tx0= tx; txstart0= txstart;
	open_stream();
	Eof= Eof0= No;
	ilev(Yes); findceol();
	get_unit(Yes);
	*aa= unit_defn(iname);
	if ((*aa) == Pnil) {
		uname= un; /*utype= ???*/
		parerr("filename and unit name incompatible","");
	}
	close_stream(tx0, txstart0);
	fclose(ifile);
	vs_ifile();
	Eof= Eof0= No;
	return Yes;
}

Visible bool is_unit(un, ut, aa) value un, **aa; literal ut; {
	context c; bool is;
	sv_context(&c);
	cntxt= In_unit;
	is= is_loaded(un, ut, aa);
	set_context(&c);
	return is;
}

#define DISCARD "the unit name is already in use; should the old unit be discarded?"
#define CANT_WRITE "cannot open file for editing; you need write permission in directory"
#define CANT_READ "unable to find file"

Visible bool unit() {
	txptr tx0= tx; value name, fname; literal type; FILE *ofile;
	if (atkw(HOW_TO) || atkw(YIELD) || atkw(TEST)) {
		tx= tx0;
		uheading(aster, &name, &type);
		fname= f_uname(name, type);
		if (unit_defn(fname) != Pnil) {
			if (is_intended(DISCARD)) free_unit(fname);
			else { tx= ceol; release(fname); release(name);
				return Yes;
			}
		}
		if (interactive) {
			ofile= fopen(strval(fname), "w");	
			if (ofile == NULL) error(CANT_WRITE);
			while (Char(tx) != Eotc) putc(Char(tx++), ofile);
			tx--;
			fclose(ofile);
			ed_unit(name, type, fname);
		} else get_unit(No);
		release(name); release(fname);
		return Yes;
	} else return No;
}

#define On_file Vnil

value last_tname= Vnil, last_tfname= Vnil; /*target*/

Visible Procedure special() {
	switch(Char(tx++)) {
		case ':':	ediuni(); break;
		case '=':	editar(); break;
		case '!':	shellcmd(); break;
		default: 	syserr("edit");
	}
}


#define FnSwitch(X) {release(fname); type= X; fname= f_uname(name, X);}

#define MONADIC \
   "the unit name is in use both for a monadic and a dyadic version;\n\
*** do you want to edit the monadic version?"

Hidden Procedure ediuni() {
	value name, fname; literal type;
	Skipsp(tx);
	if (Char(tx) == ':') {
		lst_uhds();
		To_eol(tx);
		return;
	}
	if (Ceol(tx)) {
		if (erruname == Vnil)
			parerr("no current unit name known", "");
		name= copy(erruname);
		type= errutype;
		fname= f_uname(name, type);
	} else if (Cap(Char(tx))) {
		name= keyword(ceol);
		type= FHW;
		fname= f_uname(name, FHW);
	} else if (Letter(Char(tx))) {
		name= tag(); type= FZR;
		fname= f_uname(name, FZR);
		if (!f_exists(fname)) {
			bool is_mon, is_dya;
			FnSwitch(FMN);
			is_mon= f_exists(fname);
			FnSwitch(FDY);
			is_dya= f_exists(fname);
			if (is_mon && (!is_dya || is_intended(MONADIC)))
				FnSwitch(FMN);
		}
	} else parerr("I find nothing editible here", "");
	To_eol(tx);
	if (!f_exists(fname)) pprerr("no such unit in this workspace","");
	ens_filed(fname);
	ed_unit(name, type, fname); release(name); release(fname);
}

Forward bool still_there();
Forward intlet err_line();

/* Edit a unit.
   It is possible that the user messes things up with the w command:
   this is not checked. However it is allowed to rename the unit,
   or delete it completely. If the file is empty, the unit is disposed of.
   Otherwise, uheading is used to work out the name and adicity:
   if these have changed, the new unit is written out to a new file,
   and the original is written back. Thus the original is not lost.
   Inability to find the file at all leads to the main_loop,
   so that nothing is changed. */

Hidden Procedure ed_unit(name, type, fname) value name, fname; literal type; {
	intlet el= err_line(name); value nname, nfname, sname; literal ntype;
	sname= f_save(fname); /*in case the unit gets renamed*/
	f_edit(fname, el);
	if (still_there(fname)) {
		ilev(Yes); findceol();
		uheading(name, &nname, &ntype);
		nfname= f_uname(nname, ntype);
		if (compare(fname, nfname) != 0) { /* unit heading was changed */
			f_rename(fname, nfname); f_rename(sname, fname);
			release(erruname); erruname= copy(nname);
			errutype= ntype;
		} else {
			release(erruname); erruname= copy(name);
			errutype= type;
			f_delete(sname);
		}
		release(nname); release(nfname);
		get_unit(Yes); /* file is still open */
	} else {
		free_unit(fname);
		f_delete(sname);
		release(erruname); erruname= Vnil; errlino= 0;
	}
	release(sname);
	inistreams();
}

Hidden Procedure uheading(oname, nname, ntype) value oname, *nname; literal *ntype; {
	context ic; bool hu= No;
	sv_context(&ic);
	cntxt= In_unit; uname= oname;
	lino= 1;
	if ((hu= atkw(HOW_TO)) || atkw(YIELD) || atkw(TEST)) {
		if (cur_ilev != 0) parerr("unit starts with indentation", "");
		if (hu) {
			uname= keyword(ceol); utype= FHW;
		} else {
			literal adic;
			ytu_heading(&uname, &adic, ceol, No);
			utype= (adic == Zer ? FZR : adic == Mon ? FMN : FDY);
		}
		*nname= uname; /*should really be n=copy(u); release(u);*/
		*ntype= utype;
		set_context(&ic);
	} else parerr("no HOW'TO, YIELD or TEST where expected", "");
}

Hidden bool still_there(fname) value fname; {
	/* Find out if the file exists, and is not empty.
	   Some editors don't allow a file to be edited to empty,
	   but insist it should be at least one empty line.
	   Because it is hard to unget 2 chars, an initial empty line
	   may be disregarded, but this is not harmful. */
	int k;
	ifile= fopen(strval(fname), "r");
	if (ifile == NULL) {
		vs_ifile();
		error(CANT_READ);
	}
	if ((k= getc(ifile)) == EOF || (k == '\n' && (k= getc(ifile)) == EOF)) {
		fclose(ifile);
		f_delete(fname);
		vs_ifile();
		return No;
	}
	ungetc(k, ifile);
	return Yes;
}

Hidden Procedure ens_filed(fname) value fname; {
	value *aa= unit_defn(fname); how *du;
	if (aa != Pnil) {
		du= How_to(*aa);
		if (du->filed == No) {
			txptr ux= du->fux, lux= du->lux;
			FILE *ofile= fopen(strval(fname), "w");
			if (ofile == NULL) error(CANT_WRITE);
			while (ux < lux) {
				char c= *ux++;
				putc(c == Eouc ? '\n' : c, ofile);
			}
			fclose(ofile);
			du->filed= Yes;
		}
	}
}

Hidden intlet err_line(name) value name; {
	intlet el;
	if (errlino == 0 || compare(erruname, name) != 0) return 0;
	el= errlino; errlino= 0;
	return el;
}

Hidden Procedure free_unit(fname) value fname; {
	e_delete(&defunits, fname);
}

Hidden Procedure shellcmd() {
	system(tx);
	To_eol(tx);
}

/************************** VALUES ***************************************/
/* The permanent environment in the old format was kept as a single file */
/* but this caused slow start ups if the file was big.			 */
/* Thus the new version stores each permanent target on a separate file, */
/* that furthermore is only loaded on demand.				 */
/* To achieve this, a directory is kept of the permanent tags and their	 */
/* file names. Care has to be taken that user interrupts occurring in	 */
/* the middle of an update of this directory do the least harm.		 */
/* Having the directory refer to a non-existent file is considered less	 */
/* harmful than leaving a file around that can never be accessed, for	 */
/* instance, so a file is deleted before its directory entry,		 */
/* and so forth.							 */
/*************************************************************************/

value b_perm; /*The table that maps tags to their file names*/

Visible bool is_tloaded(name, aa) value name, **aa; {
	return No; /*for now*/
}

Hidden bool new_tname(name, fname) value name, *fname; {
	value *aa;
	if (in_env(b_perm, name, &aa)) {
		*fname= copy(*aa);
		return No;
	} else {
		*fname= f_tname(name);
		e_replace(*fname, &b_perm, name);
		return Yes;
	}
}

Hidden Procedure editar() {
	value name, fname;
	Skipsp(tx);
	if (Char(tx) == '=') {
		lst_ttgs();
		To_eol(tx);
		return;
	}
	if (Ceol(tx)) {
		if (last_tfname == Vnil)
			parerr("no current target name known", "");
		fname= copy(last_tfname);
		name= copy(last_tname);
	} else if (Letter(Char(tx))) {
		name= tag();
		VOID new_tname(name, &fname);
	} else parerr("I find nothing editible here", "");
	if (!f_exists(fname)) pprerr("no such target in this workspace","");
	ens_tfiled(name, fname);
	ed_target(name, fname); release(fname); release(name);
}

Hidden Procedure lst_ttgs() {
	int k, len;
	len= length(prmnv->tab);
	k_Over_len {
		writ(*key(prmnv->tab, k));
		wri_space();
	}
	newline();
}

Hidden Procedure ed_target(name, fname) value name, fname; {
	/* Edit a target. The value in the target is written to the file,
	   and then removed from the internal permanent environment so that
	   if a syntax error occurs when reading the value back, the value is
	   absent from the internal permanent environment.
	   Thus when editing the file to correct the syntax error, the
	   file doesn't get overwritten.
	   The contents may be completely deleted in which case the target is
	   deleted.
	*/
	value v, p; context c; bool wia;
	f_edit(fname, 0);
	if (still_there(fname)) {
		release(last_tfname); last_tfname= copy(fname);
		release(last_tname); last_tname= copy(name);
		fclose(ifile); /*since still_there leaves it open*/
		sv_context(&c); wia= interactive;
		cntxt= In_value;
		getval(fname, &v);
/*		p= mk_per(v);
*/p=v;		e_replace(p, &prmnv->tab, name);
		set_context(&c); interactive= wia;
		vs_ifile();
		release(p);
/*		release(v);
*/	} else {
		e_delete(&prmnv->tab, name);
		e_delete(&b_perm, name);
		release(last_tfname); release(last_tname);
		last_tfname= Vnil; last_tname= Vnil;
	}
f_delete(fname);
}

Hidden Procedure ens_tfiled(name, fname) value name, fname; {
	value p, *aa;
	if (in_env(prmnv->tab, name, &aa) && !Is_filed(*aa)) {
		putval(fname, *aa, No);
		p= mk_per(Vnil);
		e_replace(p, &prmnv->tab, name);
		release(p);
	}
}

Hidden Procedure getval(nm, v) value nm, *v; {
	char *buf= Nil; int k;
	release(iname);
	iname= copy(nm);
	ifile= fopen(strval(iname), "r");
	if (ifile != NULL) {
		interactive= No;
		alino= 0; xeq= Yes; active_reads= 0; /*CHANGE*/
		buf= getmem((unsigned)(f_size(ifile)+2)*sizeof(char));
		if (buf == Nil) syserr("can't get buffer to read file");
		*(txend= buf)= Eotc; tx= ceol= txend+1;
		while ((k= getc(ifile)) != EOF)
			if (k != '\n') *ceol++= k;
		*ceol= '\n'; alino= 1; *v= expr(ceol);
		fclose(ifile);
		if (buf != Nil) freemem(buf);
	} else error(CANT_READ);
}

Visible Procedure getprmnv() {
	value fn= mk_text(".prmnv");
	cntxt= In_prmnv;
	if (f_exists(fn)) { /* convert from old to new format */
		getval(fn, &prmnv->tab);
		b_perm= mk_elt();
/*		putprmnv();
		f_delete(fn); /*after writing the new one, for safety*/
/* */		release(fn);
	} else {
prmnv->tab= mk_elt();
b_perm= mk_elt();
/*		release(fn);
		fn= mk_text(".b_perm");
		if (f_exists(fn)) {
			getval(fn, &b_perm);
			create_prmnv();
		} else {
			b_perm= mk_elt();
			prmnv->tab= mk_elt();
		}
*/		release(fn);
	}
}

Hidden Procedure putval(nm, v, silently) value nm, v; bool silently; {
	FILE *ofile;
	ofile= fopen(strval(nm), "w");
	if (ofile != NULL) {
		redirect(ofile);
		wri(v, No, No, Yes); newline();
		fclose(ofile);
		redirect(stdout);
	} else if (!silently) error(CANT_WRITE);
}

Visible Procedure putprmnv() {
	bool changed= No; value fn;
	value pt1, pt2; env c;
	int k, len= length(prmnv->tab);

	ignsigs(); /*because files are created before the directory is written*/
	pt1= prmnv->tab; pt2= prmnvtab; c= curnv;
	setprmnv();
	k_Over_len {
		value v= copy(*assoc(prmnv->tab, k));
		if (!Is_filed(v)) {
/*			value t= copy(*key(prmnv->tab, k));
			wri_target(t, v, &changed);
			release(t);
*/}else{e_delete(&prmnv->tab, *key(prmnv->tab, k));
		}
		release(v);
	}
fn= mk_text(".prmnv");
putval(fn, prmnv->tab, Yes);
release(fn);
	if (changed) {
		fn= mk_text(".b_perm");
		putval(fn, b_perm, Yes);
		release(fn);
	}
	prmnv->tab= pt1; prmnvtab= pt2; curnv= c; /* kludgy */
	re_sigs();
}

Hidden Procedure wri_target(t, v, changed) value t, v; bool* changed; {
	value fn, p;
	bool new= new_tname(t, &fn);
	if (new) *changed= Yes;
	putval(fn, v, Yes);
	p= mk_per(v);
	e_replace(p, &prmnv->tab, t); /*after writing file*/
	release(p); release(fn);
}

Hidden Procedure create_prmnv() {
	value p= mk_per(Vnil);
	int k, len= length(b_perm);

	k_Over_len {
		e_replace(copy(p), &prmnv->tab, *key(b_perm, k));
	}
	release(p);
}

Visible Procedure initsou() {
	defunits= mk_elt();
}
