/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3sou.c,v 1.4 85/08/22 16:59:08 timo Exp $
*/

/* Sources: maintaining units and values on external files */

#include "b.h"
#include "b0con.h"
#include "b0fea.h"
#include "b0fil.h"
#include "b1mem.h"
#include "b1obj.h"
#include "b2syn.h"
#include "b2par.h"
#include "b2nod.h"
#include "b3env.h"
#include "b3scr.h"
#include "b3err.h"
#include "b3sem.h"
#include "b3fil.h"
#include "b3sou.h"
#include "b3int.h"

/************************** UNITS ************************************/

Hidden value b_perm; /* The table that maps tags to their file names */
Hidden value b_units; /* The table that maps tags to their internal repr. */

Hidden bool
u_exists(pname, aa)
	value pname, **aa;
{
	return in_env(b_units, pname, aa);
}

Visible Procedure
def_unit(pname, u)
	value pname, u;
{
	e_replace(u, &b_units, pname);
}

Hidden Procedure
free_unit(pname)
	value pname;
{
	e_delete(&b_units, pname);
}

Hidden Procedure
del_units()
{
	int len= length(b_units), k; how *u;
	for (k= len-1; k >= 0; --k) {
		/* Reverse loop so deletions don't affect the numbering! */
		u= How_to(*assoc(b_units, k));
		if (!u->unparsed) free_unit(*key(b_units, k));
		/*Therefore standard B functions must be entered as unparsed*/
	}
}

Visible Procedure
rem_unit(u)
	parsetree u;
{
	value pname= get_pname(u);
	free_unit(pname);
	release(pname);
}

/********************************************************************** */

Visible Procedure
p_name_type(pname, name, type)
	value pname, *name; literal *type;
{
	*name= behead(pname, MkSmallInt(2));
	switch (strval(pname)[0]) {
	case '0': *type= Zer; break;
	case '1': *type= Mon; break;
	case '2': *type= Dya; break;
	case '3': *type= How; break;
	case '4': *type= Tar; break;
	default: syserr(MESS(4000, "p_name_type"));
		/* NOTREACHED */
	}
}

Visible value
permkey(name, type)
	value name; literal type;
{
	value v, w; string t;
	switch (type) {
	case Zer: t= "0"; break;
	case Mon: t= "1"; break;
	case Dya: t= "2"; break;
	case How: t= "3"; break;
	case Tar: t= "4"; break;
	default: syserr(MESS(4001, "wrong permkey"));
	}
	w= mk_text(t);
	v= concat(w, name); release(w);
	return v;
}

Visible bool
p_exists(pname, aa)
	value pname, **aa;
{
	return in_env(b_perm, pname, aa);
}

Visible value file_names;

Hidden Procedure
def_perm(pname, f)
	value pname, f;
{
	e_replace(f, &b_perm, pname);
	if (!in(f, file_names)) insert(f, &file_names);
}

Hidden Procedure
free_perm(pname)
	value pname;
{
	value *aa;
	if (p_exists(pname, &aa)) {
		remove(*aa, &file_names);
		f_delete(*aa);
		e_delete(&b_perm, pname);
	}
}

Hidden value
get_fname(pname)
	value pname;
{
	value *aa;
	if (p_exists(pname, &aa)) return copy(*aa);
	else {
		value fname, name; literal type;
		p_name_type(pname, &name, &type);
		fname= new_fname(name, type);
		def_perm(pname, fname);
		release(name);
		return fname;
	}
}

Hidden bool
p_version(name, type, pname)
	value name, *pname; literal type;
{
	value *aa;
	*pname= permkey(name, type);
	if (p_exists(*pname, &aa)) return Yes;
	release(*pname); *pname= Vnil;
	return No;
}

Hidden bool
how_unit(pname)
	value pname;
{
	value name; literal type;
	p_name_type(pname, &name, &type);
	release(name);
	return type == How;
}

Hidden bool
zermon_units(pname, other_pname)
	value pname, *other_pname;
{
	value name; literal type; bool is;
	p_name_type(pname, &name, &type);
	is= (type == Zer && p_version(name, Mon, other_pname)) ||
	    (type == Mon && p_version(name, Zer, other_pname));
	release(name);
	return is;
}

/***********************************************************************/

Hidden bool
is_loaded(pname, aa)
	value pname, **aa;
{
	value u= Vnil, npname= Vnil, get_unit();
	if (u_exists(pname, aa)) return Yes; /* already loaded */
	if (!p_exists(pname, aa)) return No;
	ifile= fopen(strval(**aa), "r");
	if (ifile == NULL) {
		vs_ifile();
		return No;
	}
	Eof= No;
	first_ilev();
	u= get_unit(&npname, Yes);
	if (still_ok) def_unit(npname, u);
	fclose(ifile);
	vs_ifile();
	Eof= No;
	if (still_ok && !u_exists(pname, aa)) {
		value name; literal type;
		p_name_type(npname, &name, &type);
		release(uname); uname= copy(pname);
		curline= How_to(u)->unit; curlino= one;
		error2(MESS(4002, "filename and unit name incompatible for "), name);
		release(name);
	}
	release(u); release(npname);
	return still_ok;
}

/* Does the unit exist without faults? */

Visible bool
is_unit(name, type, aa)
	value name, **aa; literal type;
{
	value pname;
	context c; bool is;
	sv_context(&c);
	cntxt= In_unit;
	pname= permkey(name, type);
	is= is_loaded(pname, aa);
	release(pname);
	set_context(&c);
	return is;
}

/***********************************************************************/

Hidden char DISCARD[]= "the unit name is already in use;\n\
*** should the old unit be discarded?";

#define CANT_WRITE \
	MESS(4003, "cannot create file; need write permission in directory")

#define CANT_READ MESS(4004, "unable to find file")
#define MON_VERSION MESS(4005, " is already a monadic function/predicate")
#define ZER_VERSION MESS(4006, " is already a zeroadic function/predicate")

Hidden Procedure
u_name_type(v, name, type)
	parsetree v; value *name; literal *type;
{
	switch (Nodetype(v)) {
		case HOW_TO:	*name= copy(*Branch(v, UNIT_NAME));
				*type= How;
				break;
		case YIELD:
		case TEST:	*name= copy(*Branch(v, UNIT_NAME));
				switch (intval(*Branch(v, FPR_ADICITY))) {
					case 0: *type= Zer; break;
					case 1: *type= Mon; break;
					case 2: *type= Dya; break;
					default: syserr(MESS(4007, "wrong adicity"));
				}
				break;
		default:	syserr(MESS(4008, "wrong nodetype of unit"));
	}
}

Hidden value
get_unit(pname, filed)
	value *pname; bool filed;
{
	value name; literal type;
	parsetree u= unit(No);
	if (u == NilTree) return Vnil;
	u_name_type(u, &name, &type);
	*pname= permkey(name, type);
	release(name);
	switch (Nodetype(u)) {
		case HOW_TO:	return mk_how(u, filed);
		case YIELD:	return mk_fun(type, Use, u, filed);
		case TEST:	return mk_prd(type, Use, u, filed);
		default:	syserr(MESS(4009, "wrong nodetype in 'get_unit'"));
	}
	/* NOTREACHED */
}

Visible value
get_pname(v)
	parsetree v;
{
	value pname, name; literal type;
	u_name_type(v, &name, &type);
	pname= permkey(name, type);
	release(name);
	return pname;
}

Hidden Procedure
get_heading(h, pname)
	parsetree *h; value *pname;
{
	*h= unit(Yes);
	*pname= still_ok ? get_pname(*h) : Vnil;
}

/* Create a unit via the editor or from the input stream */

Visible Procedure
create_unit()
{
	value pname= Vnil, *aa; parsetree heading= NilTree;
	if (!interactive) {
		value v= get_unit(&pname, No);
		if (still_ok) def_unit(pname, v);
		release(v); release(pname);
		return;
	}
	get_heading(&heading, &pname);
	if (still_ok) {
		value v;
		if (p_exists(pname, &aa)) {
			if (is_intended(DISCARD)) {
				free_unit(pname);
				free_perm(pname);
			} else {
				tx= ceol;
				release(pname);
				release(heading);
				return;
			}
		} else if (zermon_units(pname, &v)) {
			value name; literal type;
			p_name_type(pname, &name, &type);
			curline= heading; curlino= one;
			error3(0, name, type == Zer ? MON_VERSION
						     : ZER_VERSION);
			release(name); release(v);
		}
	}
	if (still_ok) {
		value fname= get_fname(pname);
		FILE *ofile= fopen(strval(fname), "w");
		if (ofile == NULL) error(CANT_WRITE);
		else {
			txptr tp= fcol();
			do { fputc(Char(tp), ofile); }
			while (Char(tp++) != '\n');
			f_close(ofile);
			ed_unit(pname, fname);
		}
		release(fname);
	}
	release(pname); release(heading);
}


/***********************************************************************/

/* Edit a unit. The name of the unit is either given, or is defaulted
   to the last unit edited or the last unit that gave an error, whichever
   was most recent.
   It is possible for the user to mess things up with the w command, for
   instance, but this is not checked. It is allowed to rename the unit though,
   or delete it completely. If the file is empty, the unit is disposed of.
   Otherwise, the name and adicity are determined and if these have changed,
   the new unit is written out to a new file, and the original written back.
   Thus the original is not lost.

   Renaming, deleting, or changing the adicity of a test or yield
   unfortunately requires all other units to be thrown away internally
   (by del_units), since the unit parse trees may be wrong. For instance,
   consider the effect on the following of making a formerly monadic
   function f, into a zeroadic function:
	WRITE f root 2
*/

Hidden char ZEROADIC[]=
   "the unit name is in use both for a zeroadic and a dyadic version;\n\
*** do you want to edit the zeroadic version?";

Hidden char MONADIC[]=
   "the unit name is in use both for a monadic and a dyadic version;\n\
*** do you want to edit the monadic version?";

Visible Procedure
edit_unit()
{
	value name= Vnil, pname= Vnil, v= Vnil; bool ens_filed();
	value fname;
	if (Ceol(tx)) {
		if (erruname == Vnil) parerr(MESS(4010, "no current unit"));
		else pname= copy(erruname);
	} else if (is_keyword(&name))
		pname= permkey(name, How);
	 else if (is_tag(&name)) {
		if (p_version(name, Zer, &pname)) {
			if (p_version(name, Dya, &v) && !is_intended(ZEROADIC)) {
				release(pname); pname= copy(v);
			}
		} else if (p_version(name, Mon, &pname)) {
			if (p_version(name, Dya, &v) && !is_intended(MONADIC)) {
				release(pname); pname= copy(v);
			}
		} else {
			pname= permkey(name, Dya);
		}
	} else {
		parerr(MESS(4011, "I find nothing editible here"));
	}
	if (still_ok && ens_filed(pname, &fname)) {
		ed_unit(pname, fname);
		release(fname);
	}
	release(name); release(pname); release(v);
}

Hidden char NO_U_WRITE[]=
   "you have no write permission in this workspace: you may not change the unit\n\
*** do you still want to display the unit?";

Hidden char ZER_MON[]=
   "the unit name is already in use for a zeroadic function or predicate;\n\
*** should that unit be discarded?\n\
*** (if not you have to change the monadic unit name)";

Hidden char MON_ZER[]=
   "the unit name is already in use for a monadic function or predicate;\n\
*** should that unit be discarded?\n\
*** (if not you have to change the zeroadic unit name)";

Hidden Procedure
ed_unit(pname, fname)
	value pname, fname;
{
	value sname= Vnil, npname= Vnil, nfname= Vnil;
	value u, *aa, v= Vnil, v_free= Vnil;
	intlet err_line();
	bool new_def= Yes, same_name= No, still_there(), ed_again= No;

	if (!ws_writable() && !is_intended(NO_U_WRITE)) return;
	sname= f_save(fname); /* in case the unit gets renamed */
	if (sname == Vnil) {
		error(MESS(4012, "can't save to temporary file"));
		return;
	}
	release(uname); uname= copy(pname);
#ifndef INTEGRATION
	f_edit(fname, err_line(pname));
#else
	f_edit(fname, err_line(pname), unit_prompt);
#endif
	if (!still_there(fname)) {
		free_unit(pname);
		if (!how_unit(pname)) del_units();
		release(erruname); erruname= Vnil; errlino= 0;
		free_perm(pname);
		f_delete(sname);
		release(sname);
		return;
	}
	first_ilev();
	u= get_unit(&npname, Yes);
	fclose(ifile); vs_ifile(); Eof= No;
	if (u == Vnil || npname == Vnil)
		new_def= No;
	else if (same_name= compare(pname, npname) == 0)
		new_def= p_exists(pname, &aa);
	else if (p_exists(npname, &aa))
		new_def= is_intended(DISCARD);
	else if (zermon_units(npname, &v)) {
		value name; literal type;
		p_name_type(npname, &name, &type);
		if (new_def= is_intended(type == Zer ? MON_ZER : ZER_MON)) {
			free_unit(v);
			v_free= copy(v); /* YIELD f => YIELD f x */
		} else {
			nfname= new_fname(name, type);
			f_rename(fname, nfname);
			ed_again= Yes;
		}
		release(name);
	}
	if (new_def) {
		if (!how_unit(npname)) del_units();
		if (still_ok) def_unit(npname, u);
		else free_unit(npname);
		if (!same_name) {
			nfname= get_fname(npname);
			f_rename(fname, nfname);
			if (v_free) free_perm(v_free);
		}
		release(erruname); erruname= copy(npname);
	}
	if (!same_name) f_rename(sname, fname);
	else f_delete(sname);
	if (!p_exists(pname, &aa)) f_delete(fname);
	if (ed_again) ed_unit(npname, nfname);
	release(npname); release(u); release(sname); release(nfname);
	release(v); release(v_free);
}

/* Find out if the file exists, and is not empty. Some wretched editors
   for some reason don't allow a file to be edited to empty, but insist it
   should be at least one empty line. Thus an initial empty line may be
   disregarded, but this is not harmful. */

Hidden bool still_there(fname) value fname; {
	int k;
	ifile= fopen(strval(fname), "r");
	if (ifile == NULL) {
		vs_ifile();
		/* error(CANT_READ); */
		return No;
	} else {
		if ((k= getc(ifile)) == EOF || (k == '\n' && (k= getc(ifile)) == EOF)) {
			fclose(ifile);
			f_delete(fname);
			vs_ifile();
			return No;
		}
		ungetc(k, ifile);
		return Yes;
	}
}

/* Ensure the unit is filed. If the unit was read non-interactively (eg passed
   as a parameter to b), it is only held in store.
   Editing it puts it into a file. This is the safest way to copy a unit from
   one workspace to another.
*/

Hidden bool
ens_filed(pname, fname)
	value pname, *fname;
{
	value *aa;
	if (p_exists(pname, &aa)) {
		*fname= copy(*aa);
		return Yes;
	} else if (!u_exists(pname, &aa) || How_to(*aa)->unit == NilTree) {
		pprerr(MESS(4013, "no such unit in this workspace"));
		return No;
	} else {
		how *du= How_to(*aa); FILE *ofile;
		if (du->filed == Yes) {
			syserr(MESS(4014, "ens_filed()"));
			return No;
		}
		*fname= get_fname(pname);
		ofile= fopen(strval(*fname), "w");
		if (!ofile) {
			error(CANT_WRITE);
			release(*fname);
			return No;
		} else {
			display(ofile, du->unit, No);
			f_close(ofile);
			du->filed= Yes;
			return Yes;
		}
	}
}

Hidden intlet
err_line(pname)
	value pname;
{
	if (errlino == 0 || erruname == Vnil || compare(erruname, pname) != 0)
		return 0;
	else {
		intlet el= errlino;
		errlino= 0;
		return el;
	}
}

/************************** VALUES ***************************************/
/* The permanent environment in the old format was kept as a single file */
/* but this caused slow start ups if the file was big.			 */
/* Thus the new version stores each permanent target on a separate file, */
/* that furthermore is only loaded on demand.				 */
/* To achieve this, a directory is kept of the permanent tags and their  */
/* file names. Care has to be taken that disaster occurring in		 */
/* the middle of an update of this directory does the least harm.	 */
/* Having the directory refer to a non-existent file is considered less  */
/* harmful than leaving a file around that can never be accessed, for	 */
/* instance, so a file is deleted before its directory entry,		 */
/* and so forth.							 */
/*************************************************************************/

Hidden bool
t_exists(name, aa)
	value name, **aa;
{
	return in_env(prmnv->tab, name, aa);
}

Hidden Procedure
def_target(name, t)
	value name, t;
{
	e_replace(t, &prmnv->tab, name);
}

Hidden Procedure
free_target(name)
	value name;
{
	e_delete(&prmnv->tab, name);
}

Hidden Procedure
tarfiled(name, v)
	value name, v;
{
	value p= mk_per(v);
	def_target(name, p);
	release(p);
}

Visible value
tarvalue(name, v)
	value name, v;
{
	value getval();
	if (Is_filed(v)) {
		per *p= Perm(v);
		if (p->val == Vnil) {
			value *aa, pname= permkey(name, Tar);
			if (!p_exists(pname, &aa))
				syserr(MESS(4015, "tarvalue"));
			release(pname);
			p->val= getval(*aa, In_tarval);
		}
		return p->val;
	}
	return v;
}

Hidden value last_tname= Vnil; /*last edited target */

Visible Procedure
edit_target()
 {
	value name= Vnil; bool ens_tfiled();
	value fname;
	if (Ceol(tx)) {
		if (last_tname == Vnil)
			parerr(MESS(4016, "no current target"));
		else
			name= copy(last_tname);
	} else if (!is_tag(&name))
		parerr(MESS(4017, "I find nothing editible here"));
	if (still_ok && ens_tfiled(name, &fname)) {
		ed_target(name, fname);
		release(fname);
	}
	release(name);
}

Hidden char NO_T_WRITE[]=
   "you have no write permission in this workspace: you may not change the target\n\
*** do you still want to display the target?";

Hidden Procedure
ed_target(name, fname)
	value name, fname;
{
	/* Edit a target. The value in the target is written to the file,
	   and then removed from the internal permanent environment so that
	   if a syntax error occurs when reading the value back, the value is
	   absent from the internal permanent environment.
	   Thus when editing the file to correct the syntax error, the
	   file doesn't get overwritten.
	   The contents may be completely deleted in which case the target is
	   deleted.
	*/
	value v, getval();
	if (!ws_writable() && !is_intended(NO_T_WRITE)) return;
#ifndef INTEGRATION
	f_edit(fname, 0);
#else
	f_edit(fname, 0, tar_prompt);
#endif
	if (!still_there(fname)) {
		value pname= permkey(name, Tar);
		free_target(name);
		free_perm(pname);
		release(pname);
		release(last_tname); last_tname= Vnil;
		return;
	}
	release(last_tname); last_tname= copy(name);
	fclose(ifile); /*since still_there leaves it open*/
	v= getval(fname, In_edval);
	if (still_ok) def_target(name, v);
	release(v);
}

Hidden bool
ens_tfiled(name, fname)
	value name, *fname;
{
	value *aa;
	if (!t_exists(name, &aa)) {
		pprerr(MESS(4018, "no such target in this workspace"));
		return No;
	} else {
		value pname= permkey(name, Tar);
		*fname= get_fname(pname);
		if (!Is_filed(*aa)) {
			putval(*fname, *aa, No);
			tarfiled(name, *aa);
		}
		release(pname);
		return Yes;
	}
}

/***************************** Values on files ****************************/

Hidden value
getval(fname, ct)
	value fname;
	literal ct; /* context */
{
	char *buf= Nil; int k; parsetree e, code; value v= Vnil;
	ifile= fopen(strval(fname), "r");
	if (ifile) {
		txptr fcol_save= first_col, tx_save= tx; context c;
		sv_context(&c);
		cntxt= ct;
		buf= getmem((unsigned)(f_size(ifile)+2)*sizeof(char));
		if (buf == Nil)
			syserr(MESS(4019, "can't get buffer to read file"));
		first_col= tx= ceol= buf;
		while ((k= getc(ifile)) != EOF)
			if (k != '\n') *ceol++= k;
		*ceol= '\n';
		fclose(ifile); vs_ifile();
		e= expr(ceol);
		if (still_ok) fix_nodes(&e, &code);
		curline=e; curlino= one;
		v= evalthread(code); curline= Vnil;
		release(e);
		if (buf != Nil) freemem((ptr) buf);
		set_context(&c);
		first_col= fcol_save; tx= tx_save;
	} else {
		error(CANT_READ);
		vs_ifile();
	}
	return v;
}

Visible Procedure
getprmnv()
{
	intlet k, len; value name, fname; literal type;
	if (f_exists(BPERMFILE)) {
		value fn;
		fn= mk_text(BPERMFILE);
		b_perm= getval(fn, In_prmnv);
		release(fn);
		if (!still_ok) exit(1);
		len= length(b_perm);
		k_Over_len {
			p_name_type(*key(b_perm, k), &name, &type);
			if (type == Tar) tarfiled(name, Vnil);
			fname= copy(*assoc(b_perm, k));
			insert(fname, &file_names);
			release(fname); release(name);
		}
	} else
		b_perm= mk_elt();

#ifdef CONVERSION
	if (f_exists(PRMNVFILE)) { /* convert from old to new format */
		value tab, v, pname, new_fname();
		value fn= mk_text(PRMNVFILE), save= mk_text(SAVEPRMNVFILE);
		tab= getval(fn, In_prmnv);
		if (!still_ok) exit(1);
		len= length(tab);
		k_Over_len {
			name= copy(*key(tab, k));
			v= copy(*assoc(tab, k));
			def_target(name, v);
			pname= permkey(name, Tar);
			fname= get_fname(pname);
			putval(fname, v, Yes);
			tarfiled(name, v);
			release(name); release(v); release(fname);
			release(pname);
		}
		f_rename(fn, save);
		if (len > 0)
			printf("*** [Old permanent environment converted]\n");
		release(tab); release(fn); release(save);
	}
#endif CONVERSION
}

Hidden Procedure
putval(fname, v, silently)
	value fname, v; bool silently;
{
	FILE *ofile; value fn= mk_text(tempfile); bool was_ok= still_ok;
	ofile= fopen(strval(fn), "w");
	if (ofile != NULL) {
		redirect(ofile);
		still_ok= Yes;
		wri(v, No, No, Yes); newline();
		f_close(ofile);
		redirect(stdout);
		if (still_ok) f_rename(fn, fname);
	} else if (!silently) error(CANT_WRITE);
	still_ok= was_ok;
	release(fn);
}

Visible Procedure
putprmnv()
{
	static bool active;
	value v, name, fname, fn, *aa, pname; literal type;
	int k, len;
	if (active) return;
	active= Yes;
	len= length(b_perm);
	for (k= len-1; k>=0; --k) {
		p_name_type(*key(b_perm, k), &name, &type);
		if (type == Tar && !t_exists(name, &aa))
			free_perm(*key(b_perm, k));
		release(name);
	}
	len= length(prmnv->tab);
	k_Over_len {
		v= copy(*assoc(prmnv->tab, k));
		if (!Is_filed(v)) {
			name= copy(*key(prmnv->tab, k));
			pname= permkey(name, Tar);
			fname= get_fname(pname);
			putval(fname, v, Yes);
			tarfiled(name, v);
			release(name); release(fname); release(pname);
		}
		release(v);
	}
	fn= mk_text(BPERMFILE);
	putval(fn, b_perm, Yes);
	/* Remove the file if the permanent environment is empty */
	if (length(b_perm) == 0) f_delete(fn);
	release(fn);
	active= No;
}

Visible Procedure
initsou()
{
	b_units= mk_elt();
	file_names= mk_elt();
}

Visible Procedure
endsou()
{
	/* Release everything around so "memory leakage" can be detected */
	release(b_units); b_units= Vnil;
	release(b_perm); b_perm= Vnil;
	release(file_names); file_names= Vnil;
	release(last_tname); last_tname= Vnil;
}

Visible Procedure
lst_uhds()
{
	intlet k, len= length(b_perm); int c;
	value name; literal type;
	k_Over_len {
		p_name_type(*key(b_perm, k), &name, &type);
		if (type != Tar) {
			FILE *fn= fopen(strval(*assoc(b_perm, k)), "r");
			if (fn) {
				while ((c= getc(fn)) != EOF && c != '\n')
					putc(c, stdout);
				putc('\n', stdout);
				fclose(fn);
			}
		}
		release(name);
	}
	len= length(b_units);
	k_Over_len {
		how *u= How_to(*assoc(b_units, k));
#ifndef TRY
		value *aa;
		if (u -> filed == No && !p_exists(*key(b_units, k), &aa))
#else
		if (u -> filed == No)
#endif
			display(stdout, u -> unit, Yes);
	}
	fflush(stdout);
}
