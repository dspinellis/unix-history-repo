/************************************************************************/
/*									*/
/*		cplus-field.c						*/
/*									*/
/*	Code for handling XREF output from g++				*/
/*									*/
/************************************************************************/


#ifdef FIELD_XREF

#include "config.h"
#include "tree.h"
#include "cplus-tree.h"
#include "input.h"

#include <stdio.h>
#include <ctype.h>
#include <strings.h>




/************************************************************************/
/*									*/
/*	Common definitions						*/
/*									*/
/************************************************************************/

typedef int	Integer;
typedef char *	String;


#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifndef NULL
#define NULL 0
#endif


#define PALLOC(typ) ((typ *) calloc(1,sizeof(typ)))


#define SALLOC(str) ((String) ((str == NULL) ? NULL : (strcpy(malloc(strlen(str)+1),str))))
#define SFREE(str) (str != NULL && (free(str),0))

#define STREQL(s1,s2) (strcmp((s1),(s2)) == 0)
#define STRNEQ(s1,s2) (strcmp((s1),(s2)) != 0)
#define STRLSS(s1,s2) (strcmp((s1),(s2)) < 0)
#define STRLEQ(s1,s2) (strcmp((s1),(s2)) <= 0)
#define STRGTR(s1,s2) (strcmp((s1),(s2)) > 0)
#define STRGEQ(s1,s2) (strcmp((s1),(s2)) >= 0)





/************************************************************************/
/*									*/
/*	Type definitions						*/
/*									*/
/************************************************************************/


typedef struct _XREF_FILE *	XREF_FILE;
typedef struct _XREF_SCOPE *	XREF_SCOPE;



typedef struct _XREF_FILE {
   String name;
   XREF_FILE next;
} XREF_FILE_INFO;





typedef struct _XREF_SCOPE {
   Integer gid;
   Integer lid;
   XREF_FILE file;
   Integer start;
   XREF_SCOPE outer;
} XREF_SCOPE_INFO;






/************************************************************************/
/*									*/
/*	Local storage							*/
/*									*/
/************************************************************************/


static	char		doing_xref = 0;
static	FILE *		xref_file = NULL;
static	char		xref_name[1024];
static	XREF_FILE	all_files = NULL;
static	String		wd_name = NULL;
static	XREF_SCOPE	cur_scope = NULL;
static	Integer 	scope_ctr = 0;
static	XREF_FILE	last_file = NULL;
static	tree		last_fct = NULL;




/************************************************************************/
/*									*/
/*	Forward definitions						*/
/*									*/
/************************************************************************/


extern	void		FIELD_xref_begin();
extern	void		FIELD_xref_end();
extern	void		FIELD_xref_file();
extern	void		FIELD_xref_start_scope();
extern	void		FIELD_xref_end_scope();
extern	void		FIELD_xref_ref();
extern	void		FIELD_xref_decl();
extern	void		FIELD_xref_call();
extern	void		FIELD_xref_function();
extern	void		FIELD_xref_assign();
extern	void		FIELD_xref_hier();
extern	void		FIELD_xref_member();


static	void		gen_assign();
static	XREF_FILE	find_file();
static	String		filename();
static	String		fctname();
static	String		declname();
static	void		simplify_type();
static	String		fixname();
static	void		open_xref_file();

extern	char *		type_as_string();




/************************************************************************/
/*									*/
/*	FIELD_xref_begin -- start cross referencing			*/
/*									*/
/************************************************************************/


void
FIELD_xref_begin(file)
   String file;
{
   String s,t;

   doing_xref = 1;

   if (file != NULL && STRNEQ(file,"-")) {
      open_xref_file(file);
      FIELD_xref_file(file);
    };
};





/************************************************************************/
/*									*/
/*	FIELD_xref_end -- finish cross referencing			*/
/*									*/
/************************************************************************/


void
FIELD_xref_end(ect)
   int ect;
{
   XREF_FILE xf;

   if (!doing_xref) return;

   xf = find_file(input_filename);
   if (xf == NULL) return;

   while (cur_scope != NULL) {
      FIELD_xref_end_scope(cur_scope->gid,0,0,0,0);
    };

   doing_xref = 0;

   if (xref_file == NULL) return;

   fclose(xref_file);

   xref_file = NULL;
   all_files = NULL;

   if (ect > 0) unlink(xref_name);
};






/************************************************************************/
/*									*/
/*	FIELD_xref_file -- handle file xrefing				*/
/*									*/
/************************************************************************/


void
FIELD_xref_file(name)
   String name;
{
   XREF_FILE xf;
   char wdbuf[1024];

   if (!doing_xref || name == NULL) return;

   if (xref_file == NULL) {
      open_xref_file(name);
      if (!doing_xref) return;
    };

   if (all_files == NULL) {
      fprintf(xref_file,"SCP * 0 0 0 0 RESET\n");
    };

   xf = find_file(name);
   if (xf != NULL) return;

   xf = PALLOC(XREF_FILE_INFO);
   xf->name = SALLOC(name);
   xf->next = all_files;
   all_files = xf;

   if (wd_name == NULL) {
      getwd(wdbuf);
      wd_name = SALLOC(wdbuf);
    };

   fprintf(xref_file,"FIL %s %s 0\n",name,wd_name);

   filename(xf);
   fctname(NULL);
};






/************************************************************************/
/*									*/
/*	FIELD_xref_start_scope -- begin a scope 			*/
/*	FIELD_xref_end_scope -- finish a scope				*/
/*									*/
/************************************************************************/


void
FIELD_xref_start_scope(id)
   Integer id;
{
   XREF_SCOPE xs;
   XREF_FILE xf;

   if (!doing_xref) return;
   xf = find_file(input_filename);

   xs = PALLOC(XREF_SCOPE_INFO);
   xs->file = xf;
   xs->start = lineno;
   if (xs->start <= 0) xs->start = 1;
   xs->gid = id;
   xs->lid = ++scope_ctr;
   xs->outer = cur_scope;
   cur_scope = xs;
};





void
FIELD_xref_end_scope(id,inid,prm,keep,trns)
   Integer id;
   Integer inid;
   Integer prm,keep,trns;
{
   XREF_FILE xf;
   XREF_SCOPE xs,lxs,oxs;
   String stype;

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;

   lxs = NULL;
   for (xs = cur_scope; xs != NULL; xs = xs->outer) {
      if (xs->gid == id) break;
      lxs = xs;
    };
   if (xs == NULL) return;

   if (inid != 0) {
      for (oxs = cur_scope; oxs != NULL; oxs = oxs->outer) {
	 if (oxs->gid == inid) break;
       };
      if (oxs == NULL) return;
      inid = oxs->lid;
    };

   if (prm == 2) stype = "SUE";
   else if (prm != 0) stype = "ARGS";
   else if (keep == 2 || inid != 0) stype = "INTERN";
   else stype = "EXTERN";

   fprintf(xref_file,"SCP %s %d %d %d %d %s\n",
	      filename(xf),xs->start,lineno,xs->lid,inid,stype);

   if (lxs == NULL) cur_scope = xs->outer;
   else lxs->outer = xs->outer;

   free(xs);
};





/************************************************************************/
/*									*/
/*	FIELD_xref_ref -- handle reference				*/
/*									*/
/************************************************************************/


void
FIELD_xref_ref(fct,name)
   tree fct;
   String name;
{
   XREF_FILE xf;

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;

   fprintf(xref_file,"REF %s %d %s %s\n",
	      filename(xf),lineno,fctname(fct),name);
};






/************************************************************************/
/*									*/
/*	FIELD_xref_decl -- handle declaration				*/
/*									*/
/************************************************************************/


void
FIELD_xref_decl(fct,decl)
   tree fct;
   tree decl;
{
   XREF_FILE xf;
   String cls;
   String name;
   char buf[10240];

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;

   if (DECL_NAME(decl) == NULL) return;

   if (TREE_CODE(decl) == TYPE_DECL) cls = "TYPEDEF";
   else if (TREE_CODE(decl) == FIELD_DECL) cls = "FIELD";
   else if (TREE_CODE(decl) == VAR_DECL) {
      if (fct == NULL && TREE_STATIC(decl) &&
	     TREE_READONLY(decl) && DECL_INITIAL(decl) != 0 &&
	     !TREE_PUBLIC(decl) && !TREE_EXTERNAL(decl) &&
	     DECL_MODE(decl) != BLKmode) cls = "CONST";
      else if (TREE_EXTERNAL(decl)) cls = "EXTERN";
      else if (TREE_PUBLIC(decl)) cls = "EXTDEF";
      else if (TREE_STATIC(decl)) cls = "STATIC";
      else if (TREE_REGDECL(decl)) cls = "REGISTER";
      else cls = "AUTO";
    }
   else if (TREE_CODE(decl) == PARM_DECL) cls = "PARAM";
   else if (TREE_CODE(decl) == FIELD_DECL) cls = "FIELD";
   else if (TREE_CODE(decl) == CONST_DECL) cls = "CONST";
   else if (TREE_CODE(decl) == FUNCTION_DECL) {
      if (TREE_EXTERNAL(decl)) cls = "EXTERN";
      else if (TREE_PUBLIC(decl)) cls = "EFUNCTION";
      else cls = "SFUNCTION";
    }
   else if (TREE_CODE(decl) == LABEL_DECL) cls = "LABEL";
   else if (TREE_CODE(decl) == UNION_TYPE) {
      cls = "UNIONID";
      decl = TYPE_NAME(decl);
    }
   else if (TREE_CODE(decl) == RECORD_TYPE) {
      if (CLASSTYPE_DECLARED_CLASS(decl)) cls = "CLASSID";
      else cls = "STRUCTID";
      decl = TYPE_NAME(decl);
    }
   else if (TREE_CODE(decl) == ENUMERAL_TYPE) {
      cls = "ENUMID";
      decl = TYPE_NAME(decl);
    }
   else cls = "UNKNOWN";

   name = IDENTIFIER_POINTER(DECL_NAME(decl));

   type_as_string(buf,TREE_TYPE(decl));
   simplify_type(buf);

   fprintf(xref_file,"DCL %s %d %s %d %s %s %s\n",
	      filename(xf),lineno,name,
	      (cur_scope != NULL ? cur_scope->lid : 0),
	      cls,fctname(fct),buf);
};






/************************************************************************/
/*									*/
/*	FIELD_xref_call -- handle call					*/
/*									*/
/************************************************************************/


void
FIELD_xref_call(fct,name)
   tree fct;
   String name;
{
   XREF_FILE xf;
   char buf[1024];

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;
   name = fixname(name,buf);

   fprintf(xref_file,"CAL %s %d %s %s\n",
	      filename(xf),lineno,name,fctname(fct));
};






/************************************************************************/
/*									*/
/*	FIELD_xref_function -- handle functions 			*/
/*									*/
/************************************************************************/


void
FIELD_xref_function(fct,args)
   tree fct;
   tree args;
{
   XREF_FILE xf;
   int ct;
   char buf[1024];

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;

   ct = 0;
   buf[0] = 0;
   if (args == NULL) args = DECL_ARGUMENTS(fct);

   FIELD_xref_decl(NULL,fct);

   for ( ; args != NULL; args = TREE_CHAIN(args)) {
      FIELD_xref_decl(fct,args);
      if (ct != 0) strcat(buf,",");
      strcat(buf,declname(args));
      ++ct;
    };

   fprintf(xref_file,"PRC %s %d %s %d %d %s\n",
	      filename(xf),lineno,declname(fct),
	      (cur_scope != NULL ? cur_scope->lid : 0),
	      ct,buf);
};






/************************************************************************/
/*									*/
/*	FIELD_xref_assign -- handle assignment				*/
/*									*/
/************************************************************************/


void
FIELD_xref_assign(name)
   tree name;
{
   XREF_FILE xf;

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;

   gen_assign(xf,name);
};



static void
gen_assign(xf,name)
   XREF_FILE xf;
   tree name;
{
   String s;

   s = NULL;

   switch (TREE_CODE(name)) {
      case IDENTIFIER_NODE :
	 s = IDENTIFIER_POINTER(name);
	 break;
      case VAR_DECL :
	 s = declname(name);
	 break;
      case COMPONENT_REF :
	 gen_assign(xf,TREE_OPERAND(name,0));
	 gen_assign(xf,TREE_OPERAND(name,1));
	 break;
      case INDIRECT_REF :
      case OFFSET_REF :
      case ARRAY_REF :
      case BUFFER_REF :
	 gen_assign(xf,TREE_OPERAND(name,0));
	 break;
      case COMPOUND_EXPR :
	 gen_assign(xf,TREE_OPERAND(name,1));
	 break;
      default :
	 break;
    };

   if (s != NULL) {
      fprintf(xref_file,"ASG %s %d %s\n",filename(xf),lineno,s);
    };
};





/************************************************************************/
/*									*/
/*	FIELD_xref_hier -- handle hierarchy				*/
/*									*/
/************************************************************************/


void
FIELD_xref_hier(cls,base,pub,virt,frnd)
   String cls;
   String base;
   int pub;
   int virt;
   int frnd;
{
   XREF_FILE xf;

   if (!doing_xref) return;
   xf = find_file(input_filename);
   if (xf == NULL) return;

   fprintf(xref_file,"HIE %s %d %s %s %d %d %d\n",
	      filename(xf),lineno,cls,base,pub,virt,frnd);
};





/************************************************************************/
/*									*/
/*	FIELD_xref_member -- handle member				*/
/*									*/
/************************************************************************/


void
FIELD_xref_member(cls,fld)
   tree cls;
   tree fld;
{
   XREF_FILE xf;
   String prot;
   Integer confg,pure;
   String d,p;
   Integer i;
   char buf[1024],bufa[1024];

   if (!doing_xref) return;
   xf = find_file(fld->decl.filename);
   if (xf == NULL) return;

   if (TREE_PRIVATE(fld)) prot = "PRIVATE";
   else if (TREE_PROTECTED(fld)) prot = "PROTECTED";
   else prot = "PUBLIC";

   confg = 0;
   if (TREE_CODE(fld) == FUNCTION_DECL && DECL_CONST_MEMFUNC_P(fld)) confg = 1;
   else if (TREE_CODE(fld) == CONST_DECL) confg = 1;

   pure = 0;
   if (TREE_CODE(fld) == FUNCTION_DECL && DECL_ABSTRACT_VIRTUAL_P(fld)) pure = 1;

   d = IDENTIFIER_POINTER(cls);
   sprintf(buf,"%d%s",strlen(d),d);
   i = strlen(buf);
   strcpy(bufa,declname(fld));
   for (p = &bufa[1]; *p != 0; ++p) {
      if (p[0] == '_' && p[1] == '_' && p[2] >= '0' && p[2] <= '9') {
	 if (strncmp(&p[2],buf,i) == 0) *p = 0;
	 break;
       }
      else if (p[0] == '_' && p[1] == '_' && p[2] == 'C' && p[3] >= '0' && p[3] <= '9') {
	 if (strncmp(&p[3],buf,i) == 0) *p = 0;
	 break;
       };
    };

   fprintf(xref_file,"MEM %s %d %s %s %s %d %d %d %d %d %d %d\n",
	      filename(xf),fld->decl.linenum,d,
	      bufa,
	      prot,
	      (TREE_CODE(fld) == FUNCTION_DECL ? 0 : 1),
	      (TREE_INLINE(fld) ? 1 : 0),
	      (DECL_FRIEND_P(fld) ? 1 : 0),
	      (DECL_VIRTUAL_P(fld) ? 1 : 0),
	      (TREE_STATIC(fld) ? 1 : 0),
	      pure,confg);
};






/************************************************************************/
/*									*/
/*	find_file -- find file entry given name 			*/
/*									*/
/************************************************************************/



static XREF_FILE
find_file(name)
   String name;
{
   XREF_FILE xf;

   for (xf = all_files; xf != NULL; xf = xf->next) {
      if (STREQL(name,xf->name)) break;
    };

   return xf;
};





/************************************************************************/
/*									*/
/*	filename -- return name for output				*/
/*									*/
/************************************************************************/


static String
filename(xf)
   XREF_FILE xf;
{
   if (xf == NULL) {
      last_file = NULL;
      return "*";
    };

   if (last_file == xf) return "*";

   last_file = xf;

   return xf->name;
};






/************************************************************************/
/*									*/
/*	fctname -- return name for function				*/
/*									*/
/************************************************************************/


static String
fctname(fct)
   tree fct;
{
   extern char * declname();
   static char fctbuf[1024];
   String s;

   if (fct == NULL && last_fct == NULL) return "*";

   if (fct == NULL) {
      last_fct = NULL;
      return "*TOP*";
    };

   if (fct == last_fct) return "*";

   last_fct = fct;

   s = declname(fct);
   s = fixname(s,fctbuf);

   return s;
};






/************************************************************************/
/*									*/
/*	declname -- print name for declaration				*/
/*									*/
/************************************************************************/


static String
declname(dcl)
   tree dcl;
{
   if (DECL_NAME(dcl) == NULL) return "?";

   return IDENTIFIER_POINTER (DECL_NAME (dcl));
}





/************************************************************************/
/*									*/
/*	simplify_type -- simplify a type string 			*/
/*									*/
/************************************************************************/


static void
simplify_type(typ)
   String typ;
{
   String s;
   Integer lvl,i;

   i = strlen(typ);
   while (i > 0 && isspace(typ[i-1])) typ[--i] = 0;

   if (i > 7 && STREQL(&typ[i-5],"const")) {
      typ[i-5] = 0;
      i -= 5;
    };

   if (typ[i-1] != ')') return;

   s = &typ[i-2];
   lvl = 1;
   while (*s != 0) {
      if (*s == ')') ++lvl;
      else if (*s == '(') {
	 --lvl;
	 if (lvl == 0) {
	    s[1] = ')';
	    s[2] = 0;
	    break;
	  };
       };
      --s;
    };

   if (*s != 0 && s[-1] == ')') {
      --s;
      --s;
      if (*s == '(') s[2] = 0;
      else if (*s == ':') {
	 while (*s != '(') --s;
	 s[1] = ')';
	 s[2] = 0;
       };
    };
};





/************************************************************************/
/*									*/
/*	fixname -- fixup a function name (take care of embedded spaces	*/
/*									*/
/************************************************************************/


static String
fixname(nam,buf)
   String nam;
   String buf;
{
   String s,t;
   int fg;

   s = nam;
   t = buf;
   fg = 0;

   while (*s != 0) {
      if (*s == ' ') {
	 *t++ = '\36';
	 ++fg;
       }
      else *t++ = *s;
      ++s;
    };

   if (fg == 0) return nam;

   return buf;
};





/************************************************************************/
/*									*/
/*	open_xref_file -- open file for xrefing 			*/
/*									*/
/************************************************************************/


static void
open_xref_file(file)
   String file;
{
   String s,t;

   s = rindex(file,'/');
   if (s == NULL) sprintf(xref_name,".%s.gxref",file);
   else {
      ++s;
      strcpy(xref_name,file);
      t = rindex(xref_name,'/');
      ++t;
      *t++ = '.';
      strcpy(t,s);
      strcat(t,".gxref");
    };

   xref_file = fopen(xref_name,"w");

   if (xref_file == NULL) {
      error("Can't create cross-reference file `%s'",xref_name);
      doing_xref = 0;
    };
};



#endif


/* end of cplus-field.c */
