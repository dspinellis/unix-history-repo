/* $Header: /f/osi/others/quipu/uips/xd/RCS/calls.c,v 7.4 91/02/22 09:32:41 mrose Interim $ */
#ifndef lint
	static char *rcsid = "$Id: calls.c,v 7.4 91/02/22 09:32:41 mrose Interim $";
#endif
/*
 $Log:	calls.c,v $
 * Revision 7.4  91/02/22  09:32:41  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:50:20  mrose
 * sync
 * 
 * Revision 7.2  90/07/27  08:45:53  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:42:12  mrose
 * sync
 * 
 * Revision 7.0  90/06/12  13:10:48  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:34  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:04  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:04  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/09  15:57:27  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:36  emsrssn
 * Initial revision
 * 
 * 
*/

#include "manifest.h"
#include "quipu/util.h"
#include "quipu/common.h"
#include "quipu/entry.h"
#include <malloc.h>
#include "usr.dirent.h"
#include "tailor.h"
#include "sequence.h"
#include "filt.h"
#include "y.tab.h"
#include "symtab.h"

struct attrcomp * sort_attrs();
static dn2str ();

#define RESBUF 10000

PS opt;
Attr_Sequence read_types = 0, read_types2;

table_entry symtab = 0;
int typetoggled = 0;

int dn_print (), rdn_print(), as_print();

char bound = FALSE;  /* indication of whether bound */
char * TidyString();

D_seq dnseq = NULLDS, backseq = NULLDS, showseq = NULLDS;
int entry_number, back_buf_num, element_num;

/* These are common operation variables */
#define STRINGLEN 1000
#define SMALLSTRING 255
#define MAXTYPES  255

char            goto_path[STRINGLEN];         /* Used by the 'G:goto' command*/
char            base_path[STRINGLEN];         /* Used by all DS operations   */
char 		friendly_base_path[STRINGLEN];
char		friendly_name[STRINGLEN];
char            namestr[STRINGLEN];
char            cache [STRINGLEN];
char            bindpass [STRINGLEN];
char            srchvalue[STRINGLEN];         /* Used by search */
char            svalue [STRINGLEN];
char            mvalue [STRINGLEN];

unsigned int curr_filt = 0;
unsigned int filt_num = 0;
unsigned int typeindx = 0;
filt_struct *filt_arr[MAXTYPES];
char	*filtvalue[MAXTYPES];
char    *filttype[MAXTYPES];

int default_num;
int *av_typeindx;
int *available_types[MAXTYPES];
char  *levels[MAXTYPES];
int  defaults[MAXTYPES];

extern Filter make_filter();
extern int Switch_On_Result_Update();
extern int Switch_Off_Result_Update();
extern void Clear_Results();
extern void xprint();

extern char *local_dit;

#ifndef NO_STATS
extern LLog    *log_stat;
#endif


/* These are used as the data for the binding connection */
char   passwd[STRINGLEN];
extern char *   myname;

extern int asizelimit;
DN user_name;

char * addobj = NULLCP;
Filter search_filter;
FILE *file;
char	       *file_names[MAXTYPES];
char dua_help_dir[256];

char *get_strioid(ptr)
register char *ptr;
{
  register char *end_ptr;

  while(*ptr == '"') ptr++;
  while(*ptr != '"') ptr++;

  while(*ptr > '9' || *ptr < '0') ptr++;

  end_ptr = ptr;
  while(*end_ptr != '\n') end_ptr++;

  *end_ptr = '\0';
  return ptr;
}


user_tailor ()
{
  char           *part1;
  char           *part2;
  char           *getenv ();
  char           *ptr = "/.quipurc";
  char	         *config_dir = "/.duaconfig/";
  char           *isode_config_dir = "xd/duaconfig/";
  char	         *type_dir = "filterTypes/";
  char	         *read_File = "readTypes";
  char           *type_defaults = "typeDefaults";

  char            Dish_Home[BUFSIZ];
  char		  read_Home[BUFSIZ];
  char		  type_Home[BUFSIZ];
  char            type_defaults_Home[BUFSIZ];

  char	          stroid_buf[BUFSIZ];

  DIR 	         *dir;

  struct dirent  *dir_ent;
  
  char            Read_in_Stuff[STRINGLEN];
  char           *p, 
  		 *TidyString(),
  		 *SkipSpace(),
                 *end;

  int             count, n, num;
  int     tempints[MAXTYPES];

  (void) strcpy(dua_help_dir, isodefile("xd/helpdir/", 0));


  if ((opt = ps_alloc (std_open)) == NULLPS)
    fatal (-1,"ps_alloc failed");
  if (std_setup (opt,stdout) == NOTOK)
    fatal (-1,"std_setup failed");

  namestr[0] = '\0';
  *passwd = '\0';
  cache[0] = '\0';

  (void) strcpy (Dish_Home, getenv ("HOME"));
  (void) strcpy(read_Home, Dish_Home);
  (void) strcpy(type_Home, Dish_Home);
  
  (void) strcat(Dish_Home, ptr);
  (void) strcat(read_Home, config_dir);
  (void) strcat(read_Home, read_File);
  (void) strcat(type_Home, config_dir);
  (void) strcat(type_Home, type_dir);
	
  if (!(dir = opendir(type_Home))) {
    (void) strcpy(type_Home, isodefile(isode_config_dir,0));
    (void) strcat(type_Home, type_dir);
    if(!(dir = opendir(type_Home))) {
       (void) strcpy(type_Home, "./");
       (void) strcat(type_Home, isode_config_dir);
       (void) strcat(type_Home, type_dir);
       if(!(dir = opendir(type_Home))) {
         (void) printf("Can't find directory %s!\n", type_dir);
         quit(1);
       }
    }
  }

  rewinddir(dir);
  filt_num = 0;
  while(dir_ent = readdir(dir)) {
    if (!(strncmp(dir_ent->d_name, "Type_", 5))) {
      file_names[filt_num] = 
	(char *) malloc((unsigned int) (strlen(dir_ent->d_name) + strlen(type_Home) + 2) );
      (void) strcpy(file_names[filt_num], type_Home);
      (void) strcat(file_names[filt_num], dir_ent->d_name);
      filt_num++;
    }
  }
  (void) closedir(dir);
  
  if ((file = fopen (Dish_Home, "r")) == 0);
  else {
  while (fgets (Read_in_Stuff, STRINGLEN, file) != 0) {
    p = SkipSpace (Read_in_Stuff);
    if (( *p == '#') || (*p == '\0'))
      continue;  /* ignore comments and blanks */

    part1 = p;
    if ((part2 = index (p,':')) == NULLCP) 
      continue; /* ignore it */

    *part2++ = '\0';
    part2 = TidyString(part2);
    
    if (strcmp (part1, "username") == 0) 
      (void) strcpy (namestr, part2);
    else if (strcmp (part1, "password") == 0) 
      (void) strcpy (passwd, part2);
    else if (lexequ (part1, "dsap") == 0)
      (void) tai_string (part2);
    else if (lexequ (part1, "isode") == 0) {
      char * split;
      if ((split = index (part2,' ')) != NULLCP) {
	*split++ = 0;
	(void) isodesetvar (part2,split,0);
      }
    } else if (strcmp (part1, "service") == 0) 
      new_service (part2);
  }
  isodexport (NULLCP);
  (void) fclose(file);
  }

  if (!(file = fopen(read_Home, "r"))) {
    (void) strcpy(read_Home, isodefile(isode_config_dir,0));
    (void) strcat(read_Home, read_File);
    if (!(file = fopen(read_Home, "r"))) {
      (void) strcpy(read_Home, "./");
      (void) strcat(read_Home, isode_config_dir);
      (void) strcat(read_Home, read_File);
      if (!(file = fopen(read_Home, "r"))) {
        (void) printf("Can't find read file (%s)!\n", read_Home);
        quit(1);
      }
    }
  }
  load_oid_table("oidtable");
  
  while(fgets(Read_in_Stuff, STRINGLEN, file) != 0) {
    (void) strcpy(stroid_buf, get_strioid(Read_in_Stuff));
    if (*stroid_buf) {
      if (!read_types)
	read_types = as_comp_new(AttrT_new(stroid_buf), NULLAV, NULLACL_INFO);
      else {
	read_types2 = as_comp_new(AttrT_new(stroid_buf) ,NULLAV, NULLACL_INFO);
	read_types = as_merge(read_types, read_types2);
      }
    }
  }
  (void) fclose(file);
  
  for (curr_filt = 0; curr_filt < filt_num; curr_filt++) {
    if (!(file = fopen(file_names[curr_filt], "r"))) {
      (void) printf("Can't find file %s!\n", (char *) file_names[curr_filt]);
      quit(1);
    }
    filtvalue[curr_filt] = (char *) malloc(STRINGLEN);
    *filtvalue[curr_filt] = '\0';
    
    yyparse();
    (void) fclose(file);
  }
  filttype[curr_filt] = NULLCP;
  for (count = 0; count < filt_num; count++)
    free(file_names[count]);

  if (!(file = fopen(type_defaults_Home, "r"))) {
    (void) strcpy(type_defaults_Home, isodefile(isode_config_dir,0));
    (void) strcat(type_defaults_Home, type_defaults);
    if (!(file = fopen(type_defaults_Home, "r"))) {
      (void) strcpy(type_defaults_Home, "./Xd/duaconfig/");
      (void) strcat(type_defaults_Home, type_defaults);
      if (!(file = fopen(type_defaults_Home, "r"))) {
        (void) fprintf(stderr, "Can't open typeDefaults file\n");
        quit(1);
      }
    }
  }

  default_num = 0;
  while (fgets (Read_in_Stuff, STRINGLEN, file) != 0) {
    p = SkipSpace(Read_in_Stuff);
    if (( *p == '#') || (*p == '\0'))
      continue;

    part1 = p;
    if ((part2 = index (p,':')) == NULLCP)
      continue;

    end = part2 - 1;
    while (isspace(*end)) end--;
    *++end = '\0';

    *part2++ = '\0';

    while (isspace(*part2)) part2++;
    end = part2;

    while (!isspace(*end) && *end != ',' && *end != ':') end++;

    count = 0;
    while (*part2 != ':') {
      n = 0;
      while (n < filt_num && strncmp(filttype[n], part2,
                                    (int) (end - part2))) n++;

      if (n == filt_num) {
        (void) fprintf(stderr, "Parsing error in typeDefaults file!");
        quit(1);
      } else {
        tempints[count] = n;
        count++;
        part2 = end;
        while (!isalpha(*part2) && *part2 != ':' && part2 != '\0') part2++;

        if (*part2 == '\0') {
          (void) fprintf(stderr, "Parsing error in typeDefaults file!");
          quit(1);
        }

        if (*part2 != ':') {
          while (!isalpha(*part2)) part2++;
          end = part2;
          while (!isspace(*end) && *end != ',' &&
                 *end != ':' && *end != '\0') end++;
          if (*end == '\0') {
            (void) fprintf(stderr, "Parsing error in typeDefaults file!");
            quit(1);
          }
        } else end = part2;
      }
    }

    if (*end == ':') {
      while(isspace(*++end));
      p = end;
      while(!isspace(*++end));
      *end = '\0';

      n = 0;
      while (n < filt_num && strcmp(filttype[n], p)) n++;

      if (n == filt_num) {
        (void) fprintf(stderr, "Parsing error in typeDefaults file!");
        quit(1);
      } else {
        num = 0;
        while (num < count && n != tempints[num]) num++;
        if (num == count) {
          (void) fprintf(stderr, "Parsing error in typeDefaults file!");
          quit(1);
        }
      }

      defaults[default_num] = n;

      levels[default_num] = malloc((unsigned int) (strlen(part1) + 1) );
      (void) strcpy(levels[default_num], part1);
      available_types[default_num] =
        (int *) malloc((unsigned int) (sizeof(int) * (count+1)) );

      for (n = 0; n < count; n++)
        available_types[default_num][n] = tempints[n];

      available_types[default_num][n] = -1;
      default_num++;
    }
  }
  av_typeindx = available_types[0];
  typeindx = defaults[0];
}

cnnct_bind()
{
  struct ds_bind_arg bindarg;
  struct ds_bind_arg bindresult;
  struct ds_bind_error binderr;
  extern  char * dsa_address,
  	       * myname;
  extern  char * tailfile;
  FILE * fp;
  char buf [BUFSIZ];

  if (*passwd != 0)
    (void) strcpy(bindpass,"******");
  else
    bindpass[0] = '\0';
  /* set dsa_address */
  dsa_address = NULLCP;
  /* read tailor file to get address */
  if( (fp = fopen(isodefile(tailfile,0), "r")) == (FILE *)NULL) {
    (void) printf ("Cannot open tailor file %s\n",isodefile(tailfile,0));
    return;
  }
  while(fgets(buf, sizeof(buf), fp) != NULLCP)
    if ( (*buf != '#') && (*buf != '\n') )
      (void) tai_string (buf);
  
  (void) fclose(fp);
  
  if (dsa_address == NULLCP)
    dsa_address = myname;
  
  /* set password */
  if (bindpass[0] != 0) {
    if (strcmp (bindpass,"******") != 0)
      (void) strcpy (passwd,bindpass);
  } else 
    passwd[0] = 0;
  
  /* now bind */
  bindarg.dba_version = DBA_VERSION_V1988;
  if (passwd[0] == 0) {
    bindarg.dba_passwd_len = 0;
    bindarg.dba_passwd [0] = '\0';
  } else {
    bindarg.dba_passwd_len = strlen (passwd);
    (void) strcpy (bindarg.dba_passwd,passwd);
  }
  
  bindarg.dba_dn = (*namestr == 0? NULLDN: str2dn(namestr));

  if (ds_bind (&bindarg,&binderr,&bindresult) != DS_OK) {
    (void) printf (binderr.dbe_type == DBE_TYPE_SECURITY?
	    "Bind security error - Check name and pasword.\n":
	    "Bind service error - Can't contact DSA!\n");
    quit(1);
  } else {
    user_name = bindarg.dba_dn;
    (void) strcpy (buf, "TERM");
    if(local_dit && *local_dit)
      (void) strcpy(base_path, local_dit);

#ifndef NO_STATS
    LLOG (log_stat,LLOG_NOTICE,("bound ('%s' to '%s')",namestr,dsa_address));
#endif
#ifndef NO_STATS
    LLOG (log_stat,LLOG_NOTICE,("xd bound to directory"));
#endif

    make_friendly(friendly_base_path, base_path);
    set_default_type();
  }
  entry_number = 0;
  back_buf_num = 0;
  backseq = dnseq = NULLDS;
}

rd_start()
{
  struct ds_read_arg read_arg;
  struct ds_read_result   result;
  struct DSError          error;
  Entry read_entry;
  
  if (*friendly_base_path == 'T') {
    free_seq(dnseq);
    dnseq = NULLDS;
    entry_number  = 0;
    return;
  }


  if (get_default_service (&read_arg.rda_common) != 0) {
    xprint ("Default service error -> check your .quipurc\n");
    return ;
  }

  read_arg.rda_eis.eis_allattributes = FALSE;
  read_arg.rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
  read_arg.rda_eis.eis_select = read_types;

  read_arg.rda_common.ca_servicecontrol.svc_options = 1;
  read_arg.rda_object = (*friendly_base_path != 'T'? str2dn(base_path): NULLDN);
  if ((read_entry = 
       local_find_entry (read_arg.rda_object, FALSE)) != NULLENTRY) {
    read_entry->e_attributes = sort_attrs(read_entry->e_attributes);
    read_print (as_print, (caddr_t) read_entry->e_attributes);
    return;
  }

#ifndef NO_STATS
  LLOG (log_stat,LLOG_NOTICE,("read +%s",base_path));
#endif

  if (ds_read (&read_arg,&error,&result) != DS_OK) {
    /* deal with error */
    Clear_Results();
    xprint("Read error due to:\n");
    quipu_error(&error);
  } else {
    /* use data */
    if (result.rdr_entry.ent_attr == NULLATTR) {
      free_seq(dnseq);
      dnseq = NULLDS;
      entry_number = 0;
      xprint("No attributes found! Sorry.");
      return;
    }
    xprint("Result of look up ...\n");

    if (result.rdr_common.cr_aliasdereferenced)
      xprint("Alias dereferenced)\n");

    result.rdr_entry.ent_attr = sort_attrs(result.rdr_entry.ent_attr);
    cache_entry(&(result.rdr_entry), TRUE, TRUE);
    read_print(as_print, (caddr_t) result.rdr_entry.ent_attr);
  }
}

back_start()
{
  if (!back_buf_num) {
    xprint("You haven't been anywhere yet!\n");
    return;
  }
  element_num = back_buf_num;
  showseq = backseq;
}

widen()
{
  register char *str, *sptr;
  int count = 0;

  str = get_from_seq(count+1, backseq);
  while (count < back_buf_num && strcmp(str, base_path)){
    count++;
    str = get_from_seq(count+1, backseq);
  }	
  if (count == back_buf_num) {
    add_seq(&backseq, base_path);
    back_buf_num++;
  }

  str = base_path;
  if (*str != 'T') {

    for (sptr = str; *sptr != '\0'; sptr++)
      if (*sptr == '@')	str = sptr;

    sptr = str;
    typetoggled = 0;

    if (str != base_path) {
      if (*--sptr == ' ')
	str = sptr;
      *str = '\0';
    } else
      (void) strcpy(base_path, "The World");

    make_friendly(friendly_base_path, base_path);
    rd_start();
    set_default_type();
  }
}

set_default_type()
{
  int count;
  DN base_name;

  if (*base_path != 'T') {
    base_name = str2dn(base_path);
    while (base_name->dn_parent) base_name = base_name->dn_parent;
    for (count = 0; count < default_num &&
         strcmp(levels[count] ,base_name->dn_rdn->rdn_at->
                oa_ot.ot_stroid);
         count++);

    if (count < default_num) {
      av_typeindx = available_types[count];
      typeindx = defaults[count];
    } else {
      av_typeindx = available_types[0];
      typeindx = defaults[0];
    }
  } else {
    for(count = 0; count < default_num && strcmp(levels[count], "@"); count++);
    if (count < default_num) {
      av_typeindx = available_types[count];
      typeindx = defaults[count];
    } else {
      av_typeindx = available_types[0];
      typeindx = defaults[0];
    }
  }

  make_friendly(friendly_base_path, base_path);
  typetoggled = 0;
  Set_Search_Type(filttype[typeindx]);    /* change string in widget */
}

/* These are the functions called by the list level widgets */

list_start()
{
  struct ds_search_arg search_arg;
  struct ds_search_result result;
  struct DSError          error;

  xprint("OK, listing.");
  xprint("Chugging along.....");

  if (get_default_service (&search_arg.sra_common) != 0) {
    xprint ("Default service error -> check your .quipurc\n");
    return ;
  }
  
  search_arg.sra_common.ca_servicecontrol.svc_sizelimit = asizelimit;
  search_arg.sra_common.ca_servicecontrol.svc_options = 1;
  search_arg.sra_baseobject = (*base_path != 'T'?
			       str2dn (base_path):
			       NULLDN);
  search_arg.sra_eis.eis_allattributes = FALSE;
  search_arg.sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
  search_arg.sra_eis.eis_select = read_types;
  search_arg.sra_subset = SRA_ONELEVEL;

  search_arg.sra_filter = filter_alloc();
  search_arg.sra_filter->flt_type = FILTER_NOT;
  search_arg.sra_filter->flt_next = NULLFILTER;
  search_arg.sra_filter->flt_un.flt_un_filter = filter_alloc();
  search_arg.sra_filter->flt_un.flt_un_filter->flt_type = FILTER_ITEM;
  search_arg.sra_filter->flt_un.flt_un_filter->flt_next = NULLFILTER;
  search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.fi_type
    = FILTERITEM_EQUALITY;

  search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.fi_un.
    fi_un_ava.ava_type = AttrT_new("2.5.4.0");

  search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.fi_un.
    fi_un_ava.ava_value = 
      str2AttrV("dsa", search_arg.sra_filter->flt_un.flt_un_filter->
		flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->
		oa_syntax);

#ifndef NO_STATS
  LLOG (log_stat,LLOG_NOTICE,("search +%s,extent %d, val objectClass != dsa",base_path,search_arg.sra_subset));
#endif


  if (search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.
      fi_un.fi_un_ava.ava_value == NULLAttrV) {
    xprint("No can do. Sorry!");
  } else if (ds_search (&search_arg,&error,&result) != DS_OK) {
    /* deal with error */
    free_seq(dnseq);
    dnseq = NULLDS;
    entry_number = 0;
    xprint("Search error due to:\n");
    quipu_error(&error);
  } else {
    correlate_search_results (&result);
    if (result.CSR_entries != NULLENTRYINFO) {
      register EntryInfo *ptr;
      
      xprint ("Result of search ...\n");
      if (result.CSR_common.cr_aliasdereferenced) {
	xprint ("(Alias dereferenced -  object is ");
	quipu_print (dn_print, (caddr_t) result.CSR_object);
	dn_free (result.CSR_object);
	xprint (")\n");
      }

      free_seq(dnseq);
      dnseq = NULLDS;
      entry_number = 0;

      for (ptr = result.CSR_entries; ptr != NULLENTRYINFO;
	   ptr = ptr->ent_next) {
	entry_number++;
	dn2str ((caddr_t)ptr->ent_dn, goto_path);
	add_seq (&dnseq, goto_path);
	cache_entry (ptr,TRUE,TRUE);
      }

      if (entry_number == 1)
	get_listed_object(1);

    } else if (result.CSR_limitproblem != LSR_TIMELIMITEXCEEDED) {
      free_seq(dnseq);
      dnseq = NULLDS;
      entry_number  = 0;
      xprint("Nothing found. Sorry!");
    }

    if(result.CSR_limitproblem != LSR_NOLIMITPROBLEM) {
      if(result.CSR_limitproblem == LSR_TIMELIMITEXCEEDED) {
	free_seq(dnseq);
	dnseq = NULLDS;
	entry_number = 0;
	xprint("(Time limit exceeded)");
      } else
	xprint("(Size limit exceeded)");
    }
    entryinfo_free(result.CSR_entries, 0);
  }

  filter_free(search_arg.sra_filter);
}

rdn2str(ptr,cptr)
caddr_t ptr;
char * cptr;
{
  PS ps;
  char buffer [RESBUF];
  
  if ((ps = ps_alloc(str_open)) == NULLPS) {
    return ;
  }
  if (str_setup(ps, buffer, RESBUF, 1) == NOTOK) {
    return ;
  }
  rdn_print(ps, (RDN) ptr, READOUT);
  
  ps_free(ps);
  *ps->ps_ptr = 0;
  
  (void) strcpy(cptr, buffer);
}


/* search ... */

srch_start()
{
  struct ds_search_arg search_arg;
  struct ds_search_result result;
  struct DSError          error;
  char *str = base_path;

  if (*mvalue == '\0') {
    list_start();
    return;
  }
  
  xprint("OK. Starting search.\n");

  if (get_default_service (&search_arg.sra_common) != 0) {
    xprint ("Default service error -> check your .quipurc\n");
    return ;
  }
  
  search_arg.sra_common.ca_servicecontrol.svc_sizelimit = asizelimit;
  search_arg.sra_common.ca_servicecontrol.svc_options = 1;
  search_arg.sra_baseobject = (*base_path != 'T'? 
			       str2dn (base_path):
			       NULLDN);
  search_arg.sra_eis.eis_allattributes = FALSE;
  search_arg.sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
  search_arg.sra_eis.eis_select = read_types;
  
  while (*str != '\0' && *str != '@') str++;
  
  search_arg.sra_subset = ((*base_path != 'T' && *str == '@')?
			 SRA_WHOLESUBTREE:
			 SRA_ONELEVEL); 
  search_arg.sra_filter =
    make_filter(filt_arr[typeindx]);

#ifndef NO_STATS
  LLOG (log_stat,LLOG_NOTICE,("search +%s, extent %d, val %s",base_path,search_arg.sra_subset,mvalue));
#endif


  if(ds_search (&search_arg,&error,&result) != DS_OK) {
    /* deal with error */
    free_seq(dnseq);
    dnseq = NULLDS;
    entry_number = 0;
    xprint(" Search error due to:\n ");
    quipu_error(&error);
  } else {
    correlate_search_results (&result);

    if (result.CSR_entries != NULLENTRYINFO) {
      register EntryInfo *ptr;

      if (result.CSR_common.cr_aliasdereferenced) {
	xprint (" (Alias dereferenced - object is ");
	quipu_print (dn_print, (caddr_t) result.CSR_object);
	dn_free (result.CSR_object);
	xprint (")\n");
      }

      free_seq(dnseq);
      dnseq = NULLDS;
      entry_number = 0;
      for (ptr = result.CSR_entries; 
	   ptr != NULLENTRYINFO; ptr = ptr->ent_next){
	entry_number++;
	dn2str ((caddr_t) ptr->ent_dn, goto_path);
	add_seq (&dnseq, goto_path);
	cache_entry (ptr,TRUE,TRUE);
      }

      if (entry_number == 1)
	get_listed_object(1);

    } else if(result.CSR_limitproblem != LSR_TIMELIMITEXCEEDED) {
      free_seq(dnseq);
      dnseq = NULLDS;
      entry_number = 0;
      xprint("Nothing found using current search parameters. Sorry!\n");
    }
    
    if(result.CSR_limitproblem != LSR_NOLIMITPROBLEM)
      if(result.CSR_limitproblem == LSR_TIMELIMITEXCEEDED) {
	free_seq(dnseq);
	dnseq = NULLDS;
	entry_number = 0;
	xprint("(Time limit exceeded)");
      } else
	xprint("(Size limit exceeded)");
    
    entryinfo_free(result.CSR_entries, 0);
  }
  filter_free(search_arg.sra_filter);
}

static dn2str (ptr,cptr)
caddr_t ptr;
char * cptr;
{
  PS ps;
  char buffer [RESBUF];
  
  if((ps = ps_alloc(str_open)) == NULLPS) return ;

  if(str_setup(ps, buffer, RESBUF, 1) == NOTOK) return ;
  
  dn_print(ps, (DN) ptr, EDBOUT);
  *ps->ps_ptr = 0;
  
  ps_free (ps);

  (void) strcpy(cptr, buffer);
}


read_print(func,ptr)
int (*func) ();
caddr_t ptr;
{
  PS ps;
  char buffer [RESBUF];
  char save;
  int i, size;
  register char *str, *sptr;
  
  if ((ps = ps_alloc (str_open)) == NULLPS) return ;
  
  if (str_setup (ps,buffer,RESBUF,1) == NOTOK) return ;
  
  (*func) (ps, ptr, READOUT);
  *ps->ps_ptr = 0;

  ps_free(ps);
  str = buffer ;
  sptr = str;
  size = strlen(buffer);
  
  free_seq(dnseq);
  dnseq = NULLDS;
  entry_number = 0;

  Switch_Off_Result_Update();
  
  for (i = 0; i <= size; i++, sptr++)
    if (*sptr == '\n' || *sptr == '\0') {
      entry_number++;
      save = *sptr ;
      *sptr = '\0';
      xprint(str);
      str = sptr+1;
      *sptr = save;
    }

  Switch_On_Result_Update();
}

quipu_print (func,ptr)
int (*func) ();         /* assumes func (PS ,dataptr,(int) format); */
caddr_t ptr;
{
  /* log info to pstream */
  PS ps;
  char buffer [RESBUF];
  register char *str, *sptr;
  char save;

  if ((ps = ps_alloc (str_open)) == NULLPS) return ;
  
  if (str_setup (ps,buffer,RESBUF,1) == NOTOK) return ;
  
  (*func) (ps,ptr,READOUT);
  *ps->ps_ptr = 0;
  
  ps_free (ps);
  
  /* print in blocks of 100 bytes-larger seems too much for curses*/
  str = buffer;
  do {
    for (sptr = str; *sptr != '\0'; sptr++);
    save = *sptr;
    *sptr = 0;
    xprint (str);
    *sptr = save;
    str = sptr;
  } while (*sptr != '\0');
}

quipu_error (err)
struct DSError * err;
{
  PS ps;
  char buffer [RESBUF];
  
  if ((ps = ps_alloc(str_open)) == NULLPS) return ;
  
  if (str_setup (ps, buffer, RESBUF, 1) == NOTOK) return ;
  ds_error(ps, err);
  
  *ps->ps_ptr = 0;
  xprint(buffer);
}

free_all()
{
  int count;
  
  free_seq(dnseq);
  free_seq(backseq);
  free_table(symtab);
  for (count = 0; count < filt_num; count++) {
    free_filt(filt_arr[count]);
    free(filtvalue[count]);
    free(filttype[count]);
  }
}

get_listed_object(entrynum)
int entrynum;
{
  int count = 0;
  char *sptr, *str;
  
  if (entrynum > element_num) return;

  if (sptr = get_from_seq (entrynum, showseq)) {

    str = get_from_seq(count+1, backseq);
    while (count < back_buf_num && strcmp(str, base_path)){
      count++;
      str = get_from_seq(count+1, backseq);
    }
    
    if (count == back_buf_num) {
      add_seq(&backseq, base_path);
      back_buf_num++;
    }

    (void) strcpy(base_path, sptr);
    make_friendly(friendly_base_path, base_path);
    rd_start();
    typetoggled = 0;
    set_default_type();
  }
  *srchvalue = '\0';
}

make_friendly(fstr, str)
char *fstr;
register char *str;
{
  register char *end_ptr;
  char save;
  
  *fstr = '\0';
  if (!strcmp(str, "The World")) {
    (void) strcpy(fstr, str);
    return;
  }

  while (*str != '\0') {
    while (*str != '=') str++;
    while (*str == ' ') str++;

    end_ptr = ++str;
    while (*end_ptr != '@' && *end_ptr != '\0') end_ptr++;
    save = *end_ptr;
    *end_ptr = '\0';
    if (*fstr == '\0')
      (void) strcpy(fstr, str);
    else
      (void) strcat(fstr, str);
    *end_ptr = save;
    str = end_ptr;
    if (*str != '\0')
      (void) strcat(fstr, ", ");
  }
}
  


goto_addr()
{
  set_default_type();
  rd_start();
}

struct attrcomp *
sort_attrs(entry_attrs)
struct attrcomp *entry_attrs;
{
  struct attrcomp *last, *next, *curr, *first, *firstn;

  first = curr = entry_attrs;
  firstn = last = next = 0;

  while (curr)
    if (!strcmp("2.5.4.3", curr->attr_type->oa_ot.ot_stroid) ||
        !strcmp("2.5.4.4", curr->attr_type->oa_ot.ot_stroid) ||
        !strcmp("0.9.2342.19200300.100.1.3",
                curr->attr_type->oa_ot.ot_stroid) ||
        !strcmp("0.9.2342.19200300.100.1.2",
                curr->attr_type->oa_ot.ot_stroid) ||
        !strcmp("2.5.4.20", curr->attr_type->oa_ot.ot_stroid)) {

      if (first == curr) first = curr->attr_link;

      if (next)
        next->attr_link = curr;
      else
        firstn = curr;

      next = curr;

      if (last)
        last->attr_link = curr->attr_link;

      curr = curr->attr_link;
      next->attr_link = 0;
    } else {
      last = curr;
      curr = curr->attr_link;
    }

  if (next) {
    next->attr_link = first;
    return firstn;
  } else
    return first;
}
