/* calls.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/calls.c,v 7.4 91/02/22 09:32:09 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/calls.c,v 7.4 91/02/22 09:32:09 mrose Interim $
 */

#include "calls.h"

#define RESBUF 10000

typedef enum {rfc822, greybook} mailtype;
mailtype mailformat = rfc822;

int scrollize;
PS opt;
Attr_Sequence read_types = 0, read_types2 = 0, oclass = 0;

str_seq curr_dnseq, textseq, back_seq;

int current_entry, entry_number, display_entry;
int back_buf_num;

int dn_print (), rdn_print(), as_print();
void quit(), int_quit();
void help_back(), help_list(), help_up(), help_number(), help_srch(), 
     help_cncs();
void setwidgets();

char bound = FALSE;  /* indication of wether bound */
char * TidyString();

/* hack to get isode/curses compatability */
#define WINDOW char

#include "widget.h"
#include "wdgtdefs.h"

extern text_height;
int text_state;
int histlimit = 20;
char testing = FALSE;

#define DN_LIST 0
#define TEXT 1
#define BACK_LIST 3

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
char            mvalue [STRINGLEN];

unsigned int curr_filt = 0;
unsigned int filt_num = 0;
unsigned int typeindx = 0;
filt_struct *filt_arr[MAXTYPES];
char	*filtvalue[MAXTYPES];
char  *filttype[MAXTYPES];

int default_num;
int *av_typeindx;
int *available_types[MAXTYPES];
char  *levels[MAXTYPES];
int  defaults[MAXTYPES];

#ifndef NO_STATS
extern LLog    *log_stat;
#endif 

/* These are used as the data for the binding connection */
char   passwd[STRINGLEN];
extern char *   myname;

extern int sizelimit;
extern char    *local_dit;
DN user_name;

char * addobj = NULLCP;

Filter search_filter;

FILE *config_file;
char *file_names[MAXTYPES];

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

void user_tailor()
{
  char           *part1;
  char           *part2;
  char           *getenv ();
  char           *ptr = "/.duarc";
  char	         *config_dir = "/.duaconfig/";
  char           *isode_config_dir = "sd/duaconfig/";
  char	         *type_dir = "filterTypes/";
  char	         *readTypes = "readTypes";
  char           *typeDefaults = "typeDefaults";

  char            user_home[BUFSIZ];
  char		  read_path[BUFSIZ];
  char		  type_path[BUFSIZ];
  char            type_defaults_path[BUFSIZ];

  char	          stroid_buf[BUFSIZ];

  DIR 	         *config_directory;

  struct dirent  *dir_ent;
  
  char            Read_in_Stuff[STRINGLEN];
  char           *p, 
  		 *TidyString(),
  		 *SkipSpace(),
  		 *end;

  int             count, n, num;
  int     tempints[MAXTYPES];

#ifndef NO_STATS
  ll_hdinit (log_stat,"sd");
#endif

  if ((opt = ps_alloc (std_open)) == NULLPS)
    fatal (-1,"ps_alloc failed");
  if (std_setup (opt,stdout) == NOTOK)
    fatal (-1,"std_setup failed");

  cache[0] = '\0';

  (void) strcpy (user_home, getenv ("HOME"));
  (void) strcpy(read_path, user_home);
  (void) strcpy(type_path, user_home);
  (void) strcpy(type_defaults_path, user_home);
  
  (void) strcat(user_home, ptr);
  (void) strcat(read_path, config_dir);
  (void) strcat(read_path, readTypes);
  (void) strcat(type_path, config_dir);
  (void) strcat(type_path, type_dir);
  (void) strcat(type_defaults_path, config_dir);
  (void) strcat(type_defaults_path, typeDefaults);

  if (testing) {
    (void) strcpy(type_path, "./sd/duaconfig/");
    (void) strcat(type_path, type_dir);
    if (!(config_directory = opendir(type_path))) {
      (void) fprintf(stderr, "File error! Test sd should be run from source directory!\n");
      int_quit(1);
    }
  } else {
    if (!(config_directory = opendir(type_path))) {
      (void) strcpy(type_path, isodefile(isode_config_dir,0));
      (void) strcat(type_path, type_dir);
      if(!(config_directory = opendir(type_path))) {
	quit("Can't find directory filterTypes.\n", 1);
      }
    }
  }

  rewinddir(config_directory);
  filt_num = 0;
  while(dir_ent = readdir(config_directory)) {
    if (!(strncmp(dir_ent->d_name, "Type_", 5))) {
      file_names[filt_num] = 
	(char *) malloc((unsigned)
			(strlen(dir_ent->d_name) + strlen(type_path) + 2));
      (void) strcpy(file_names[filt_num], type_path);
      (void) strcat(file_names[filt_num], dir_ent->d_name);
      filt_num++;
    }
  }
  (void) closedir(config_directory);
  
  if ((config_file = fopen (user_home, "r")) == 0);
  else {
    while (fgets (Read_in_Stuff, STRINGLEN, config_file) != 0) {
      p = SkipSpace (Read_in_Stuff);
      if (( *p == '#') || (*p == '\0'))
	continue;  /* ignore comments and blanks */
      
      part1 = p;
      if ((part2 = index (p,':')) == NULLCP) 
	continue; /* ignore it */
      
      *part2++ = '\0';
      part2 = TidyString(part2);
      
      if ((lexequ(part1, "username") == 0) && namestr[0] == '\0') 
	(void) strcpy (namestr, part2);
      else if ((lexequ(part1, "password") == 0) && passwd[0] == '\0') 
	(void) strcpy (passwd, part2);
      else if (lexequ(part1, "prefergreybook") == 0)
	mailformat = greybook;
      else if (lexequ(part1, "dsap") == 0)
	(void) tai_string (part2);
      else if (lexequ(part1, "isode") == 0) {
	char * split;
	if ((split = index(part2,' ')) != NULLCP) {
	  *split++ = 0;
	(void) isodesetvar(part2,split,0);
	}
      } else if(strcmp (part1, "service") == 0) 
	new_service (part2);
      else if (lexequ(part1, "history") == 0) {
	(void) sscanf(part2, "%d", &histlimit);
	if (histlimit < 1) histlimit = 1;
      }
    }
    isodexport ();
    (void) fclose(config_file);
  }

  if (testing) {
    (void) strcpy(read_path, "./sd/duaconfig/");
    (void) strcat(read_path, readTypes);
    if (!(config_file = fopen(read_path, "r"))) {
      (void) fprintf(stderr, "File error! Test sd must be run from source directory!\n");
      int_quit(1);
    }
  } else {
    if (!(config_file = fopen(read_path, "r"))) {
      (void) strcpy(read_path, isodefile(isode_config_dir,0));
      (void) strcat(read_path, readTypes);
      if (!(config_file = fopen(read_path, "r"))) {
	quit("Can't find file readTypes.\n", 1);
      }
    }
  }

/*  load_oid_table("oidtable");*/
  
  while(fgets(Read_in_Stuff, STRINGLEN, config_file) != 0) {
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
  (void) fclose(config_file);
  
  for (curr_filt = 0; curr_filt < filt_num; curr_filt++) {
    if (!(config_file = fopen(file_names[curr_filt], "r"))) {
      quit("Can't find filter template file\n", 1);
    }
    filtvalue[curr_filt] = (char *) malloc(STRINGLEN);
    *filtvalue[curr_filt] = '\0';
    
    (void) yyparse();
    (void) fclose(config_file);
  }
  filttype[curr_filt] = NULLCP;
  for (count = 0; count < filt_num; count++)
    free(file_names[count]);

  if (testing) {
    (void) strcpy(type_defaults_path, "./sd/duaconfig/");
    (void) strcat(type_defaults_path, typeDefaults);
    if (!(config_file = fopen(type_defaults_path, "r"))) {
      (void) fprintf(stderr, "File error! Test sd must be run from source directory.\n");
      int_quit(1);
    }
  } else {
    if (!(config_file = fopen(type_defaults_path, "r"))) {
      (void) strcpy(type_defaults_path, isodefile(isode_config_dir,0));
      (void) strcat(type_defaults_path, typeDefaults);
      if (!(config_file = fopen(type_defaults_path, "r"))) {
	(void) strcpy(type_defaults_path, "./sd/duaconfig/");
	(void) strcat(type_defaults_path, typeDefaults);
	if (!(config_file = fopen(type_defaults_path, "r"))) {
	  (void) fprintf(stderr, "Can't open typeDefaults file\n");
        int_quit(1);
	}
      }
    }
  }

  default_num = 0;
  while (fgets (Read_in_Stuff, STRINGLEN, config_file) != 0) {
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
	int_quit(1);
      } else {
        tempints[count] = n;
        count++;
        part2 = end;
        while (!isalpha(*part2) && *part2 != ':' && part2 != '\0') part2++;

        if (*part2 == '\0') {
          (void) fprintf(stderr, "Parsing error in typeDefaults file!");
          int_quit(1);
        }

        if (*part2 != ':') {
          while (!isalpha(*part2)) part2++;
          end = part2;
          while (!isspace(*end) && *end != ',' &&
                 *end != ':' && *end != '\0') end++;
          if (*end == '\0') {
            (void) fprintf(stderr, "Parsing error in typeDefaults file!");
            int_quit(1);
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
        int_quit(1);
      } else {
        num = 0;
        while (num < count && n != tempints[num]) num++;
        if (num == count) {
          (void) fprintf(stderr, "Parsing error in typeDefaults file!");
          int_quit(1);
        }
      }

      defaults[default_num] = n;

      levels[default_num] = strdup(part1);
      available_types[default_num] = (int *) malloc((unsigned) 
						    sizeof(int) * (count+1));

      for (n = 0; n < count; n++)
        available_types[default_num][n] = tempints[n];

      available_types[default_num][n] = -1;
      default_num++;
    }
  }
  (void) fclose(config_file);
}

void main_help()
{
  cleartext();
  killwidgets(mainwdgts);
  setwidgets(dethelpwdgts,-1);
  help_cncs();
}

void main_bind()
{
  cleartext();
  if (*passwd != 0)
    (void) strcpy(bindpass,"******");
  else
    bindpass[0] = '\0';
}

void cnnct_quit ()
{
  quit("Exiting sd.\n", 0);
}

void cnnct_bind()
{
  struct ds_bind_arg bindarg;
  struct ds_bind_arg bindresult;
  struct ds_bind_error binderr;
  extern  char * dsa_address,
  	       * myname;
  extern  char * tailfile;
  FILE * fp;
  char buf [BUFSIZ];

  /* set dsa_address */
  dsa_address = NULLCP;
  /* read tailor file to get address */
  if( (fp = fopen(isodefile(tailfile,0), "r")) == (FILE *)NULL) {
    tprint ("Cannot open tailor file %s\n",isodefile(tailfile,0));
    return;
  }
  while(fgets(buf, sizeof(buf), fp) != NULLCP)
    if ( (*buf != '#') && (*buf != '\n') )
      (void) tai_string(buf);
  
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
    bindarg.dba_passwd_len = strlen(passwd);
    (void) strcpy (bindarg.dba_passwd, passwd);
  }
  
  bindarg.dba_dn = (*namestr == 0? NULLDN: str2dn(namestr));

  if (ds_bind (&bindarg,&binderr,&bindresult) != DS_OK) {
    if (binderr.dbe_type == DBE_TYPE_SECURITY)
      quit("Bind security error - Check name and pasword.\n", 0);
    else 
      quit("Bind service error - Can't contact DSA!\n", 1);
  } else {
    setdialogstr(getwidget(mainwdgts, '\0'), friendly_base_path, STRINGLEN);
    setdialogstr(getwidget(mainwdgts, 's'), mvalue, STRINGLEN);
    setdialogstr(getwidget(mainwdgts, '*'), srchvalue, 6);
    settogglstrs(getwidget(mainwdgts, 't'), filttype, 0);
    setwidgets (mainwdgts,-1);
    user_name = bindarg.dba_dn;

    if(local_dit && *local_dit)
      (void) strcpy(base_path, local_dit);
    else 
      (void) strcpy(base_path, "The World");

    oclass = as_comp_new(AttrT_new("objectClass"), NULLAV, NULLACL_INFO);
    make_friendly(friendly_base_path, base_path);
    printdialog(getwidget(mainwdgts,'\0'));
    set_default_type();

    (void) strcpy (buf, "TERM");

#ifndef NO_STATS
    LLOG (log_stat,LLOG_NOTICE,("bound ('%s' to '%s')",namestr,dsa_address));
#endif
#ifndef NO_STATS
    LLOG (log_stat,LLOG_NOTICE,("sd called from a ('%s')", getenv(buf)));
#endif
  }
  display_entry = current_entry = 1;
  entry_number = 0;
  back_buf_num = 0;
  textseq = back_seq = curr_dnseq = NULLDS;
  text_state = TEXT;
}

void rd_start()
{
  struct ds_read_arg read_arg;
  struct ds_read_result   result;
  struct DSError          error;
  Entry read_entry;
  
  cleartext();
  if (*friendly_base_path == 'T') {
    free_seq(textseq);
    if (text_state == DN_LIST) free_seq(curr_dnseq);
    curr_dnseq = textseq = NULLDS;
    text_state = TEXT;
    entry_number  = 0;
    return;
  }

  xprint("Reading data on ");
  xprint(friendly_base_path);
  xprint(".\n");
  tprint("Working. Please wait.....");

  if ( get_default_service (&read_arg.rda_common) != 0) {
    xprint ("Default service error -> check your .quipurc\n");
    return ;
  }

  read_arg.rda_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
  read_arg.rda_eis.eis_allattributes = FALSE;
  read_arg.rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
  read_arg.rda_eis.eis_select = read_types;

  read_arg.rda_object = (*friendly_base_path != 'T'? 
			 str2dn(base_path): 
			 NULLDN);
  if ((read_entry = local_find_entry(read_arg.rda_object, FALSE)) 
      != NULLENTRY &&
      read_entry->e_data != E_TYPE_CONSTRUCTOR) {
    read_entry->e_attributes = sort_attrs(read_entry->e_attributes);
    read_print (as_print, (caddr_t) read_entry->e_attributes);
    return;
  }

#ifndef NO_STATS
  LLOG (log_stat,LLOG_NOTICE,("read +%s",base_path));
#endif

  if (ds_read (&read_arg,&error,&result) != DS_OK) {
    /* deal with error */
    cleartext();
    quipu_error(&error);
  } else {
    /* use data */
    if (result.rdr_entry.ent_attr == NULLATTR) {
      free_seq(textseq);
      if (text_state == DN_LIST) free_seq(curr_dnseq);
      curr_dnseq = textseq = NULLDS;
      entry_number = 0;
      text_state = TEXT;
      xprint("No attributes");
      return;
    }
    xprint("Result of look up ...\n");
    if (result.rdr_common.cr_aliasdereferenced)
      xprint("Alias dereferenced)\n");
    
    result.rdr_entry.ent_attr = sort_attrs(result.rdr_entry.ent_attr);
    cache_entry(&(result.rdr_entry), FALSE, TRUE);
    read_print(as_print, (caddr_t) result.rdr_entry.ent_attr);
  }
}

void back_start()
{
  if (!back_buf_num) {
    cleartext();
    tprint("History Buffer Empty!\n");
    sleep(3);
    scrollbar('\0');
    return;
  }

  if (text_state == DN_LIST) free_seq(curr_dnseq);

  free_seq(textseq);
  textseq = NULLDS;
  curr_dnseq = back_seq;
  text_state = BACK_LIST;
  entry_number = back_buf_num;
  current_entry = display_entry = 1;
  scrollbar('\0');
}

void widen()
{
  register char *str, *sptr;
  int count = 0;
  str_seq first;

  str = get_from_seq(count+1, back_seq);
  while (count < back_buf_num && strcmp(str, base_path)){
    count++;
    str = get_from_seq(count+1, back_seq);
  }	
  if (count == back_buf_num) {
    add_seq(&back_seq, base_path);
    if (++back_buf_num > histlimit) {
      first = back_seq;
      back_seq = back_seq->next;
      first->next = 0;
      free_seq(first);
    }
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
    set_default_type();
    printdialog(getwidget(mainwdgts, '\0'));
    rd_start();
  }
}

void set_default_type()
{
  int count, lastindx;
  WIDGET *wdgt, *vwdgt;
  DN base_name;

  wdgt = getwidget(mainwdgts,'t');
  vwdgt = getwidget(mainwdgts, 's');

  if (av_typeindx)
    lastindx = wdgt->tindx;
  else
    lastindx = 0;

  if (*base_path != 'T') {
    if (!typetoggled) {
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
      count = typeindx;

      if (count < filt_num) {
	if (lastindx)
	  (void) strcpy(filtvalue[lastindx], vwdgt->dstr);
	wdgt->tindx = count;
	(void) strcpy(vwdgt->dstr, filtvalue[wdgt->tindx]);
      }
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
    count = typeindx;
    if (count < filt_num) {
      if (lastindx)
	(void) strcpy(filtvalue[lastindx], vwdgt->dstr);
      wdgt->tindx = count;
      (void) strcpy(vwdgt->dstr, filtvalue[wdgt->tindx]);
    }
  }

  make_friendly(friendly_base_path, base_path);
  typetoggled = 0;
  printtoggle(wdgt);
  printdialog(vwdgt);
}

/* These are the functions called by the list level widgets */

void list_start()
{
  struct ds_search_arg search_arg;
  struct ds_search_result result;
  struct DSError          error;

  cleartext();
  xprint("OK, listing.\n");
  tprint("Working. Please wait.....");

  if (get_default_service (&search_arg.sra_common) != 0) {
    xprint ("Default service error -> check your .quipurc\n");
    return ;
  }
  
  search_arg.sra_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
  search_arg.sra_baseobject = (*base_path != 'T'?
			       str2dn (base_path):
			       NULLDN);

  search_arg.sra_eis.eis_allattributes = FALSE;
  search_arg.sra_eis.eis_infotypes = EIS_ATTRIBUTETYPESONLY;
  search_arg.sra_eis.eis_select = 0;
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

  if (search_arg.sra_filter->flt_un.flt_un_filter->flt_un.flt_un_item.
      fi_un.fi_un_ava.ava_value == NULLAttrV) {
    cleartext();
    xprint("Internal Error! Sorry.");
  } else if (ds_search (&search_arg,&error,&result) != DS_OK) {
    /* deal with error */
    free_seq(textseq);
    if (text_state == DN_LIST) free_seq(curr_dnseq);
    curr_dnseq = textseq = NULLDS;
    text_state = TEXT;
    entry_number = 0;
    cleartext();
    xprint("Search error!\n");
    quipu_error(&error);
  } else {
    cleartext();
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

      if (text_state == DN_LIST) free_seq(curr_dnseq);
      free_seq(textseq);
      curr_dnseq = textseq = NULLDS;
      display_entry = current_entry = 1;
      entry_number = 0;
      text_state = DN_LIST;

      for (ptr = result.CSR_entries; ptr != NULLENTRYINFO;
	   ptr = ptr->ent_next) {
	entry_number++;
	dn2buf ((caddr_t)ptr->ent_dn, goto_path);
	add_seq (&curr_dnseq, goto_path);
      }

      if (entry_number) curr_dnseq = SortList(curr_dnseq);
      scrollbar('\0');
    } else if (result.CSR_limitproblem != LSR_TIMELIMITEXCEEDED) {
      free_seq(textseq);
      if (text_state == DN_LIST) free_seq(curr_dnseq);
      curr_dnseq = textseq = NULLDS;
      entry_number  = 0;
      text_state = TEXT;
      xprint("Nothing found<");
    }

    if(result.CSR_limitproblem != LSR_NOLIMITPROBLEM) {
      switch (result.CSR_limitproblem) {
      case LSR_TIMELIMITEXCEEDED:
        if (entry_number > 0) 
	  xprint("(Time limit exceeded. Partial results only.)");
        else {
          free_seq(curr_dnseq);
          curr_dnseq = NULLDS;
	  xprint("(Time limit exceeded.)");
        }
        break;
      case LSR_SIZELIMITEXCEEDED:
	xprint("(Size limit exceeded)");
        break;
      case LSR_ADMINSIZEEXCEEDED:
        if (entry_number > 0) 
	  xprint("(Administrative limit reached. Partial results only.)");
        else {
          free_seq(curr_dnseq);
          curr_dnseq = NULLDS;
	  xprint("(Administrative limit reached.)");
        }
        break;
      }
    }

    entryinfo_free(result.CSR_entries, 0);
  }

  filter_free(search_arg.sra_filter);
}

void rdn2str(ptr,cptr)
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

void srch_start()
{
  struct ds_search_arg search_arg;
  struct ds_search_result result;
  struct DSError          error;
  WIDGET *wdgt;
  DN curr_rdn;

  if (*mvalue == '\0') {
    list_start();
    return;
  }
  
  cleartext();
  xprint("OK. Starting search.\n");
  tprint("Working. Please wait.....");

  wdgt = getwidget(mainwdgts,'t');
  
  if (get_default_service (&search_arg.sra_common) != 0) {
    xprint ("Default service error -> check your .quipurc\n");
    return ;
  }
  
  search_arg.sra_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
  curr_rdn = search_arg.sra_baseobject = (*base_path != 'T'? 
					  str2dn (base_path):
					  NULLDN);

  search_arg.sra_eis.eis_allattributes = FALSE;
  search_arg.sra_eis.eis_infotypes = EIS_ATTRIBUTETYPESONLY;
  search_arg.sra_eis.eis_select = 0;
  
  search_arg.sra_subset = SRA_ONELEVEL;
  while (curr_rdn != NULLDN) {
    if (!strcmp(curr_rdn->dn_rdn->rdn_at->oa_ot.ot_stroid,
		"2.5.4.10")) {
      search_arg.sra_subset = SRA_WHOLESUBTREE;
      break;
    }
    curr_rdn = curr_rdn->dn_parent;
  }

  if ((search_arg.sra_filter = make_filter(filt_arr[gettogglindx(wdgt)])) 
      == NULLFILTER) {
    (void) xprint("Invalid search filter!");
    return;
  }

#ifndef NO_STATS
  LLOG (log_stat,LLOG_NOTICE,("search +%s, extent %d, val %s", 
			      base_path, search_arg.sra_subset, mvalue));
#endif

  if(ds_search (&search_arg,&error,&result) != DS_OK) {
    /* deal with error */
    free_seq(textseq);
    if (text_state == DN_LIST) free_seq(curr_dnseq);
    curr_dnseq = textseq = NULLDS;
    entry_number = 0;
    text_state = TEXT;
    cleartext();
    xprint(" Search error!\n ");
    quipu_error(&error);
  } else {
    cleartext();
    correlate_search_results (&result);

    if (result.CSR_entries != NULLENTRYINFO) {
      register EntryInfo *ptr;

      if (result.CSR_common.cr_aliasdereferenced) {
	xprint (" (Alias dereferenced - object is ");
	quipu_print (dn_print, (caddr_t) result.CSR_object);
	dn_free (result.CSR_object);
	xprint (")\n");
      }

      if (text_state == DN_LIST) free_seq(curr_dnseq);
      free_seq(textseq);
      curr_dnseq = textseq = NULLDS;
      display_entry = current_entry = 1;
      entry_number = 0;
      text_state = DN_LIST;

      for (ptr = result.CSR_entries; 
	   ptr != NULLENTRYINFO; ptr = ptr->ent_next){
	entry_number++;
	dn2buf ((caddr_t) ptr->ent_dn, goto_path);
	add_seq (&curr_dnseq, goto_path);
      }

      if (entry_number > 1) curr_dnseq = SortList(curr_dnseq);

      if (entry_number != 1)
	scrollbar('\0');
      else {
	if  (!isleafnode(goto_path)) {
	  get_listed_object('1', (WIDGET *) 0);
	} else {
	  char temp[1024];
	  (void) strcpy(temp, base_path);
	  (void) strcpy(base_path, goto_path);
	  rd_start();
	  (void) strcpy(base_path, temp);
	}
      }
    } else if(result.CSR_limitproblem != LSR_TIMELIMITEXCEEDED) {
      free_seq(textseq);
      if (text_state == DN_LIST) free_seq(curr_dnseq);
      curr_dnseq = textseq = NULLDS;
      entry_number = 0;
      text_state = TEXT;
      xprint("Nothing found using current search parameters.\n");
    }
    
    if(result.CSR_limitproblem != LSR_NOLIMITPROBLEM)
      switch (result.CSR_limitproblem) {
      case LSR_TIMELIMITEXCEEDED:
        if (entry_number > 0) 
	  xprint("(Time limit exceeded. Partial results only.)");
        else {
          free_seq(curr_dnseq);
          curr_dnseq = NULLDS;
	  xprint("(Time limit exceeded.)");
        }
        break;
      case LSR_SIZELIMITEXCEEDED:
	xprint("(Size limit exceeded)");
        break;
      case LSR_ADMINSIZEEXCEEDED:
        if (entry_number > 0) 
	  xprint("(Administrative limit reached. Partial results only.)");
        else {
          free_seq(curr_dnseq);
          curr_dnseq = NULLDS;
	  xprint("(Administrative limit reached.)");
        }
        break;
      }    
    entryinfo_free(result.CSR_entries, 0);
  }

  filter_free(search_arg.sra_filter);
}

void dn2buf (ptr,cptr)
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


void read_print(func,ptr)
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
  
  if (text_state == DN_LIST) free_seq(curr_dnseq);
  curr_dnseq = NULLDS;
  free_seq(textseq);
  textseq = NULLDS;
  display_entry = current_entry = 1;
  entry_number = 0;
  text_state = TEXT;
  
  for (i = 0; i <= size; i++, sptr++)
    if (*sptr == '\n' || *sptr == '\0') {
      entry_number++;
      save = *sptr ;
      *sptr = '\0';
      
      if (mailformat == greybook && indexstring(str, "rfc822") >= 0)
        (void) rfc2jnt(str);
      add_seq(&textseq, str);
      
      str = sptr+1;
      *sptr = save;
    }
  
  scrollbar('\0');
}

void quipu_print(func,ptr)
     int (*func)();         /* assumes func (PS ,dataptr,(int) format); */
     caddr_t ptr;
{
  PS ps;
  char buffer [RESBUF];
  int count;
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
    for (count = 0, sptr = str; *sptr != '\0' &&
	 count < 100; sptr++, count++);
    save = *sptr;
    *sptr = 0;
    xprint (str);
    *sptr = save;
    str = sptr;
  } while (*sptr != '\0');
}

void quipu_error(err)
     struct DSError *err;
{
  switch(err->dse_type) {
  case DSE_LOCALERROR:
    xprint(" SD internal error!\n");
    break;
  case DSE_REMOTEERROR:
    xprint(" Problem with remote directory server!\n");
    break;
  case DSE_ATTRIBUTEERROR:
    xprint(" Faulty data found in database!\n");
    break;
  case DSE_REFERRAL:
  case DSE_DSAREFERRAL:
    xprint(" Requested data unavailable at present.\n");
    break;
  case DSE_SECURITYERROR:
    xprint(" You do not have the privileges required\n to make this request!");
    break;
  case DSE_NAMEERROR:
    xprint(" Invalid directory position!\n");
    break;
  case DSE_SERVICEERROR:
    xprint(" Directory Service Error!\n");
    break;
  default:
    xprint(" Requested data unavailable at present.\n");
  }
}

void returnmain()
{
  QUITFN();
  setwidgets (mainwdgts,-1);
  rd_start();
  scrollbar('\0');
}

void get_listed_object(number, wdgt)
     char number;
     WIDGET *wdgt;
{
  int entrynum, count = 0;
  char *sptr, *str;
  char buffer[1024];
  
  if (text_state != DN_LIST && text_state != BACK_LIST) {
    *wdgt->dstr = '\0';
    printdialog(wdgt);
    return;
  }
  
  *srchvalue = number;
  *(srchvalue+1) = '\0';

  if (wdgt) {
    printdialog(wdgt);
    dialog(wdgt);
  }
  
  entrynum = atoi(srchvalue);

  if (entrynum > entry_number) {
    *wdgt->dstr = '\0';
    printdialog(wdgt);
    return;
  }

  if (sptr = get_from_seq (entrynum, curr_dnseq)) 
    if(!isleafnode(sptr)) {
      cleartext();
      str = get_from_seq(count+1, back_seq);
      
      while (str && count < back_buf_num && strcmp(str, base_path)){
	count++;
	str = get_from_seq(count+1, back_seq);
      }
      
      if (count == back_buf_num) {
	add_seq(&back_seq, base_path);
	back_buf_num++;
      }
      
      (void) strcpy(base_path, sptr);
      make_friendly(friendly_base_path, base_path);
      wdgt = getwidget(mainwdgts, '\0');
      printdialog(wdgt);
      rd_start();
      typetoggled = 0;
      set_default_type();
    } else {
      (void) strcpy(buffer, base_path);
      (void) strcpy(base_path, sptr);
      make_friendly(friendly_base_path, base_path);
      rd_start();
      (void) strcpy(base_path, buffer);
    }
  *srchvalue = '\0';
}

void scrollbar(command)
     char command;
{
  register char *str;
  char *base_rdn;
  register int rdn_count = 0;
  int lines, count = 0;
  str_seq thisseq;
  
  if (!entry_number)
    return;
  
  if(command == '[') {
    if(display_entry >= entry_number)
      return;

    for (count = 0; (display_entry + count) < entry_number && 
	 count < text_height/2; count++);

    current_entry += count;
  } else if(command == ']') {

    for (count = 0; (current_entry - count) > 1 && 
	 count < text_height/2; count++);

    current_entry -= count;
  }
  
  cleartext();
  
  switch(text_state) {

  case BACK_LIST:
    thisseq = back_seq;
    break;

  case DN_LIST:
    base_rdn = base_path;
    while(*base_rdn != '\0') {
      if(*base_rdn == '@')
	rdn_count++;
      base_rdn++;
    }
    if (*base_path != 'T') rdn_count++;
    thisseq = curr_dnseq;
    break;

  case TEXT:
    thisseq = textseq;
    break;
  }
  
  if (current_entry > entry_number) 
    current_entry = 1;
  
  lines = linec()-2;
  count = 0;
  
  for (display_entry = current_entry; display_entry <= entry_number 
       && gety() < lines; display_entry++) {

    if (text_state == DN_LIST || text_state == BACK_LIST)
      xprintint(" %d ", display_entry) ;
    
    base_rdn = str = get_from_seq(display_entry, thisseq);

    if (text_state == DN_LIST && rdn_count) {
      while (rdn_count) {
	if (*str == '@') rdn_count--;
	str++;
      }

      while(*str == ' ') str++;
      count = (int) (str - base_rdn);

    } else if (count) str += count;
    
    if (str) {
      if (text_state != TEXT) {
	make_friendly(friendly_name, str);
	xprint(friendly_name);
      } else
	xprint(str);
      xprint("\n");
    }
  }

  if (text_state == DN_LIST)
    if (current_entry >= entry_number) xprint("<List finished>");
    else        xprintint("<Total of %d entries>", entry_number);

  printbar(entry_number, current_entry, display_entry-current_entry);
  display_entry--;
  return;
}

void make_friendly(fstr, str)
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
   
void goto_addr()
{
  set_default_type();
  rd_start();
}

int isleafnode(name)
     char *name;
{
  struct ds_list_arg list_arg;
  struct ds_list_result   list_result;

  struct ds_read_arg read_arg;
  struct ds_read_result   read_result;

  struct DSError          list_error, read_error;
  char entry_str[1024];

  if (get_default_service (&read_arg.rda_common) != 0) return(1);

  read_arg.rda_common.ca_servicecontrol.svc_options = 1;
  read_arg.rda_eis.eis_allattributes = FALSE;
  read_arg.rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
  read_arg.rda_eis.eis_select = oclass;

  read_arg.rda_object = (*name? str2dn(name): NULLDN);
  if (ds_read (&read_arg,&read_error,&read_result) != DS_OK) return(1);
  else {
    if (read_result.rdr_entry.ent_attr == NULLATTR) return(0);
    entry2str((caddr_t) read_result.rdr_entry.ent_attr, entry_str, 1024);
    if (issubstr(entry_str, "NonLeaf")) return(0);
  }
  if (get_default_service (&list_arg.lsa_common) != 0) return(1);

  list_arg.lsa_common.ca_servicecontrol.svc_sizelimit = 1;
  list_arg.lsa_common.ca_servicecontrol.svc_options = 1;

  if (*name) 
    list_arg.lsa_object = str2dn(name);
  else
    list_arg.lsa_object = NULLDN;

  if (ds_list (&list_arg,&list_error,&list_result) != DS_OK) 
    return (1);
  else {
    if (list_result.lsr_subordinates == NULLSUBORD) return(1);
    else return(0);
  }
}

void entry2str(ptr, cptr, size)
     caddr_t ptr;
     char *cptr;
     int size;
{
  PS ps;

  if ((ps = ps_alloc (str_open)) == NULLPS) return ;
  if (str_setup (ps, cptr, size, 1) == NOTOK) return ;

  as_print(ps, (Attr_Sequence) ptr, READOUT);
  *ps->ps_ptr = 0;

  ps_free(ps);
}

int issubstr(str, substr)
     char *str;
     char *substr;
{
  register char *sptr;
  char c;
  int substrlen = strlen(substr);
  int count;

  if (*substr == '\0' || *str == '\0') return(0);

  sptr = str;
  c = *substr;

  while (1) {
    while (*sptr != '\0' && *sptr != c) sptr++;
    if (*sptr == '\0') return(0);
    for (count = 0; count >= 0 && count < substrlen; count++) {
      if (sptr[count] == '\0') return(0);
      else if (substr[count] != sptr[count]) count = -2;
    }
    if (count == substrlen) return(1);
    sptr++;
  }
}

int indexstring(string, substring)
     char *string, *substring;
{
  register char *sub, *str;
  char c, s;
  int indx = 0;

  while (1) {
    str = string + indx;;
    if (*str == '\0') return(-1);
    sub = substring;

    if (*str == *sub) {
      s = *str;
      c = *sub;
      while(c == s && c != '\0') {
        c = *++sub;
        s = *++str;
      }
      if (c == '\0') return((int) indx);
      else if(s == '\0') return(-1);
    }
    indx++;
  }
}

void rfc2jnt(string)
     char *string;
{
  char reversed[STRINGLEN];
  char front[STRINGLEN];
  register char *part;
  char *mailbox;

  mailbox = string;
  while (*mailbox != '-') mailbox++;

  reversed[0] = '\0';
  part = string + strlen(string);

  if (*part != '\0') return;
  
  while(1) {
    while (*part != '.' && *part != '@') --part;

    if (*part == '.') {
      if (reversed[0] != '\0') (void) strcat(reversed, ".");
      part++;
      (void) strcat(reversed, part);
      *--part = '\0';
      --part;
    } else {
      part++;
      (void) strcat(reversed, ".");
      (void) strcat(reversed, part);
      *part-- = '\0';
      while (!isspace(*part)) --part;
      ++part;
      (void) strcpy(front, part);
      (void) strcpy(string, "mailbox               - ");
      (void) strcat(string, front);
      (void) strcat(string, reversed);
      return;
    }
  }
}
  
struct attrcomp *sort_attrs(entry_attrs)
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


char *GetSurname(name)
     register char *name;
{
  while (*name != '\0') name++;
  while (*name != ' ' && *name != '=') name--;
  return ++name;
}

char *GetWholeRelName(name)
     register char *name;
{
  while (*name!= '\0') name++;
  while (*name != '=') name--;
  while (!isalpha(*name)) name++;
  return name;
}

str_seq SortList(list)
     str_seq list;
{
  register str_seq currEntry, lastSortedEntry , currSortedEntry; 
  str_seq sortedList;
  char *sortedName, *currName;
  register DN curr_dn;
  DN dn;

  if (!list) return 0;

  currEntry = list;
  sortedList = 0;

  while (currEntry) {
    dn = curr_dn = str2dn(currEntry->dname);

    while (curr_dn && curr_dn->dn_parent != NULLDN) 
      curr_dn = curr_dn->dn_parent;
    
    if (!strcmp(curr_dn->dn_rdn->rdn_at->oa_ot.ot_stroid, "2.5.4.3"))
      currName = GetSurname(currEntry->dname);
    else 
      currName = GetWholeRelName(currEntry->dname);

    dn_free(dn);

    if (!sortedList) {
      sortedList = currEntry;
      currEntry = currEntry->next;
      sortedList->next = 0;
    } else {
      lastSortedEntry = 0;
      currSortedEntry = sortedList;

      while (currSortedEntry != 0) {
	dn = curr_dn = str2dn(currSortedEntry->dname);
	
	while (curr_dn && curr_dn->dn_parent != NULLDN) 
	  curr_dn = curr_dn->dn_parent;
	
	if (!strcmp(curr_dn->dn_rdn->rdn_at->oa_ot.ot_stroid, 
		    "2.5.4.3"))
	  sortedName = GetSurname(currSortedEntry->dname);
	else 
	  sortedName = GetWholeRelName(currSortedEntry->dname);
	
	dn_free(dn);

	if (strcmp(currName, sortedName) <= 0) {
	  if (lastSortedEntry) {
	    lastSortedEntry->next = currEntry;
	    currEntry = currEntry->next;
	    lastSortedEntry->next->next = currSortedEntry;
	  } else {
	    sortedList = currEntry;
	    currEntry = currEntry->next;
	    sortedList->next = currSortedEntry;
	  }
	  currSortedEntry = 0;
	} else {
	  lastSortedEntry = currSortedEntry;
	  currSortedEntry = currSortedEntry->next;
	  if (!currSortedEntry) {
	    lastSortedEntry->next = currEntry;
	    currEntry = currEntry->next;
	    lastSortedEntry->next->next = 0;
	    currSortedEntry = 0;
	  }
	}
      }
    }
  }
  return sortedList;
}


