#include "quipu/util.h"
#include "quipu/attrvalue.h"

extern LLog * log_dsap;
extern char dsa_mode;

int file_attr_length;

static char *
readfile (file)
FILE * file;
{
char *parse_buffer;
int parse_len;
char	*buf;
int	buflen;
int	curlen;
int	size;
	
    curlen = 0;
    buflen = parse_len = LINESIZE;
    buf = parse_buffer = smalloc(LINESIZE);

    for (;;) {
	if ( buflen <= 100) {
		parse_len += LINESIZE;
		buflen += LINESIZE;
		if ((parse_buffer = realloc(parse_buffer, (unsigned)parse_len)) == NULLCP)
		    exit (2);	/* ??? */
		buf = parse_buffer + curlen;
	}

	if (fgets (buf, buflen,file) == NULLCP) {
	    file_attr_length = curlen;
	    return (parse_buffer);
        }

	size = strlen(buf);
	if (buf[size - 1] == '\n') {
	    buf[--size] = '\0';
	}
	buf += size;
	buflen -= size;
	curlen += size;

    }

    /* NOTREACHED */
}


PE grab_filepe (av)
AttributeValue av;
{
FILE * fptr;
struct file_syntax * fs;
sntx_table *tbl, * get_syntax_table();
PE ret_pe = NULLPE, grab_pe();

	fs = (struct file_syntax *) av->av_struct;

	if (fs->fs_attr != NULLAttrV)
		return (grab_pe (fs->fs_attr));

	if (fs->fs_name == NULLCP) 
		goto out;	/* should never happen */

	if ((fptr = fopen (fs->fs_name,"r")) != NULL) {
		tbl = get_syntax_table (fs->fs_real_syntax);
		if (tbl->s_parse == NULLIFP) { /* treat as pure asn */
			PS fps;
			fps = ps_alloc (std_open);
			if ((std_setup (fps,fptr)) == NOTOK) {
				(void) fclose (fptr);
				ps_free (fps);
				goto out;
			}
			ret_pe = ps2pe (fps);
			if (fps->ps_errno != PS_ERR_NONE) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("%s in attribute file '%s'",ps_error(fps->ps_errno),fs->fs_name));
				if (ret_pe) {
					pe_free (ret_pe);
					ret_pe = NULLPE;
				}
			} else if (ret_pe == NULLPE)
				LLOG (log_dsap,LLOG_EXCEPTIONS,("invalid ASN in file '%s'",fs->fs_name));
			ps_free (fps);
		} else {
			char * buffer;
			AttributeValue newav;

			buffer = readfile (fptr);
	
			if ((newav = str2AttrV (buffer,fs->fs_real_syntax)) == NULLAttrV){
				LLOG (log_dsap,LLOG_EXCEPTIONS,("invalid format in file %s",fs->fs_name));
			} else {
				ret_pe = grab_pe (newav);
				AttrV_free (newav);
			}
			file_attr_length = 0;
			free (buffer);
		}
		(void) fclose (fptr);
	} else 
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("Attribute file '%s' not found",fs->fs_name));

out:;
	if ((ret_pe == NULLPE) && dsa_mode) {
		LLOG(log_dsap,LLOG_NOTICE,("Error with file attribute '%s'",fs->fs_name));
		/* As we are a DSA, return the PE containing NULL 
		/* a NULLPE will cause the encode to fail, thus make the
		/* operation fail.
		/* Real solution is to remove the attribute at the encode 
		/* stage - but that is tricky... 
 		/* DUAs should fail.
		*/
		return (pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL));
	}
	return (ret_pe);

}


file_decode (x)
AttributeValue x;
{
struct file_syntax * fs;

	fs = (struct file_syntax *) smalloc (sizeof(struct file_syntax));
	fs->fs_ref = 1;
	fs->fs_real_syntax = x->av_syntax;
	if (x->av_syntax >= AV_WRITE_FILE)
		fs->fs_real_syntax -= AV_WRITE_FILE;
	fs->fs_name = NULLCP;
	fs->fs_mode = FS_DEFAULT;

	fs->fs_attr = AttrV_alloc ();
	fs->fs_attr->av_syntax = fs->fs_real_syntax;
	fs->fs_attr->av_struct = x->av_struct;
		
	x->av_syntax = AV_FILE;
	x->av_struct = (caddr_t)fs;
}
