/* nrs_info.c - nRSInformation attribute */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/*
	SYNTAX
	nrs_info ::= <context> "$" <addr_sp_id> "$" <routes>
	context ::= <integer> | <context_str>
	context_str ::= "x29" | "ts29" | "niftp" | "mail-niftp" | "mail-telex"
		| "jtmp" | "jtmp-files" | "jtmp-reg" | "ybts-node" | "ybts"
		| "ftam" | "jtm" | "jtm-reg" | "vt" | "motis"
	addr_sp_id ::= <integer> | <addr_sp_id_str>
	addr_sp_id_str ::= "pss" | "janet" | "telex" | "osi-cons"
	routes ::= <route> "|" <routes> | empty
	route ::= <cost> "#" <addressing_info>
	cost ::= <asn> | empty	-- empty implies cost is ASN.1 NULL
	addressing_info ::=
		  "dte_only" ":" <dte_number>
		| "dte_applic_info" ":" <dte_number> "#" <applic_info>
		| "dte_cudf" ":" <dte_number> "#" <cudf>
		| "dte_cudf_applic_info" ":" <dte_number> "#" <cudf> "#" <applic_info>
		| "dte_ybts" ":" <dte_number> "#" <ybts_string>
		| "dte_ybts_applic_info" ":" <dte_number> "#" <ybts_string> "#" <applic_info>
		| "dte_ybts_applic_relay" ":" <dte_number> "#" <ybts_string> "#" <applic_relay>
		| "none_needed" ":"
		| "osi_addressing" ":" <nsap> "#" <tsel> "#" <ssel> "#" <psel>
"#" <place_holder> "#" <application_title> "#" <per_app_context_info>
		| "osi_nsap_only" ":" <nsap>
		| "osi_nsap_applic_info" ":" <nsap> "#" <applic_info>
		| "osi_nsap_applic_relay" ":" <nsap> "#" <applic_relay>
		| "dte_ybts_osi_addressing" ":" <dte_number> "#" <ybts_string>
			"#" <tsel> "#" <ssel> "#" <psel> "#" <place_holder>
			"#" <application_title> "#" <per_app_context_info>

	dte_number ::= <numeric_string>
	cudf ::= <octet_string>
	ybts ::= <visible_string>
	nsap ::= <numeric_string>
	tselector ::= <octet_string>
	sselector ::= <octet_string>
	pselector ::= <octet_string>
	place_holder ::= <asn>
	application_title ::= <asn>
	per_app_context_info ::= <asn>
	applic_info ::= <vis_str_seq>
	applic_relay ::= <vis_str_seq>
	vis_str_seq ::= <visible_string> | <str_seq> "$" <visible_string>
	
	EXAMPLES
*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attr.h"		/* Def.s for READOUT etc */
#include "quipu/nrs_info.h"

extern LLog	* log_dsap;
PE		  asn2pe();

PE	  asnstr2pe (orig)
char	* orig;
{
	PE	  result;
	char	* str;

	if (orig == NULLCP)
		return(NULLPE);

	str = SkipSpace(orig);

	if ((*str) == '\0')
		return(NULLPE);

	if (strncmp (str, "{ASN}", 5) == 0)
	{
		result = asn2pe ((char *)(orig + 5));
	}
	else
	{
		parse_error ("Malformed ASN string '%s'", orig);
		result = NULLPE;
	}

	return(result);
}

static str_seq_free (arg)
struct str_seq	* arg;
{
	if (arg == (struct str_seq *)NULL)
		return;

	if (arg->ss_str)
		free (arg->ss_str);

	if (arg->ss_next)
		str_seq_free (arg->ss_next);

	free ((char *)arg);
}

static struct str_seq	* str_seq_cpy (arg)
struct str_seq	* arg;
{
	struct str_seq	* ret;

	if (arg == (struct str_seq *)NULL)
		return ((struct str_seq *)NULL);

	ret = (struct str_seq *) smalloc (sizeof (struct str_seq));

	ret->ss_str = strdup (arg->ss_str);

	ret->ss_next = str_seq_cpy (arg->ss_next);

	return (ret);
}

static str_seq_cmp (arg1, arg2)
struct str_seq	* arg1;
struct str_seq	* arg2;
{
	int	  ret;

	if (arg1 == (struct str_seq *)NULL)
		if (arg2 == (struct str_seq *)NULL)
			return (0);
		else
			return (-1);

	if (arg2 == (struct str_seq *)NULL)
		return (1);

	if ((ret = lexequ (arg1->ss_str, arg2->ss_str)) != 0)
		return (ret);

	return (str_seq_cmp (arg1->ss_next, arg2->ss_next));
}

static str_seq_print (ps, strseq, format)
register PS	  ps;
struct str_seq	* strseq;
int		  format;
{
	struct str_seq	* ss;

	for (ss=strseq; ss != (struct str_seq *)NULL; ss = ss->ss_next)
	{
		if (ss != strseq)
			if (format == READOUT)
				ps_printf (ps, "//");
			else
				ps_printf (ps, "$");

		ps_printf (ps, "%s", ss->ss_str);
	}
}

static struct str_seq	* str2str_seq (orig)
char * orig;
{
	struct str_seq	* result;
	struct str_seq	**ss;
	char		* ptr_prev;
	char		* ptr_next;

	if (((ptr_prev = orig) == NULLCP) || ((*ptr_prev) == '\0'))
	{
		return ((struct str_seq *)NULL);
	}

	ss = &(result);

	while ( (ptr_next=index (ptr_prev, '$')) != NULLCP)
	{
		*ptr_next = '\0';
		ptr_next++;
		(*ss) = (struct str_seq *) smalloc (sizeof (struct str_seq));

		(*ss)->ss_str = strdup (ptr_prev);

		ss = &((*ss)->ss_next);
		ptr_prev = ptr_next;
	}

	(*ss) = (struct str_seq *) smalloc (sizeof (struct str_seq));

	(*ss)->ss_str = strdup (ptr_prev);

	(*ss)->ss_next = (struct str_seq *)NULL;

	return (result);
}

static addr_info_free (arg)
struct addr_info	* arg;
{
	if (arg == (struct addr_info *)NULL)
		return;

	if (arg->dte_number)
		free (arg->dte_number);

	if (arg->cudf)
		free (arg->cudf);

	if (arg->ybts_string)
		free (arg->ybts_string);

	if (arg->nsap)
		free (arg->nsap);

	if (arg->tselector)
		free (arg->tselector);

	if (arg->sselector)
		free (arg->sselector);

	if (arg->pselector)
		free (arg->pselector);

	if (arg->place_holder)
		pe_free (arg->place_holder);

	if (arg->application_title)
		pe_free (arg->application_title);

	if (arg->per_app_context_info)
		pe_free (arg->per_app_context_info);

	if (arg->applic_info)
		str_seq_free (arg->applic_info);

	if (arg->applic_relay)
		str_seq_free (arg->applic_relay);

	free ((char *) arg);
}

static struct addr_info	* addr_info_cpy (arg)
struct addr_info	* arg;
{
	struct addr_info	* ret;

	if (arg == (struct addr_info *)NULL)
		return ((struct addr_info *)NULL);

	ret = (struct addr_info *) calloc (1, sizeof (struct addr_info));

	switch (ret->addr_info_type = arg->addr_info_type)
	{
	case ADDR_INFO_DTE_ONLY:
		ret->dte_number = strdup (arg->dte_number);
		break;
	case ADDR_INFO_DTE_APPLIC_INFO:
		ret->dte_number = strdup (arg->dte_number);
		ret->applic_info = str_seq_cpy (arg->applic_info);
		break;
	case ADDR_INFO_DTE_CUDF:
		ret->dte_number = strdup (arg->dte_number);
		ret->cudf = strdup (arg->cudf);
		break;
	case ADDR_INFO_DTE_CUDF_APPLIC_INFO:
		ret->dte_number = strdup (arg->dte_number);
		ret->cudf = strdup (arg->cudf);
		ret->applic_info = str_seq_cpy (arg->applic_info);
		break;
	case ADDR_INFO_DTE_YBTS:
		ret->dte_number = strdup (arg->dte_number);
		ret->ybts_string = strdup (arg->ybts_string);
		break;
	case ADDR_INFO_DTE_YBTS_APPLIC_INFO:
		ret->dte_number = strdup (arg->dte_number);
		ret->ybts_string = strdup (arg->ybts_string);
		ret->applic_info = str_seq_cpy (arg->applic_info);
		break;
	case ADDR_INFO_DTE_YBTS_APPLIC_RELAY:
		ret->dte_number = strdup (arg->dte_number);
		ret->ybts_string = strdup (arg->ybts_string);
		ret->applic_relay = str_seq_cpy (arg->applic_relay);
		break;
	case ADDR_INFO_NONE_NEEDED:
		break;
	case ADDR_INFO_OSI_ADDRESSING:
		ret->nsap = strdup (arg->nsap);
		ret->tselector = strdup (arg->tselector);
		ret->sselector = strdup (arg->sselector);
		ret->pselector = strdup (arg->pselector);

		if (arg->place_holder == NULLPE)
			ret->place_holder = NULLPE;
		else
			ret->place_holder = pe_cpy (arg->place_holder);

		if (arg->application_title == NULLPE)
			ret->application_title = NULLPE;
		else
			ret->application_title = pe_cpy (arg->application_title);

		if (arg->per_app_context_info == NULLPE)
			ret->per_app_context_info = NULLPE;
		else
			ret->per_app_context_info = pe_cpy (arg->per_app_context_info);

		break;
	case ADDR_INFO_OSI_NSAP_ONLY:
		ret->nsap = strdup (arg->nsap);
		break;
	case ADDR_INFO_OSI_NSAP_APPLIC_INFO:
		ret->nsap = strdup (arg->nsap);
		ret->applic_info = str_seq_cpy (arg->applic_info);
		break;
	case ADDR_INFO_OSI_NSAP_APPLIC_RELAY:
		ret->nsap = strdup (arg->nsap);
		ret->applic_relay = str_seq_cpy (arg->applic_relay);
		break;
	case ADDR_INFO_DTE_YBTS_OSI_ADDRESSING:
		ret->dte_number = strdup (arg->dte_number);
		ret->ybts_string = strdup (arg->ybts_string);
		ret->tselector = strdup (arg->tselector);
		ret->sselector = strdup (arg->sselector);
		ret->pselector = strdup (arg->pselector);

		if (arg->place_holder == NULLPE)
			ret->place_holder = NULLPE;
		else
			ret->place_holder = pe_cpy (arg->place_holder);

		if (arg->application_title == NULLPE)
			ret->application_title = NULLPE;
		else
			ret->application_title = pe_cpy (arg->application_title);

		if (arg->per_app_context_info == NULLPE)
			ret->per_app_context_info = NULLPE;
		else
			ret->per_app_context_info = pe_cpy (arg->per_app_context_info);

		break;
	default:
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("addr_info_cpy(): Unknown addressing info type %d", arg->addr_info_type));
		break;
	}

	return (ret);
}

static addr_info_cmp (arg1, arg2)
struct addr_info	* arg1;
struct addr_info	* arg2;
{
	int	  ret;

	if (arg1 == (struct addr_info *)NULL)
		if (arg2 == (struct addr_info *)NULL)
			return (0);
		else
			return (-1);

	if (arg2 == (struct addr_info *)NULL)
		return (1);

	if (arg1->addr_info_type < arg2->addr_info_type)
		return (-1);

	if (arg1->addr_info_type > arg2->addr_info_type)
		return (1);

	switch (arg1->addr_info_type)
	{
	case ADDR_INFO_DTE_ONLY:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_APPLIC_INFO:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = str_seq_cmp (arg1->applic_info, arg2->applic_info)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_CUDF:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->cudf, arg2->cudf)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_CUDF_APPLIC_INFO:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->cudf, arg2->cudf)) != 0)
			return (ret);
		if ((ret = str_seq_cmp (arg1->applic_info, arg2->applic_info)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_YBTS:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->ybts_string, arg2->ybts_string)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_YBTS_APPLIC_INFO:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->ybts_string, arg2->ybts_string)) != 0)
			return (ret);
		if ((ret = str_seq_cmp (arg1->applic_info, arg2->applic_info)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_YBTS_APPLIC_RELAY:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->ybts_string, arg2->ybts_string)) != 0)
			return (ret);
		if ((ret = str_seq_cmp (arg1->applic_relay, arg2->applic_relay)) != 0)
			return (ret);
		break;
	case ADDR_INFO_NONE_NEEDED:
		break;
	case ADDR_INFO_OSI_ADDRESSING:
		if ((ret = lexequ (arg1->nsap, arg2->nsap)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->tselector, arg2->tselector)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->sselector, arg2->sselector)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->pselector, arg2->pselector)) != 0)
			return (ret);
		if ((ret = quipu_pe_cmp (arg1->place_holder, arg2->place_holder)) != 0)
			return (ret);
		if ((ret = quipu_pe_cmp (arg1->application_title, arg2->application_title)) != 0)
			return (ret);
		if ((ret = quipu_pe_cmp (arg1->per_app_context_info, arg2->per_app_context_info)) != 0)
			return (ret);
		break;
	case ADDR_INFO_OSI_NSAP_ONLY:
		if ((ret = lexequ (arg1->nsap, arg2->nsap)) != 0)
			return (ret);
		break;
	case ADDR_INFO_OSI_NSAP_APPLIC_INFO:
		if ((ret = lexequ (arg1->nsap, arg2->nsap)) != 0)
			return (ret);
		if ((ret = str_seq_cmp (arg1->applic_info, arg2->applic_info)) != 0)
			return (ret);
		break;
	case ADDR_INFO_OSI_NSAP_APPLIC_RELAY:
		if ((ret = lexequ (arg1->nsap, arg2->nsap)) != 0)
			return (ret);
		if ((ret = str_seq_cmp (arg1->applic_relay, arg2->applic_relay)) != 0)
			return (ret);
		break;
	case ADDR_INFO_DTE_YBTS_OSI_ADDRESSING:
		if ((ret = lexequ (arg1->dte_number, arg2->dte_number)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->ybts_string, arg2->ybts_string)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->tselector, arg2->tselector)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->sselector, arg2->sselector)) != 0)
			return (ret);
		if ((ret = lexequ (arg1->pselector, arg2->pselector)) != 0)
			return (ret);
		if ((ret = quipu_pe_cmp (arg1->place_holder, arg2->place_holder)) != 0)
			return (ret);
		if ((ret = quipu_pe_cmp (arg1->application_title, arg2->application_title)) != 0)
			return (ret);
		if ((ret = quipu_pe_cmp (arg1->per_app_context_info, arg2->per_app_context_info)) != 0)
			return (ret);
		break;
	default:
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("addr_info_cmp(): Unknown addressing info type %d", arg1->addr_info_type));
		break;
	}

	return (0);
}

static addr_info_print (ps, info, format)
register PS		  ps;
struct addr_info	* info;
int			  format;
{
	switch (info->addr_info_type)
	{
	case ADDR_INFO_DTE_ONLY:
		ps_printf (ps, "%s", "dte_only");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		break;
	case ADDR_INFO_DTE_APPLIC_INFO:
		ps_printf (ps, "%s", "dte_applic_info");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		str_seq_print (ps, info->applic_info, format);
		break;
	case ADDR_INFO_DTE_CUDF:
		ps_printf (ps, "%s", "dte_cudf");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		ps_printf (ps, "%s", info->cudf);
		break;
	case ADDR_INFO_DTE_CUDF_APPLIC_INFO:
		ps_printf (ps, "%s", "dte_cudf_applic_info");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		ps_printf (ps, "%s", info->cudf);
		ps_printf (ps, "#");
		str_seq_print (ps, info->applic_info, format);
		break;
	case ADDR_INFO_DTE_YBTS:
		ps_printf (ps, "%s", "dte_ybts");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		ps_printf (ps, "%s", info->ybts_string);
		break;
	case ADDR_INFO_DTE_YBTS_APPLIC_INFO:
		ps_printf (ps, "%s", "dte_ybts_applic_info");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		ps_printf (ps, "%s", info->ybts_string);
		ps_printf (ps, "#");
		str_seq_print (ps, info->applic_info, format);
		break;
	case ADDR_INFO_DTE_YBTS_APPLIC_RELAY:
		ps_printf (ps, "%s", "dte_ybts_applic_relay");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		ps_printf (ps, "%s", info->ybts_string);
		ps_printf (ps, "#");
		str_seq_print (ps, info->applic_relay, format);
		break;
	case ADDR_INFO_NONE_NEEDED:
		ps_printf (ps, "%s", "none_needed");
		ps_printf (ps, ":");
		break;
	case ADDR_INFO_OSI_ADDRESSING:
		ps_printf (ps, "%s", "osi_addressing");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->nsap);
		ps_printf (ps, "#");
		if (info->tselector)
			ps_printf (ps, "%s", info->tselector);
		ps_printf (ps, "#");
		if (info->sselector)
			ps_printf (ps, "%s", info->sselector);
		ps_printf (ps, "#");
		if (info->pselector)
			ps_printf (ps, "%s", info->pselector);
		ps_printf (ps, "#");
		if (info->place_holder)
			pe_print (ps, info->place_holder, format);
		ps_printf (ps, "#");
		if (info->application_title)
			pe_print (ps, info->application_title, format);
		ps_printf (ps, "#");
		if (info->per_app_context_info)
			pe_print (ps, info->per_app_context_info, format);
		break;
	case ADDR_INFO_OSI_NSAP_ONLY:
		ps_printf (ps, "%s", "osi_nsap_only");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->nsap);
		break;
	case ADDR_INFO_OSI_NSAP_APPLIC_INFO:
		ps_printf (ps, "%s", "osi_nsap_applic_info");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->nsap);
		ps_printf (ps, "#");
		str_seq_print (ps, info->applic_info, format);
		break;
	case ADDR_INFO_OSI_NSAP_APPLIC_RELAY:
		ps_printf (ps, "%s", "osi_nsap_applic_relay");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->nsap);
		ps_printf (ps, "#");
		str_seq_print (ps, info->applic_relay, format);
		break;
	case ADDR_INFO_DTE_YBTS_OSI_ADDRESSING:
		ps_printf (ps, "%s", "dte_ybts_osi_addressing");
		ps_printf (ps, ":");
		ps_printf (ps, "%s", info->dte_number);
		ps_printf (ps, "#");
		ps_printf (ps, "%s", info->ybts_string);
		ps_printf (ps, "#");
		if (info->tselector)
			ps_printf (ps, "%s", info->tselector);
		ps_printf (ps, "#");
		if (info->sselector)
			ps_printf (ps, "%s", info->sselector);
		ps_printf (ps, "#");
		if (info->pselector)
			ps_printf (ps, "%s", info->pselector);
		ps_printf (ps, "#");
		if (info->place_holder)
			pe_print (ps, info->place_holder, format);
		ps_printf (ps, "#");
		if (info->application_title)
			pe_print (ps, info->application_title, format);
		ps_printf (ps, "#");
		if (info->per_app_context_info)
			pe_print (ps, info->per_app_context_info, format);
		break;
	default:
		ps_printf (ps, "%s", "addr_info print error");
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("addr_info_print(): Unknown addressing info type %d", info->addr_info_type));
		break;
	}
}

static struct addr_info	* str2addr_info (orig)
char * orig;
{
	struct addr_info	* result;
	char			* copy;
	char			* ptr_prev;
	char			* ptr_next;
	char			* eraser;

	result = (struct addr_info *) calloc (1, sizeof (struct addr_info));

	copy = strdup (orig);
	ptr_prev = SkipSpace (copy);

	if ( (ptr_next=index (ptr_prev, ':')) == NULLCP)
	{
		parse_error ("first separator(:) missing in addr_info '%s'", orig);
		free (copy);
		free ((char *) result);
		return ((struct addr_info *) NULL);
	}
	*ptr_next = '\0';

	/* Eliminate trailing spaces so that strcmp succeeds */
	eraser = ptr_next;
	for (eraser--; (*eraser) == ' '; eraser--)
	{
		(*eraser) = '\0';
	}

	ptr_next++;

	if (strcmp (ptr_prev, "dte_only") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_ONLY;
		ptr_prev = SkipSpace (ptr_next);
		result->dte_number = strdup (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_applic_info") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_APPLIC_INFO;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->applic_info = str2str_seq (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_cudf") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_CUDF;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->cudf = strdup (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_cudf_applic_info") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_CUDF_APPLIC_INFO;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("third separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->cudf = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->applic_info = str2str_seq (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_ybts") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_YBTS;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->ybts_string = strdup (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_ybts_applic_info") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_YBTS_APPLIC_INFO;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("third separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->ybts_string = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->applic_info = str2str_seq (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_ybts_applic_relay") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_YBTS_APPLIC_RELAY;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("third separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->ybts_string = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->applic_relay = str2str_seq (ptr_prev);
	}
	else if (strcmp (ptr_prev, "none_needed") == 0)
	{
		result->addr_info_type = ADDR_INFO_NONE_NEEDED;
	}
	else if (strcmp (ptr_prev, "osi_addressing") == 0)
	{
		result->addr_info_type = ADDR_INFO_OSI_ADDRESSING;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->nsap = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("third separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->tselector = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("fourth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->sselector = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("fifth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->pselector = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("sixth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->place_holder = asnstr2pe (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("seventh separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->application_title = asnstr2pe (ptr_prev);

		ptr_prev = ptr_next;

		result->per_app_context_info = asnstr2pe (ptr_prev);
	}
	else if (strcmp (ptr_prev, "osi_nsap_only") == 0)
	{
		result->addr_info_type = ADDR_INFO_OSI_NSAP_ONLY;
		ptr_prev = SkipSpace (ptr_next);
		result->nsap = strdup (ptr_prev);
	}
	else if (strcmp (ptr_prev, "osi_nsap_applic_info") == 0)
	{
		result->addr_info_type = ADDR_INFO_OSI_NSAP_APPLIC_INFO;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->nsap = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->applic_info = str2str_seq (ptr_prev);
	}
	else if (strcmp (ptr_prev, "osi_nsap_applic_relay") == 0)
	{
		result->addr_info_type = ADDR_INFO_OSI_NSAP_APPLIC_RELAY;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->nsap = strdup (ptr_prev);

		ptr_prev = SkipSpace (ptr_next);

		result->applic_relay = str2str_seq (ptr_prev);
	}
	else if (strcmp (ptr_prev, "dte_ybts_osi_addressing") == 0)
	{
		result->addr_info_type = ADDR_INFO_DTE_YBTS_OSI_ADDRESSING;
		ptr_prev = SkipSpace (ptr_next);
		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("second separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->dte_number = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("third separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->ybts_string = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("fourth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->tselector = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("fifth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->sselector = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("sixth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->pselector = strdup (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("seventh separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->place_holder = asnstr2pe (ptr_prev);

		ptr_prev = ptr_next;

		if ( (ptr_next=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("eighth separator(#) missing in addr_info '%s'", orig);
			free (copy);
			free ((char *) result);
			return ((struct addr_info *) NULL);
		}
		*ptr_next = '\0';
		ptr_next++;

		result->application_title = asnstr2pe (ptr_prev);

		ptr_prev = ptr_next;

		result->per_app_context_info = asnstr2pe (ptr_prev);
	}
	else
	{
		parse_error ("unknown addr_info type: '%s'", ptr_prev);
		free (copy);
		free ((char *) result);
		return ((struct addr_info *) NULL);
	}

	return (result);
}

static nrs_routes_free (arg)
struct nrs_routes * arg;
{
	if (arg == (struct nrs_routes *)NULL)
		return;

	if (arg->cost)
		pe_free (arg->cost);

	if (arg->addr_info)
		addr_info_free (arg->addr_info);

	if (arg->next)
		nrs_routes_free (arg->next);

	free ((char *) arg);
}

static struct nrs_routes * nrs_routes_cpy (arg)
struct nrs_routes * arg;
{
	struct nrs_routes * ret;

	if (arg == (struct nrs_routes *)NULL)
		return ((struct nrs_routes *)NULL);

	ret = (struct nrs_routes *) smalloc (sizeof (struct nrs_routes));

	if (arg->cost == NULLPE)
		ret->cost = NULLPE;
	else
		ret->cost = pe_cpy (arg->cost);

	ret->addr_info = addr_info_cpy (arg->addr_info);

	ret->next = nrs_routes_cpy (arg->next);

	return (ret);
}

static nrs_routes_cmp (arg1, arg2)
struct nrs_routes * arg1;
struct nrs_routes * arg2;
{
	int	  ret;

	if (arg1 == (struct nrs_routes *)NULL)
		if (arg2 == (struct nrs_routes *)NULL)
			return (0);
		else
			return (-1);

	if (arg2 == (struct nrs_routes *)NULL)
		return (1);

	if ((ret = pe_cmp (arg1->cost, arg2->cost)) != 0)
		return (ret);

	if ((ret = addr_info_cmp (arg1->addr_info, arg2->addr_info)) != 0)
		return (ret);

	return (nrs_routes_cmp (arg1->next, arg2->next));
}

static nrs_routes_print (ps, routes, format)
register PS		  ps;
struct nrs_routes	* routes;
int			  format;
{
	struct nrs_routes	* rt;

	for (rt=routes; rt != (struct nrs_routes *)NULL; rt = rt->next)
	{
		if (format == READOUT)
			ps_printf (ps, "\n---> ");

		if (rt->cost)
			pe_print (ps, rt->cost, format);

		ps_printf (ps, "#");

		if (rt->addr_info)
			addr_info_print (ps, rt->addr_info, format);

		if (rt->next != (struct nrs_routes *)NULL)
			ps_printf (ps, "|");
	}
}

static struct nrs_routes	* str2nrs_routes (orig)
char * orig;
{
	struct nrs_routes	* result;
	struct nrs_routes	**rt;
	char			* copy;
	char			* ptr_prev;
	char			* ptr_next;
	char			* ptr_mid;

	result = (struct nrs_routes *) smalloc (sizeof (struct nrs_routes));

	ptr_prev = SkipSpace(orig);

	if ((ptr_prev == NULLCP) || ((*ptr_prev) == '\0'))
	{
		return ((struct nrs_routes *)NULL);
	}

	rt = &(result);

	while ( (ptr_next=index (ptr_prev, '|')) != NULLCP)
	{
		*ptr_next = '\0';
		ptr_next++;
		(*rt) = (struct nrs_routes *) smalloc (sizeof (struct nrs_routes));

		copy = strdup (ptr_prev);

		if ( (ptr_mid=index (ptr_prev, '#')) == NULLCP)
		{
			parse_error ("separator(#) missing in nrs_route '%s'", copy);
			free (copy);
			free ((char *) result);
			return ((struct nrs_routes *) NULL);
		}
		*ptr_mid = '\0';
		ptr_mid++;

		/*
		* route-cost is not optional - use encoding of NULL
		* when this element is absent
		*/
		if ((*ptr_prev) == '\0')
		{
			(*rt)->cost = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL);
		}
		else if (((*rt)->cost = asnstr2pe (ptr_prev)) == NULLPE)
		{
			parse_error ("route-cost ASN malformed in nrs_route '%s'", copy);
			free (copy);
			free ((char *) result);
			return ((struct nrs_routes *) NULL);
		}

		ptr_prev = SkipSpace (ptr_mid);

		(*rt)->addr_info = str2addr_info (ptr_prev);

		rt = &((*rt)->next);
		ptr_prev = SkipSpace(ptr_next);
	}

	(*rt) = (struct nrs_routes *) smalloc (sizeof (struct nrs_routes));

	copy = strdup (ptr_prev);

	if ( (ptr_mid=index (ptr_prev, '#')) == NULLCP)
	{
		parse_error ("separator(#) missing in nrs_route '%s'", copy);
		free (copy);
		free ((char *) result);
		return ((struct nrs_routes *) NULL);
	}
	*ptr_mid = '\0';
	ptr_mid++;

	/*
	* route-cost is not optional - use encoding of NULL
	* when this element is absent
	*/
	if ((ptr_prev == NULLCP) || ((*ptr_prev) == '\0'))
	{
		(*rt)->cost = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL);
	}
	else if (((*rt)->cost = asnstr2pe (ptr_prev)) == NULLPE)
	{
		parse_error ("route-cost ASN malformed in nrs_route '%s'", copy);
		free (copy);
		free ((char *) result);
		return ((struct nrs_routes *) NULL);
	}

	ptr_prev = SkipSpace (ptr_mid);

	(*rt)->addr_info = str2addr_info (ptr_prev);

	(*rt)->next = (struct nrs_routes *)NULL;

	return (result);
}

static nrs_info_free (arg)
struct nrs_info * arg;
{
	if (arg == (struct nrs_info *)NULL)
		return;

	if (arg->routes)
		nrs_routes_free (arg->routes);

	free ((char *) arg);
}

static struct nrs_info * nrs_info_cpy (arg)
struct nrs_info * arg;
{
	struct nrs_info * result;

	if (arg == (struct nrs_info *)NULL)
                return ((struct nrs_info *)NULL);

	result = (struct nrs_info *) smalloc (sizeof (struct nrs_info));

	result->context = arg->context;

	result->addr_sp_id = arg->addr_sp_id;

	result->routes = nrs_routes_cpy (arg->routes);

	return (result);
}

static nrs_info_cmp (arg1, arg2)
struct nrs_info * arg1;
struct nrs_info * arg2;
{
	if (arg1 == (struct nrs_info *) NULL)
		if (arg2 == (struct nrs_info *) NULL)
			return (0);
		else 
			return (-1);

	if (arg2 == (struct nrs_info *) NULL)
		return (1);

	if (arg1->context < arg2->context)
		return (-1);

	if (arg1->context > arg2->context)
		return (1);

	if (arg1->addr_sp_id < arg2->addr_sp_id)
		return (-1);

	if (arg1->addr_sp_id > arg2->addr_sp_id)
		return (1);

	return (nrs_routes_cmp (arg1->routes, arg2->routes));
}

static context_print (ps, ctx, format)
register PS	  ps;
int		  ctx;
int		  format;
{
	if (format != READOUT)
	{
		ps_printf (ps, "%d", ctx);
		return;
	}

	switch(ctx)
	{
	case NRS_Context_X29:
		ps_printf (ps, "x29");
		break;
	case NRS_Context_TS29:
		ps_printf (ps, "ts29");
		break;
	case NRS_Context_NIFTP:
		ps_printf (ps, "niftp");
		break;
	case NRS_Context_MAIL_NIFTP:
		ps_printf (ps, "mail-niftp");
		break;
	case NRS_Context_MAIL_TELEX:
		ps_printf (ps, "mail-telex");
		break;
	case NRS_Context_JTMP:
		ps_printf (ps, "jtmp");
		break;
	case NRS_Context_JTMP_FILES:
		ps_printf (ps, "jtmp-files");
		break;
	case NRS_Context_JTMP_REG:
		ps_printf (ps, "jtmp-reg");
		break;
	case NRS_Context_YBTS_NODE:
		ps_printf (ps, "ybts-node");
		break;
	case NRS_Context_YBTS:
		ps_printf (ps, "ybts");
		break;
	case NRS_Context_FTAM:
		ps_printf (ps, "ftam");
		break;
	case NRS_Context_JTM:
		ps_printf (ps, "jtm");
		break;
	case NRS_Context_JTM_REG:
		ps_printf (ps, "jtm-reg");
		break;
	case NRS_Context_VT:
		ps_printf (ps, "vt");
		break;
	case NRS_Context_MOTIS:
		ps_printf (ps, "motis");
		break;
	default:
		ps_printf (ps, "%d", ctx);
		break;
	}
}

static addr_sp_id_print (ps, asi, format)
register PS	  ps;
int		  asi;
int		  format;
{
	if (format != READOUT)
	{
		ps_printf (ps, "%d", asi);
		return;
	}

	switch(asi)
	{
	case NRS_Address_Space_Id_PSS:
		ps_printf (ps, "pss");
		break;
	case NRS_Address_Space_Id_JANET:
		ps_printf (ps, "janet");
		break;
	case NRS_Address_Space_Id_TELEX:
		ps_printf (ps, "telex");
		break;
	case NRS_Address_Space_Id_OSI_CONS:
		ps_printf (ps, "osi-cons");
		break;
	default:
		ps_printf (ps, "%d", asi);
		break;
	}
}

static nrs_info_print (ps, nrs, format)
register PS	  ps;
struct nrs_info	* nrs;
int		  format;
{
	context_print (ps, nrs->context, format);

	ps_printf (ps, "$");

	addr_sp_id_print (ps, nrs->addr_sp_id, format);

	ps_printf (ps, "$");

	if (nrs->routes)
		nrs_routes_print (ps, nrs->routes, format);
}

static str2context (orig)
char	* orig;
{
	char	* str;
	char	* cc;
	int	  its_all_digits;

	str = SkipSpace(orig);
	its_all_digits = 1;

	if ((str == NULLCP) || ((*str) == '\0'))
	{
		parse_error ("no context string", NULLCP);
		return(-1);
	}

	/* Check for numeric-ness and strip trailing spaces */
	for (cc=str; (*cc)!='\0'; cc++)
	{
		if (isspace(*cc))
		{
			(*cc) = '\0';
			cc++;
			break;
		}

		if (!isdigit(*cc))
		{
			its_all_digits = 0;
		}
	}

	while (isspace(*cc))
		cc++;
	if ((*cc) != '\0')
	{
		parse_error ("malformed context string", NULLCP);
		return(-1);
	}

	if (its_all_digits)
	{
		return(atoi(str));
	}
	else if (strcmp (str, "x29") == 0)
	{
		return (NRS_Context_X29);
	}
	else if (strcmp (str, "ts29") == 0)
	{
		return (NRS_Context_TS29);
	}
	else if (strcmp (str, "niftp") == 0)
	{
		return (NRS_Context_NIFTP);
	}
	else if (strcmp (str, "mail-niftp") == 0)
	{
		return (NRS_Context_MAIL_NIFTP);
	}
	else if (strcmp (str, "mail-telex") == 0)
	{
		return (NRS_Context_MAIL_TELEX);
	}
	else if (strcmp (str, "jtmp") == 0)
	{
		return (NRS_Context_JTMP);
	}
	else if (strcmp (str, "jtmp-files") == 0)
	{
		return (NRS_Context_JTMP_FILES);
	}
	else if (strcmp (str, "jtmp-reg") == 0)
	{
		return (NRS_Context_JTMP_REG);
	}
	else if (strcmp (str, "ybts-node") == 0)
	{
		return (NRS_Context_YBTS_NODE);
	}
	else if (strcmp (str, "ybts") == 0)
	{
		return (NRS_Context_YBTS);
	}
	else if (strcmp (str, "ftam") == 0)
	{
		return (NRS_Context_FTAM);
	}
	else if (strcmp (str, "jtm") == 0)
	{
		return (NRS_Context_JTM);
	}
	else if (strcmp (str, "jtm-reg") == 0)
	{
		return (NRS_Context_JTM_REG);
	}
	else if (strcmp (str, "vt") == 0)
	{
		return (NRS_Context_VT);
	}
	else if (strcmp (str, "motis") == 0)
	{
		return (NRS_Context_MOTIS);
	}
	else
	{
		parse_error ("unknown context string '%s'", str);
		return (-1);
	}
}

static str2addr_sp_id (orig)
char	* orig;
{
	char	* str;
	char	* cc;
	int	  its_all_digits;

	str = SkipSpace(orig);
	its_all_digits = 1;

	if ((str == NULLCP) || ((*str) == '\0'))
	{
		parse_error ("no address space string", NULLCP);
		return(-1);
	}

	/* Check for numeric-ness and strip trailing spaces */
	for (cc=str; (*cc)!='\0'; cc++)
	{
		if (isspace(*cc))
		{
			(*cc) = '\0';
			cc++;
			break;
		}

		if (!isdigit(*cc))
		{
			its_all_digits = 0;
		}
	}

	while (isspace(*cc))
		cc++;
	if ((*cc) != '\0')
	{
		parse_error ("malformed address space string", NULLCP);
		return(-1);
	}

	if (its_all_digits)
	{
		return(atoi(str));
	}
	else if (strcmp (str, "pss") == 0)
	{
		return(NRS_Address_Space_Id_PSS);
	}
	else if (strcmp (str, "janet") == 0)
	{
		return(NRS_Address_Space_Id_JANET);
	}
	else if (strcmp (str, "telex") == 0)
	{
		return(NRS_Address_Space_Id_TELEX);
	}
	else if (strcmp (str, "osi-cons") == 0)
	{
		return(NRS_Address_Space_Id_OSI_CONS);
	}
	else
	{
		parse_error ("unknown address space string '%s'", str);
		return (-1);
	}
}

static struct nrs_info	* str2nrs_info (orig)
char * orig;
{
	struct nrs_info	* result;
	char		* copy;
	char		* ptr_prev;
	char		* ptr_next;

	result = (struct nrs_info *) smalloc (sizeof (struct nrs_info));

	copy = strdup (orig);
	ptr_prev = copy;

	if ( (ptr_next=index (ptr_prev, '$')) == NULLCP)
	{
		parse_error ("first separator($) missing in nrs_info '%s'", orig);
		free (copy);
		free ((char *) result);
		return ((struct nrs_info *) NULL);
	}
	*ptr_next = '\0';
	ptr_next++;

	if ((result->context = str2context (ptr_prev)) == -1)
	{
		parse_error ("malformed context '%s'", orig);
		free (copy);
		free ((char *) result);
		return ((struct nrs_info *) NULL);
	}

	ptr_prev = ptr_next;

	if ( (ptr_next=index (ptr_prev, '$')) == NULLCP)
	{
		parse_error ("second separator($) missing in nrs_info '%s'", orig);
		free (copy);
		free ((char *) result);
		return ((struct nrs_info *) NULL);
	}
	*ptr_next = '\0';
	ptr_next++;

	if ((result->addr_sp_id = str2addr_sp_id (ptr_prev)) == -1)
	{
		parse_error ("malformed context '%s'", orig);
		free (copy);
		free ((char *) result);
		return ((struct nrs_info *) NULL);
	}

	if (((ptr_prev = SkipSpace (ptr_next)) == NULLCP) || ((*ptr_prev) == '\0'))
	{
		result->routes = ((struct nrs_routes *)NULL);
		return (result);
	}

	if ((result->routes = str2nrs_routes (ptr_prev)) == (struct nrs_routes *)NULL)
	{
		parse_error ("unparseable routes in nrs_info '%s'", orig);
		free (copy);
		free ((char *) result);
		return ((struct nrs_info *) NULL);
	}

	return (result);
}

static PE nrs_info_enc (nrs)
struct nrs_info * nrs;
{
PE ret_pe;

        if (encode_QuipuNRS_NRSInformation (&ret_pe, 1, 0, NULLCP, nrs) != OK)
		return (NULLPE);

	return (ret_pe);
}

static struct nrs_info * nrs_info_dec (pe)
PE pe;
{
	struct nrs_info	* nrs;

	if (decode_QuipuNRS_NRSInformation (pe, 1, NULLIP, NULLVP, &nrs) != OK)
	{
		return ((struct nrs_info *) NULL);
	}

	return (nrs);
}

nrs_info_syntax ()
{
	(void) add_attribute_syntax ("NRSInformation",
		(IFP) nrs_info_enc,	(IFP) nrs_info_dec,
		(IFP) str2nrs_info,	nrs_info_print,
		(IFP) nrs_info_cpy,	nrs_info_cmp,
		nrs_info_free,		NULLCP,
		NULLIFP,		TRUE);
}

