%{
/* unfixit.l - convert shortened external names to names back to their 
 *             original names.  (See fixit.l)
 */

/* 
 * This program is included to satisfy "dumb" compilers/linkers which 
 * do not know about externals of names longer than 8 bytes.
 *
 *  Steven W. Layten
 *  Chemical Abstracts Service
 *  PO BOX 3012
 *  Columbus, OH 43210
 */
%}
%%
"actfilnm"		printf("action_file_name");
"actnout"		printf("action_out");
"addacpt"		printf("add_accept");
"alllower"		printf("all_lower");
"allupper"		printf("all_upper");
"allocarr"		printf("allocate_array");
"asscrule"		printf("assoc_rule");
"bktrkfil"		printf("backtrack_file");
"bktrkrep"		printf("backtrack_report");
"bol_nded"		printf("bol_needed");
"bldeofac"		printf("build_eof_action");
"cclnstal"		printf("cclinstal");
"ccllookp"		printf("ccllookup");
"cclnegat"		printf("cclnegate");
"cclsrted"		printf("cclsorted");
"ck4bktrk"		printf("check_for_backtracking");
"cktrlcnt"		printf("check_trailing_context");
"cntdactn"		printf("continued_action");
"copystrn"		printf("copy_string");
"cpunsstr"		printf("copy_unsigned_string");
"cpyrght"		printf("copyright");
"copysngl"		printf("copysingl");
"c_mx_ccl"		printf("current_max_ccl_tbl_size");
"c_mx_dfa"		printf("current_max_dfa_size");
"c_mxdfas"		printf("current_max_dfas");
"curmxrls"		printf("current_max_rules");
"c_mx_scs"		printf("current_max_scs");
"c_mx_tmp"		printf("current_max_template_xpairs");
"c_mx_xpr"		printf("current_max_xpairs");
"c_mxccls"		printf("current_maxccls");
"curr_mns"		printf("current_mns");
"cursttyp"		printf("current_state_type");
"datflush"		printf("dataflush");
"dfacunin"		printf("dfaacc_union");
"do_indnt"		printf("do_indent");
"dmpasrl"		printf("dump_associated_rules");
"dmptrns"		printf("dump_transitions");
"dupmach"		printf("dupmachine");
"ecsfrmxt"		printf("ecs_from_xlation");
"eobstate"		printf("end_of_buffer_state");
"epsclos"		printf("epsclosure");
"expnxtck"		printf("expand_nxt_chk");
"fndtblsp"		printf("find_table_space");
"fnshrule"		printf("finish_rule");
"firstfre"		printf("firstfree");
"firstprt"		printf("firstprot");
"flxgettm"		printf("flex_gettime");
"flxerror"		printf("flexerror");
"flxfatal"		printf("flexfatal");
"fmtptmsg"		printf("format_pinpoint_message");
"gnNULtrn"		printf("gen_NUL_trans");
"gnbktrkg"		printf("gen_backtracking");
"gnbtactn"		printf("gen_bt_action");
"gnfndact"		printf("gen_find_action");
"gnlindir"		printf("gen_line_dirs");
"gnnxcste"		printf("gen_next_compressed_state");
"gnnxmtch"		printf("gen_next_match");
"gnnxtst"		printf("gen_next_state");
"gnstrtst"		printf("gen_start_state");
"hshentry"		printf("hash_entry");
"hshfct"			printf("hashfunct");
"incmxdfa"		printf("increase_max_dfas");
"indput2s"		printf("indent_put2s");
"indputs"		printf("indent_puts");
"infilnam"		printf("infilename");
"inp_fles"		printf("input_files");
"intractv"		printf("interactive");
"lndirout"		printf("line_directive_out");
"lnkmchns"		printf("link_machines");
"lst_cset"		printf("list_character_set");
"maketbls"		printf("make_tables");
"mkbgnorm"		printf("mark_beginning_as_normal");
"mktmplat"		printf("mktemplate");
"nbktrckg"		printf("num_backtracking");
"ninfiles"		printf("num_input_files");
"numraloc"		printf("num_reallocs");
"numrules"		printf("num_rules");
"nuxlatns"		printf("num_xlations");
"numnpair"		printf("numsnpairs");
"outfilnm"		printf("output_file_name");
"peakpair"		printf("peakpairs");
"perf_rep"		printf("performance_report");
"pptmsg"		printf("pinpoint_message");
"plcstate"		printf("place_state");
"prvctdan"		printf("previous_continued_action");
"prtstats"		printf("printstats");
"pgm_name"		printf("program_name");
"prtcomst"		printf("protcomst");
"rdblefrm"		printf("readable_form");
"realrjct"		printf("real_reject");
"rallocar"		printf("reallocate_array");
"rjctused"		printf("reject_really_used");
"rulelnno"		printf("rule_linenum");
"ruletype"		printf("rule_type");
"stinpfle"		printf("set_input_file");
"setupia"		printf("set_up_initial_allocations");
"startime"		printf("starttime");
"ste_type"		printf("state_type");
"symfollo"		printf("symfollowset");
"sympartn"		printf("sympartition");
"syntxerr"		printf("syntaxerror");
"tmpactfl"		printf("temp_action_file");
"todohead"		printf("todo_head");
"todonext"		printf("todo_next");
"trnschar"		printf("transchar");
"trnstout"		printf("transition_struct_out");
"trlcntxt"		printf("trlcontxt");
"vtrailrl"		printf("variable_trail_rule");
"vtrlctrl"		printf("variable_trailing_context_rules");
"varlngth"		printf("varlength");
"yycrbffr"		printf("yy_create_buffer");
"yydlbffr"		printf("yy_delete_buffer");
"yyinbffr"		printf("yy_init_buffer");
"yyldbfst"		printf("yy_load_buffer_state");
"yyswtobf"		printf("yy_switch_to_buffer");
"yyerrflg"		printf("yyerrflag");
"yymrreus"		printf("yymore_really_used");
"yymrused"		printf("yymore_used");
"yyrestrt"		printf("yyrestart");
. ECHO;
%%
main()
{
    yylex();
}
