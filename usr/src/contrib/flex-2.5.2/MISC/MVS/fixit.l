%{
/* fixit.l - convert long external names to names of 8-bytes or less */
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
"action_file_name"			printf("actfilnm");
"action_out"				printf("actnout");
"add_accept"				printf("addacpt");
"all_lower"				printf("alllower");
"all_upper"				printf("allupper");
"allocate_array"			printf("allocarr");
"assoc_rule"				printf("asscrule");
"backtrack_file"			printf("bktrkfil");
"backtrack_report"			printf("bktrkrep");
"bol_needed"				printf("bol_nded");
"build_eof_action"			printf("bldeofac");
"cclinstal"				printf("cclnstal");
"ccllookup"				printf("ccllookp");
"cclnegate"				printf("cclnegat");
"cclsorted"				printf("cclsrted");
"check_for_backtracking"		printf("ck4bktrk");
"check_trailing_context"		printf("cktrlcnt");
"continued_action"			printf("cntdactn");
"copy_string"				printf("copystrn");
"copy_unsigned_string"			printf("cpunsstr");
"copyright"				printf("cpyrght");
"copysingl"				printf("copysngl");
"current_max_ccl_tbl_size"		printf("c_mx_ccl");
"current_max_dfa_size"			printf("c_mx_dfa");
"current_max_dfas"			printf("c_mxdfas");
"current_max_rules"			printf("curmxrls");
"current_max_scs"			printf("c_mx_scs");
"current_max_template_xpairs"		printf("c_mx_tmp");
"current_max_xpairs"			printf("c_mx_xpr");
"current_maxccls"			printf("c_mxccls");
"current_mns"				printf("curr_mns");
"current_state_type"			printf("cursttyp");
"dataflush"				printf("datflush");
"dfaacc_union"				printf("dfacunin");
"do_indent"				printf("do_indnt");
"dump_associated_rules"			printf("dmpasrl");
"dump_transitions"			printf("dmptrns");
"dupmachine"				printf("dupmach");
"ecs_from_xlation"			printf("ecsfrmxt");
"end_of_buffer_state"			printf("eobstate");
"epsclosure"				printf("epsclos");
"expand_nxt_chk"			printf("expnxtck");
"find_table_space"			printf("fndtblsp");
"finish_rule"				printf("fnshrule");
"firstfree"				printf("firstfre");
"firstprot"				printf("firstprt");
"flex_gettime"				printf("flxgettm");
"flexerror"				printf("flxerror");
"flexfatal"				printf("flxfatal");
"format_pinpoint_message"		printf("fmtptmsg");
"gen_NUL_trans"				printf("gnNULtrn");
"gen_backtracking"			printf("gnbktrkg");
"gen_bt_action"				printf("gnbtactn");
"gen_find_action"			printf("gnfndact");
"gen_line_dirs"				printf("gnlindir");
"gen_next_compressed_state"		printf("gnnxcste");
"gen_next_match"			printf("gnnxmtch");
"gen_next_state"			printf("gnnxtst");
"gen_start_state"			printf("gnstrtst");
"hash_entry"				printf("hshentry");
"hashfunct"				printf("hshfct");
"increase_max_dfas"			printf("incmxdfa");
"indent_put2s"				printf("indput2s");
"indent_puts"				printf("indputs");
"infilename"				printf("infilnam");
"input_files"				printf("inp_fles");
"interactive"				printf("intractv");
"line_directive_out"			printf("lndirout");
"link_machines"				printf("lnkmchns");
"list_character_set"			printf("lst_cset");
"make_tables"				printf("maketbls");
"mark_beginning_as_normal"		printf("mkbgnorm");
"mktemplate"				printf("mktmplat");
"num_backtracking"			printf("nbktrckg");
"num_input_files"			printf("ninfiles");
"num_reallocs"				printf("numraloc");
"num_rules"				printf("numrules");
"num_xlations"				printf("nuxlatns");
"numsnpairs"				printf("numnpair");
"output_file_name"			printf("outfilnm");
"peakpairs"				printf("peakpair");
"performance_report"			printf("perf_rep");
"pinpoint_message"			printf("pptmsg");
"place_state"				printf("plcstate");
"previous_continued_action"		printf("prvctdan");
"printstats"				printf("prtstats");
"program_name"				printf("pgm_name");
"protcomst"				printf("prtcomst");
"readable_form"				printf("rdblefrm");
"real_reject"				printf("realrjct");
"reallocate_array"			printf("rallocar");
"reject_really_used"			printf("rjctused");
"rule_linenum"				printf("rulelnno");
"rule_type"				printf("ruletype");
"set_input_file"			printf("stinpfle");
"set_up_initial_allocations"		printf("setupia");
"starttime"				printf("startime");
"state_type"				printf("ste_type");
"symfollowset"				printf("symfollo");
"sympartition"				printf("sympartn");
"syntaxerror"				printf("syntxerr");
"temp_action_file"			printf("tmpactfl");
"todo_head"				printf("todohead");
"todo_next"				printf("todonext");
"transchar"				printf("trnschar");
"transition_struct_out"			printf("trnstout");
"trlcontxt"				printf("trlcntxt");
"variable_trail_rule"			printf("vtrailrl");
"variable_trailing_context_rules"	printf("vtrlctrl");
"varlength"				printf("varlngth");
"yy_create_buffer"			printf("yycrbffr");
"yy_delete_buffer"			printf("yydlbffr");
"yy_init_buffer"			printf("yyinbffr");
"yy_load_buffer_state"			printf("yyldbfst");
"yy_switch_to_buffer"			printf("yyswtobf");
"yyerrflag"				printf("yyerrflg");
"yymore_really_used"			printf("yymrreus");
"yymore_used"				printf("yymrused");
"yyrestart"				printf("yyrestrt");
.					ECHO;
%%
main()
{
    yylex();
}
