



















                   The FRANZ LISP Manual

                             by


                      _J_o_h_n _K_. _F_o_d_e_r_a_r_o


                      _K_e_i_t_h _L_. _S_k_l_o_w_e_r


                        _K_e_v_i_n _L_a_y_e_r












                         June 1983












                                               A document in
                                              four movements
9

9










                       _O_v_e_r_t_u_r_e


     _A  _c_h_o_r_u_s  _o_f  _s_t_u_d_e_n_t_s  _u_n_d_e_r  _t_h_e  _d_i_r_e_c_t_i_o_n  _o_f
     _R_i_c_h_a_r_d _F_a_t_e_m_a_n _h_a_v_e _c_o_n_t_r_i_b_u_t_e_d _t_o _b_u_i_l_d_i_n_g _F_R_A_N_Z
     _L_I_S_P _f_r_o_m _a _m_e_r_e _m_e_l_o_d_y _i_n_t_o  _a  _f_u_l_l  _s_y_m_p_h_o_n_y  .
     _T_h_e  _m_a_j_o_r _c_o_n_t_r_i_b_u_t_o_r_s _t_o _t_h_e _i_n_i_t_i_a_l _s_y_s_t_e_m _w_e_r_e
     _M_i_k_e _C_u_r_r_y,  _J_o_h_n  _B_r_e_e_d_l_o_v_e  _a_n_d  _J_e_f_f  _L_e_v_i_n_s_k_y.
     _B_i_l_l  _R_o_w_a_n  _a_d_d_e_d _t_h_e _g_a_r_b_a_g_e _c_o_l_l_e_c_t_o_r _a_n_d _a_r_r_a_y
     _p_a_c_k_a_g_e.  _T_o_m _L_o_n_d_o_n _w_o_r_k_e_d _o_n _a_n  _e_a_r_l_y  _c_o_m_p_i_l_e_r
     _a_n_d   _h_e_l_p_e_d  _i_n  _o_v_e_r_a_l_l  _s_y_s_t_e_m  _d_e_s_i_g_n.   _K_e_i_t_h
     _S_k_l_o_w_e_r _h_a_s _c_o_n_t_r_i_b_u_t_e_d _m_u_c_h _t_o _F_R_A_N_Z _L_I_S_P, _a_d_d_i_n_g
     _t_h_e  _b_i_g_n_u_m _p_a_c_k_a_g_e _a_n_d _r_e_w_r_i_t_i_n_g _m_o_s_t _o_f _t_h_e _c_o_d_e
     _t_o _i_n_c_r_e_a_s_e  _i_t_s  _e_f_f_i_c_i_e_n_c_y  _a_n_d  _c_l_a_r_i_t_y.   _K_i_p_p
     _H_i_c_k_m_a_n  _a_n_d  _C_h_a_r_l_e_s  _K_o_e_s_t_e_r _a_d_d_e_d _h_u_n_k_s.  _M_i_t_c_h
     _M_a_r_c_u_s _a_d_d_e_d *_r_s_e_t, _e_v_a_l_h_o_o_k _a_n_d  _e_v_a_l_f_r_a_m_e.   _D_o_n
     _C_o_h_e_n  _a_n_d  _o_t_h_e_r_s  _a_t  _C_a_r_n_e_g_i_e-_M_e_l_l_o_n  _m_a_d_e _s_o_m_e
     _i_m_p_r_o_v_e_m_e_n_t_s _t_o  _e_v_a_l_f_r_a_m_e  _a_n_d  _p_r_o_v_i_d_e_d  _v_a_r_i_o_u_s
     _f_e_a_t_u_r_e_s  _m_o_d_e_l_l_e_d  _a_f_t_e_r  _U_C_I/_C_M_U _P_D_P-_1_0 _L_i_s_p _a_n_d
     _I_n_t_e_r_l_i_s_p  _e_n_v_i_r_o_n_m_e_n_t_s  (_e_d_i_t_o_r,  _d_e_b_u_g_g_e_r,  _t_o_p-
     _l_e_v_e_l).  _J_o_h_n _F_o_d_e_r_a_r_o _w_r_o_t_e _t_h_e _c_o_m_p_i_l_e_r, _a_d_d_e_d _a
     _f_e_w _f_u_n_c_t_i_o_n_s, _a_n_d _w_r_o_t_e _m_u_c_h _o_f _t_h_i_s  _m_a_n_u_a_l.  _O_f
     _c_o_u_r_s_e,  _o_t_h_e_r  _a_u_t_h_o_r_s  _h_a_v_e _c_o_n_t_r_i_b_u_t_e_d _s_p_e_c_i_f_i_c
     _c_h_a_p_t_e_r_s _a_s _i_n_d_i_c_a_t_e_d.  _K_e_v_i_n _L_a_y_e_r  _m_o_d_i_f_i_e_d  _t_h_e
     _c_o_m_p_i_l_e_r  _t_o  _p_r_o_d_u_c_e _c_o_d_e _f_o_r _t_h_e _M_o_t_o_r_o_l_a _6_8_0_0_0,
     _a_n_d _h_e_l_p_e_d _m_a_k_e _F_R_A_N_Z _L_I_S_P _p_a_s_s ``_L_i_n_t''.
     _T_h_i_s _m_a_n_u_a_l _m_a_y _b_e _s_u_p_p_l_e_m_e_n_t_e_d _o_r  _s_u_p_p_l_a_n_t_e_d  _b_y
     _l_o_c_a_l _c_h_a_p_t_e_r_s _r_e_p_r_e_s_e_n_t_i_n_g _a_l_t_e_r_a_t_i_o_n_s, _a_d_d_i_t_i_o_n_s
     _a_n_d _d_e_l_e_t_i_o_n_s.  _W_e _a_t _U._C. _B_e_r_k_e_l_e_y _a_r_e _p_l_e_a_s_e_d _t_o
     _l_e_a_r_n  _o_f  _g_e_n_e_r_a_l_l_y  _u_s_e_f_u_l  _s_y_s_t_e_m _f_e_a_t_u_r_e_s, _b_u_g
     _f_i_x_e_s, _o_r _u_s_e_f_u_l _p_r_o_g_r_a_m  _p_a_c_k_a_g_e_s,  _a_n_d  _w_e  _w_i_l_l
     _a_t_t_e_m_p_t _t_o _r_e_d_i_s_t_r_i_b_u_t_e _s_u_c_h _c_o_n_t_r_i_b_u_t_i_o_n_s.










98c9 1980, 1981, 1983 by the Regents of the University of Cali-
fornia.   (exceptions:  Chapters 13, 14 (first half), 15 and
16 have separate copyrights, as indicated. These are  repro-
duced by permission of the copyright holders.)
Permission to copy without fee all or part of this  material
is  granted provided that the copies are not made or distri-
buted for direct commercial  advantage,  and  the  copyright
notice  of  the Regents, University of California, is given.
All rights reserved.



9










Work reported herein was supported in  part  by  the  U.  S.
Department  of  Energy,  Contract DE-AT03-76SF00034, Project
Agreement DE-AS03-79ER10358, and the National Science  Foun-
dation under Grant No.  MCS 7807291


UNIX is a trademark of Bell Laboratories.  VAX and  PDP  are
trademarks  of  Digital Equiptment Coporation.  MC68000 is a
trademark of Motorola Semiconductor Products, Inc.











































9

9










                           Score



                    First Movement (_a_l_l_e_g_r_o _n_o_n _t_r_o_p_p_o)

     1. FRANZ LISP
          _I_n_t_r_o_d_u_c_t_i_o_n _t_o _F_R_A_N_Z _L_I_S_P, _d_e_t_a_i_l_s _o_f _d_a_t_a _t_y_p_e_s,
          _a_n_d _d_e_s_c_r_i_p_t_i_o_n _o_f _n_o_t_a_t_i_o_n
     2. Data Structure Access
          _F_u_n_c_t_i_o_n_s _f_o_r _t_h_e _c_r_e_a_t_i_o_n, _d_e_s_t_r_u_c_t_i_o_n _a_n_d  _m_a_n_i_-
          _p_u_l_a_t_i_o_n _o_f _l_i_s_p _d_a_t_a _o_b_j_e_c_t_s.
     3. Arithmetic Functions
          _F_u_n_c_t_i_o_n_s _t_o _p_e_r_f_o_r_m _a_r_i_t_h_m_e_t_i_c _o_p_e_r_a_t_i_o_n_s.
     4. Special Functions
          _F_u_n_c_t_i_o_n_s _f_o_r _a_l_t_e_r_i_n_g _f_l_o_w _o_f _c_o_n_t_r_o_l.  _F_u_n_c_t_i_o_n_s
          _f_o_r _m_a_p_p_i_n_g _o_t_h_e_r _f_u_n_c_t_i_o_n_s _o_v_e_r _l_i_s_t_s.
     5. I/O Functions
          _F_u_n_c_t_i_o_n_s _f_o_r  _r_e_a_d_i_n_g  _a_n_d  _w_r_i_t_i_n_g  _f_r_o_m  _p_o_r_t_s.
          _F_u_n_c_t_i_o_n_s  _f_o_r  _t_h_e  _m_o_d_i_f_i_c_a_t_i_o_n  _o_f _t_h_e _r_e_a_d_e_r'_s
          _s_y_n_t_a_x.
     6. System Functions
          _F_u_n_c_t_i_o_n_s _f_o_r _s_t_o_r_a_g_e _m_a_n_a_g_e_m_e_n_t,  _d_e_b_u_g_g_i_n_g,  _a_n_d
          _f_o_r  _t_h_e _r_e_a_d_i_n_g _a_n_d _s_e_t_t_i_n_g _o_f _g_l_o_b_a_l _L_i_s_p _s_t_a_t_u_s
          _v_a_r_i_a_b_l_e_s.   _F_u_n_c_t_i_o_n_s  _f_o_r  _d_o_i_n_g   _U_N_I_X-_s_p_e_c_i_f_i_c
          _t_a_s_k_s _s_u_c_h _a_s _p_r_o_c_e_s_s _c_o_n_t_r_o_l.


                    Second Movement (_L_a_r_g_o)

     7. The Reader
          _A _d_e_s_c_r_i_p_t_i_o_n _o_f _t_h_e  _s_y_n_t_a_x  _c_o_d_e_s  _u_s_e_d  _b_y  _t_h_e
          _r_e_a_d_e_r.  _A_n _e_x_p_l_a_n_a_t_i_o_n _o_f _c_h_a_r_a_c_t_e_r _m_a_c_r_o_s.
     8. Functions, Fclosures, and Macros
          _A  _d_e_s_c_r_i_p_t_i_o_n  _o_f  _v_a_r_i_o_u_s  _t_y_p_e_s  _o_f  _f_u_n_c_t_i_o_n_a_l
          _o_b_j_e_c_t_s.   _A_n  _e_x_a_m_p_l_e _o_f _t_h_e _u_s_e _o_f _f_o_r_e_i_g_n _f_u_n_c_-
          _t_i_o_n_s.
     9. Arrays and Vectors
          _A _d_e_t_a_i_l_e_d _d_e_s_c_r_i_p_t_i_o_n _o_f _t_h_e _p_a_r_t_s  _o_f  _a_n  _a_r_r_a_y
          _a_n_d _o_f _M_a_c_l_i_s_p _c_o_m_p_a_t_i_b_l_e _a_r_r_a_y_s.
     10. Exception Handling
          _A _d_e_s_c_r_i_p_t_i_o_n _o_f _t_h_e _e_r_r_o_r _h_a_n_d_l_i_n_g  _s_e_q_u_e_n_c_e  _a_n_d
          _o_f _a_u_t_o_l_o_a_d_i_n_g.









9

9










                    Third Movement (_S_c_h_e_r_z_o)

     11. The Joseph Lister Trace Package
          _A _d_e_s_c_r_i_p_t_i_o_n _o_f _a _v_e_r_y _u_s_e_f_u_l _d_e_b_u_g_g_i_n_g _a_i_d.
     12. Liszt, the lisp compiler
          _A _d_e_s_c_r_i_p_t_i_o_n _o_f _t_h_e _o_p_e_r_a_t_i_o_n _o_f _t_h_e _c_o_m_p_i_l_e_r _a_n_d
          _h_i_n_t_s _f_o_r _m_a_k_i_n_g _f_u_n_c_t_i_o_n_s _c_o_m_p_i_l_a_b_l_e.
     13. CMU Top Level and File Package
          _A _d_e_s_c_r_i_p_t_i_o_n  _o_f  _a  _t_o_p  _l_e_v_e_l  _w_i_t_h  _a  _h_i_s_t_o_r_y
          _m_e_c_h_a_n_i_s_m _a_n_d _a _p_a_c_k_a_g_e _w_h_i_c_h _h_e_l_p_s _y_o_u _k_e_e_p _t_r_a_c_k
          _o_f _f_i_l_e_s _o_f _l_i_s_p _f_u_n_c_t_i_o_n_s.
     14 Stepper
          _A _d_e_s_c_r_i_p_t_i_o_n _o_f _a _p_r_o_g_r_a_m _w_h_i_c_h  _p_e_r_m_i_t_s  _y_o_u  _t_o
          _p_u_t  _b_r_e_a_k_p_o_i_n_t_s  _i_n  _l_i_s_p _c_o_d_e _a_n_d _t_o _s_i_n_g_l_e _s_t_e_p
          _i_t.  _A _d_e_s_c_r_i_p_t_i_o_n _o_f _t_h_e _e_v_a_l_h_o_o_k _a_n_d _f_u_n_c_a_l_l_h_o_o_k
          _m_e_c_h_a_n_i_s_m.
     15 Fixit
          _A _p_r_o_g_r_a_m _w_h_i_c_h _p_e_r_m_i_t_s _y_o_u _t_o _e_x_a_m_i_n_e _a_n_d  _m_o_d_i_f_y
          _e_v_a_l_u_a_t_i_o_n _s_t_a_c_k _i_n _o_r_d_e_r _t_o _f_i_x _b_u_g_s _o_n _t_h_e _f_l_y.
     16 Lisp Editor
          _A _s_t_r_u_c_t_u_r_e _e_d_i_t_o_r _f_o_r _i_n_t_e_r_a_c_t_i_v_e _m_o_d_i_f_i_c_a_t_i_o_n _o_f
          _l_i_s_p _c_o_d_e.


                    Final Movement (_a_l_l_e_g_r_o)

     Appendix A - Function Index
     Appendix B - List of Special Symbols
     Appendix C - Short Subjects
          _G_a_r_b_a_g_e _c_o_l_l_e_c_t_o_r, _D_e_b_u_g_g_i_n_g, _D_e_f_a_u_l_t _T_o_p _L_e_v_e_l






















9

9



