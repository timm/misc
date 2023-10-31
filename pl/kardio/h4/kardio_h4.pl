/* Files: hierar/hdiag.pl, hierar/heart1.pl, hierar/heart2.pl, 
          hierar/possible2.pl, hierar/heart3.pl, hierar/possible3.pl,
          model/heart4.pl, model/possible4.pl, hierar/hierar.pl, 
          hierar/noth.pl, hierar/nothm.pl

   kardio_h4.pl   hierarchical, 4 level model of the heart and 
                  hierarchical diagnostic algorithm, standard Prolog

   Copyright (c) 1989, Igor Mozetic. All rights reserved.

   Author : Dr. Igor Mozetic
            Austrian Research Institute for Artificial Intelligence
            Schottengasse 3
            A-1010 Vienna, Austria
            Phone: (+43 1) 533-6112
            Email: igor@ai.univie.ac.at

   Updated: 6/29/1990

The user agrees that this program and any derivative works are to be used solely
for academic purposes and are not to be sold, distributed, or commercially 
exploited in any manner. As experimental, research software, this program is 
provided free of charge on an "as is" basis without warranty of any kind. All 
title, ownership and rights to this program and any copies remain with the 
author. When publishing any results using this program, this will be properly 
acknowledged and the following publications (whichever is relevant) will be
referenced:

   Bratko, I., Mozetic, I., Lavrac, N. KARDIO: A Study in Deep and Qualitative 
   Knowledge for Expert Systems. The MIT Press, Cambridge, MA, 1989.

Recent results and (some) relations to the KARDIO book are in:

   Mozetic, I. Diagnostic efficiency of deep and surface knowledge in KARDIO.
   Artificial Intelligence in Medicine 2 (2), pp. 67-83, 1990. 

   Mozetic, I., Pfahringer, B. Improving diagnostic efficiency in KARDIO: 
   abstractions, constraint propagation, and model compilation.
   To appear in Deep Models for Medical Knowledge Engineering
   (E. Keravnou, Ed.), Elsevier, Amsterdam, 1992.

A detailed description of the hierarchical diagnosis, some applications and
complexity analysis are in:

   Mozetic, I. Hierarchical model-based diagnosis. 
   International Journal of Man-Machine Studies 35 (3), pp. 329-362, 1991.
   To appear in Readings in Model-based Diagnosis (W. Hamscher, J. de Kleer, 
   L. Console, Eds.), Morgan Kaufmann, 1992.

   Mozetic, I. Reduction of diagnostic complexity through model abstractions.
   Report TR-90-10, Austrian Research Institute for Artificial Intelligence, 
   Vienna, 1990. Proc. First Intl. Workshop on Principles of Diagnosis, 
   Stanford University, Palo Alto, CA, July 23-25, 1990.

********************************************************************************
*/

%%
%% File: hdiag.pl
%%
%  Hierarchical 4-level model of the heart, and hierarchical diagnosis.
%  Fast for diagnosis, redundant specification (level 2->3), 
%  5972 Arr-ECG pairs (5240 unique).
/*
?- hdiag( Arr ).
Arr = arr(quiet,at,normal,jeb,normal,quiet,quiet) ;
Arr = arr(quiet,at,lgl,jeb,normal,quiet,quiet) ;
no

?- explain.
Ecg4 = [regular,abnormal,between_100_250,after_P_always_QRS,shortened,normal,
        between_100_250,abnormal,after_QRS_is_P,normal]
Ecg3 = [abnormal,b_100_250,always_QRS,shortened,regular,normal,b_100_250]
Ecg2 = [present,always_QRS,present,over_100]
Ecg1 = more_than_100
Diagnosis without abstraction:
Arr1 = tachy
Level-2 detailed (y.) or alternative diagnosis: y.
Arr2 = [sv_tachy]
Level-3 detailed (y.) or alternative diagnosis: y.
Arr3 = [at]
Level-4 detailed (y.) or alternative diagnosis: y.
Arr4 = [at,jeb]
Alternative diagnosis (y.): y.
Arr3 = [at,lgl]
Level-4 detailed (y.) or alternative diagnosis: y.
Arr4 = [at,lgl,jeb]
Alternative diagnosis (y.): y.
Arr2 = [iv_tachy]
Level-3 detailed (y.) or alternative diagnosis: y.
Arr3 = [jt]
Level-4 detailed (y.) or alternative diagnosis: y.
No consistent refinement !
No more alternatives !
*/

sample_ecg(4, ecg(regular, abnormal, between_100_250, after_P_always_QRS,
   shortened, normal, between_100_250, abnormal, after_QRS_is_P, normal)).

mem( X, [X|_] ).
mem( X, [_|L] ) :- mem( X, L ).


hdiag( Arr ) :-
   sample_ecg(4, ECG ),
   hdiag(4, ECG, Arr ).

hdiag(L, ECG, Arr ) :-
   L > 1, L0 is L-1,
   h(L, ecg, ECG, ECG0 ),   % hierar.pl
   hdiag(L0, ECG0, Arr0 ),
   h(L, arr, Arr, Arr0 ),   %   -"-
   model(L, Arr, ECG ).
hdiag(L, ECG, Arr ) :-
   not_h(L, Arr ),          % noth.pl
   model(L, Arr, ECG ).
%  not_h_m(L, Arr, ECG ).   % nothm.pl (replaces: not_h, model)

model(4, Arr, ECG ) :- !,   % L = 4
   possible( Arr ),         % possible4.pl
   heart( Arr, ECG ).       % heart4.pl
model(L, Arr, ECG ) :-      % L = 1,2,3
   possible(L, Arr ),       % possibleL.pl
   heart(L, Arr, ECG ).     % heartL.pl


explain :-
   sample_ecg(4, ECG ),
   explain(4, ECG ).

explain(L, ECG ) :-
   write_ecg(L, ECG ),
   explain(L, ECG, Arr ),
   ask_alter, !.
explain(L, ECG ) :-
   write('No more alternatives !').

explain(L, ECG, Arr ) :-
   L > 1, L0 is L-1,
   h(L, ecg, ECG, ECG0 ),
   write_ecg(L0, ECG0 ),
   explain(L0, ECG0, Arr0 ),
   ask_detail(L),
   check_detail(L, ECG, Arr0, Arr ).
explain(L, ECG, Arr ) :-
   not_h(L, Arr ),
   model(L, Arr, ECG ),
%  not_h_m(L, Arr, ECG ),     % (replaces: not_h, model)
   write('Diagnosis without abstraction:'), nl,
   write_arr(L, Arr ).

check_detail(L, ECG, Arr0, Arr ) :-   
   \+ (h(L, arr, Arr, Arr0 ),
       model(L, Arr, ECG )),
   write('No consistent refinement !'), nl, !, fail.
check_detail(L, ECG, Arr0, Arr ) :-   
   h(L, arr, Arr, Arr0 ),
   model(L, Arr, ECG ),
   write_arr(L, Arr ).
            
ask_detail(L) :-
   write('Level-'), write(L),
   write(' detailed (y.) or alternative diagnosis: '), read(y).

ask_alter :-
   write('Alternative diagnosis (y.): '), read(y), !, fail.
ask_alter.


%  Output utilities

write_ecg(L, ECG ) :-
   ecg_list(L, ECG, Ecg ),
   write('Ecg'), write(L), write(' = '), write(Ecg), nl.

ecg_list(1, ECG, ECG ) :- !.
ecg_list(L, ECG, Ecg ) :- ECG =.. [ecg | Ecg].

write_arr(L, Arr ) :-
   arr_list(L, Arr, List ),
   write('Arr'), write(L), write(' = '), write(List), nl.

arr_list(1, Arr, Arr ).
arr_list(2, arr(SV, AV, IV), Arrs ) :-
   arr_list( [SV, AV, IV], Arrs ).
arr_list(3, arr(SA, ATR, AV, JUN, BB, VF), Arrs ) :-
   arr_list( [SA, ATR, AV, JUN, BB, VF], Arrs ).
arr_list(4, arr(SA, ATR, AV, JUN, BB, Reg, Ect), Arrs ) :-
   arr_list( [SA, ATR, AV, JUN, BB, Reg, Ect], Arrs ).

arr_list( [], [] ).
arr_list( [quiet | Comps], Arrs ) :- !,
   arr_list( Comps, Arrs ).
arr_list( [normal | Comps], Arrs ) :- !,
   arr_list( Comps, Arrs ).
arr_list( [no_block | Comps], Arrs ) :- !,
   arr_list( Comps, Arrs ).
arr_list( [Arr | Comps], [Arr | Arrs] ) :-
   arr_list( Comps, Arrs ).

%%
%% File: heart1.pl
%%
%  Heart model at the 1st, most abstract level.
%  Relates 3 Arr to 3 ECG.

heart(1, Arr, ECG ) :-
   gen_imp_1( Arr, Imp ),
   imp_ecg_1( Imp, ECG ).

   gen_imp_1( brady,  form(less_than_60)   ).
   gen_imp_1( rhythm, form(between_60_100) ).
   gen_imp_1( tachy,  form(more_than_100)  ).

   imp_ecg_1( form(Rate), Rate ).

possible(1, _Arr ).

%%
%% File: heart2.pl
%%
%  Heart model at the 2nd level of detail.
%  Relates 18 Arr to 12 ECG, non-redundant specification.

heart(2, arr(SV, AV, IV), ecg(P, P_QRS, QRS, Rate) ) :-
   gen_sv_2( SV, ImpATR ),
   cond_ant_2( AV, ImpATR, ImpAV ),
   gen_iv_2( IV, ImpINV ),
   summ_2( ImpINV, ImpAV, ImpVENT ),
   cond_ret_2( AV, ImpINV, ImpRET ),
   summ_2( ImpRET, ImpATR, ImpSV ),
   sv_ecg_2( ImpSV, P ),
   av_ecg_2( ImpSV, ImpVENT, P_QRS ),
   iv_ecg_2( ImpVENT, QRS, Rate ).


%  Impulse generation.

gen_sv_2( quiet,     form(supra_vent, zero)     ).
gen_sv_2( sv_brady,  form(supra_vent, under_60) ).
gen_sv_2( sv_rhythm, form(supra_vent, b_60_100) ).
gen_sv_2( sv_tachy,  form(supra_vent, over_100) ).

gen_iv_2( quiet,     form(intra_vent, zero)     ).
gen_iv_2( iv_brady,  form(intra_vent, under_60) ).
gen_iv_2( iv_rhythm, form(intra_vent, b_60_100) ).
gen_iv_2( iv_tachy,  form(intra_vent, over_100) ).


%  Impulse conduction.

cond_ant_2( no_block,   form(supra_vent, Rate), 
                        form(sv_normal,  Rate) ).
cond_ant_2( av_block_2, form(supra_vent, Rate), form(sv_blocked, Rate0) ) :- 
   reduced_2( Rate, Rate0 ).
cond_ant_2( av_block_3, form(Loc, Rate), form(Loc, zero) ).

cond_ret_2( av_block_3, form(Loc, Rate), form(Loc, zero) ).
cond_ret_2( AV,         form(intra_vent, Rate),
                        form(Retro,      Rate) ) :-
   mem( AV, [no_block, av_block_2] ),
   retro_2( Rate, Retro ).

   retro_2( zero, intra_vent ).
   retro_2( Rate, iv_simult  ) :- non_zero_2( Rate ).
   retro_2( Rate, iv_differ  ) :- non_zero_2( Rate ).


%  Impulse projection on ECG.

sv_ecg_2( form(_,  zero), absent ).
sv_ecg_2( form(SV, Rate), P      ) :-
   non_zero_2( Rate ),
   imp_p_2( SV,         P ).

   imp_p_2( supra_vent, present ).
   imp_p_2( iv_simult,  absent  ).
   imp_p_2( iv_differ,  present ).

av_ecg_2( form(_,  zero), _,            not_app ).
av_ecg_2( form(SV, Rate), form(INV, _), P_QRS   ) :-
   non_zero_2( Rate ),
   imp_p_qrs_2( SV,         INV,        P_QRS ).

   imp_p_qrs_2( supra_vent, sv_normal,  always_QRS ).
   imp_p_qrs_2( supra_vent, sv_blocked, miss_QRS   ).
   imp_p_qrs_2( iv_simult,  intra_vent, not_app    ).
   imp_p_qrs_2( iv_differ,  intra_vent, always_QRS ).
   imp_p_qrs_2( supra_vent, intra_vent, independ   ).

iv_ecg_2( form(sv_normal,  Rate), present, Rate ).
iv_ecg_2( form(sv_blocked, Rate), present, Rate ).
iv_ecg_2( form(intra_vent, Rate), present, Rate ).
/*
iv_ecg_2( form(intra_vent, Rate), wide,   Rate ).
*/


%  Impulse summation.

summ_2( form(_,    zero), Impulse,      Impulse ).
summ_2( form(Form, Rate), form(_,zero), form(Form, Rate) ) :-
   non_zero_2( Rate ).


%  Utilities.

reduced_2( Rate,     Rate     ).
reduced_2( over_100, b_60_100 ).
reduced_2( b_60_100, under_60 ).

non_zero_2( Rate ) :-
   mem( Rate, [under_60, b_60_100, over_100] ).

%%
%% File: possible2.pl
%%
%  Constraints which eliminate physiologically impossible
%  and medically uninteresting arrhythmias at level 2.
%  Non-redundant (18 Arr), ground specification.

possible(2, arr(SV,   no_block,  quiet)) :- mem(SV, [sv_rhythm,sv_brady,sv_tachy]).
possible(2, arr(quiet,no_block,  IV   )) :- mem(IV, [iv_rhythm,iv_brady,iv_tachy]).
possible(2, arr(SV,   av_block_2,quiet)) :- mem(SV, [sv_rhythm,sv_brady,sv_tachy]).
possible(2, arr(SV,   av_block_3,IV   )) :- mem(SV, [sv_rhythm,sv_brady,sv_tachy]),
                                            mem(IV, [iv_rhythm,iv_brady,iv_tachy]).

%%
%% File: heart3.pl
%%
%  Heart model at 3rd level of detail.
%  Relates 175 Arr to 263 ECG, non-redundant specification.

heart(3, arr(SA, AF, AV, JUN, BB, VF),
         ecg(P, RateP, P_QRS, PR, Rhythm, QRS, Rate) ) :-
   gen_sa_3( SA, ImpSA ),
   gen_atr_3( AF, ImpAF ),
   summ_3( ImpSA, ImpAF, ImpATR ),
   cond_ant_3( AV, ImpATR, ImpAV ),
   gen_jun_3( JUN, ImpJUN ),
   gen_vent_3( VF, ImpVF ),
   summ_3( ImpJUN, ImpVF, ImpIV ),
   cond_ret_3( AV, ImpIV, ImpRET ),
   summ_3( ImpAV, ImpIV, ImpINV ),
   summ_3( ImpRET, ImpATR, ImpSV ),
   summ_3( ImpAV, ImpJUN, ImpHIS ),
   cond_bb_3( BB, ImpHIS, ImpBB ),
   summ_3( ImpBB, ImpVF, ImpVENT ),
   atr_ecg_3( ImpSV, P, RateP ),
   av_ecg_3( ImpSV, ImpINV, P_QRS, PR ),
   vent_ecg_3( ImpVENT, Rhythm, QRS, Rate ).


%  Impulse generation.

gen_sa_3( quiet, form(sa_node, none,      zero)     ).
gen_sa_3( sr,    form(sa_node, regular,   b_60_100) ).
gen_sa_3( sb,    form(sa_node, regular,   under_60) ).
gen_sa_3( st,    form(sa_node, regular,   b_100_250) ).
gen_sa_3( sad,   form(sa_node, irregular, b_60_100) ).
gen_sa_3( sad,   form(sa_node, irregular, under_60) ).

gen_atr_3( quiet, form(atr_focus, none,      zero)     ).
gen_atr_3( wp,    form(wandering, irregular, b_60_100) ).
gen_atr_3( at,    form(atr_focus, regular,   b_100_250) ).
gen_atr_3( mat,   form(wandering, irregular, b_100_250) ).
gen_atr_3( afl,   form(flutter,   regular,   b_250_350) ).
gen_atr_3( af,    form(fibflut,   irregular, over_350) ).
gen_atr_3( af,    form(fibrill,   irregular, over_350) ).

gen_jun_3( quiet, form(junction, none,    zero)     ).
gen_jun_3( jb,    form(junction, regular, under_60) ).
gen_jun_3( jr,    form(junction, regular, b_60_100) ).
gen_jun_3( jt,    form(junction, regular, b_100_250) ).

gen_vent_3( quiet, form(ventric, none,      zero)     ).
gen_vent_3( vr,    form(ventric, regular,   under_60) ).
gen_vent_3( avr,   form(ventric, regular,   b_60_100) ).
gen_vent_3( vt,    form(ventric, regular,   b_100_250) ).
gen_vent_3( vfl,   form(ventric, regular,   b_250_350) ).
gen_vent_3( vf,    form(ventric, irregular, over_350) ).


%  Impulse conduction.

cond_ant_3( AV, form(_, Rhythm1, Rate1), form(Cond, Rhythm2, Rate2) ) :-
   anterograd_3( AV, Cond,       Rhythm1,   Rhythm2,   Rate1,     Rate2 ).

anterograd_3( normal,normal,     Rhythm,    Rhythm,    Rate,      Rate ) :-
   mem( Rate, [zero, under_60, b_60_100, b_100_250] ).
anterograd_3( avb1,  delayed,    Rhythm,    Rhythm,    Rate,      Rate ) :-
   mem( Rate, [under_60, b_60_100, b_100_250] ).
anterograd_3( wen,   prog_delay, regular,   irregular, Rate0,     Rate ) :-
   reduced_3( Rate0, Rate ).
anterograd_3( wen,   prog_delay, irregular, irregular, Rate0,     Rate ) :-
   reduced_3( Rate0, Rate ).
anterograd_3( mob2,  part_block, Rhythm,    Rhythm,    Rate0,     Rate ) :-
   reduced_3( Rate0, Rate ).
anterograd_3( avb3,  _,          _,         none,      Rate,      zero ) :-
   non_zero_3( Rate ).
anterograd_3( wpw,   kent,       Rhythm,    Rhythm,    Rate,      Rate ) :-
   non_zero_3( Rate ).
anterograd_3( lgl,   james,      Rhythm,    Rhythm,    Rate,      Rate ) :-
   mem( Rate, [under_60, b_60_100, b_100_250] ).
anterograd_3( normal,normal,     irregular, irregular, over_350,  Rate ) :-
   mem( Rate, [under_60, b_60_100, b_100_250] ).
anterograd_3( normal,normal,     regular,   irregular, b_250_350, Rate ) :-
   mem( Rate, [under_60, b_60_100, b_100_250] ).
anterograd_3( normal,normal,     regular,   regular,   b_250_350, Rate ) :-
   mem( Rate, [under_60, b_60_100, b_100_250] ).

cond_ret_3( avb3, form(Loc, _, _   ),      form(Loc, none, zero) ).
cond_ret_3( _   , form(Loc, _, Rate),      form(Loc, none, zero) ) :-
   mem( Rate, [zero, b_250_350, over_350] ).
cond_ret_3( AV,   form(Loc, Rhythm, Rate), form(Retro, Rhythm, Rate) ) :-
   mem( AV, [normal, lgl, wpw, avb1, wen, mob2] ),
   mem( Rate, [under_60, b_60_100, b_100_250] ),
   retrograd_3( Loc,      Retro ).

   retrograd_3( junction, faster ).
   retrograd_3( junction, slower ).
   retrograd_3( junction, equal  ).
   retrograd_3( ventric,  equal  ).
   retrograd_3( ventric,  slower ).

cond_bb_3( BB, form(Loc,      none,   zero), form(Loc,     none,   zero) ).
cond_bb_3( BB, form(His_cond, Rhythm, Rate), form(BB_cond, Rhythm, Rate) ) :-
   non_zero_3( Rate ),
   bb_conduct_3( BB,     His_cond, BB_cond ).

   bb_conduct_3( _,      kent,     kent     ).
   bb_conduct_3( normal, His_cond, normal   ) :-
      mem( His_cond, [normal,james,delayed,prog_delay,part_block,junction] ).
   bb_conduct_3( bbb,    His_cond, bb_block ) :-
      mem( His_cond, [normal,james,delayed,prog_delay,part_block,junction] ).


%  Impulse projection on ECG.

atr_ecg_3( form(_,  none, zero), absent, zero ).
atr_ecg_3( form(SV, _,    Rate), P,      P_rate ) :-
   non_zero_3( Rate ),
   supra_vent_3( SV,        P,        Rate, P_rate ).

   supra_vent_3( sa_node,   normal,   Rate, Rate   ).
   supra_vent_3( atr_focus, abnormal, Rate, Rate   ).
   supra_vent_3( wandering, changing, Rate, Rate   ).
   supra_vent_3( flutter,   abnormal, Rate, Rate   ).
   supra_vent_3( fibflut,   abnormal, Rate, Rate   ).
   supra_vent_3( fibrill,   absent,   _,    zero   ).
   supra_vent_3( faster,    abnormal, Rate, Rate   ).
   supra_vent_3( equal,     absent,   _,    zero   ).
   supra_vent_3( slower,    abnormal, Rate, Rate   ).

av_ecg_3( form(_, _,  zero), _,              not_app, not_app ).
av_ecg_3( form(SV, _, Rate), form(AV, _, _), P_QRS,   PR ) :-
   non_zero_3( Rate ),
   his_bundle_3( AV,      SV,        PR,          P_QRS      ).

his_bundle_3( delayed,         _,    prolonged,   always_QRS ).
his_bundle_3( prog_delay,      _,    prolonged,   miss_QRS   ).
his_bundle_3( part_block,      _,    normal,      miss_QRS   ).
his_bundle_3( james,           _,    shortened,   always_QRS ).
his_bundle_3( kent,       sa_node,   shortened,   always_QRS ).
his_bundle_3( kent,       atr_focus, shortened,   always_QRS ).
his_bundle_3( kent,       wandering, shortened,   always_QRS ).
his_bundle_3( kent,       flutter,   not_app,     not_app    ).
his_bundle_3( kent,       fibflut,   not_app,     not_app    ).
his_bundle_3( kent,       fibrill,   not_app,     not_app    ).
his_bundle_3( normal,     sa_node,   normal,      always_QRS ).
his_bundle_3( normal,     fibrill,   not_app,     not_app    ).
his_bundle_3( normal,     fibflut,   not_app,     not_app    ).
his_bundle_3( normal,     fibflut,   not_app,     independ   ).
his_bundle_3( normal,     fibflut,   not_app,     miss_QRS   ).
his_bundle_3( normal,     flutter,   not_app,     miss_QRS   ).
his_bundle_3( normal,     wandering, normal,      always_QRS ).
his_bundle_3( normal,     wandering, changing,    always_QRS ).
his_bundle_3( normal,     atr_focus, normal,      always_QRS ).
his_bundle_3( normal,     atr_focus, shortened,   always_QRS ).
his_bundle_3( normal,     atr_focus, prolonged,   always_QRS ).
his_bundle_3( junction,   faster,    shortened,   always_QRS ).
his_bundle_3( AV,         equal,     not_app,     not_app    ) :-
   mem( AV, [junction, ventric] ).
his_bundle_3( AV,         slower,    after_QRS_P, always_QRS ) :-
   mem( AV, [junction, ventric] ).
his_bundle_3( AV,         SV,        not_app,     independ   ) :-
   mem( AV, [junction, ventric] ),
   mem( SV, [sa_node, atr_focus, wandering, flutter, fibflut, fibrill] ).

vent_ecg_3( form(_,Rhythm,over_350), Rhythm, absent,over_350 ).
vent_ecg_3( form(VENT,Rhythm,Rate),  Rhythm, QRS,   Rate     ) :-
   mem( Rate, [under_60, b_60_100, b_100_250, b_250_350] ),
   ventricles_3( VENT,       QRS ).

   ventricles_3( normal,   normal  ).
   ventricles_3( bb_block, wide    ).
   ventricles_3( ventric,  wide    ).
   ventricles_3( ventric,  wide_LR ).
   ventricles_3( kent,     delta_L ).
   ventricles_3( kent,     delta_R ).


%  Impulse summation.

summ_3( form(_,   none,   zero), Impulse,             Impulse ).
summ_3( form(Loc, Rhythm, Rate), form(_, none, zero), 
        form(Loc, Rhythm, Rate) ) :-
   non_zero_3( Rate ).


%  Utilities.

reduced_3( Rate,      Rate ) :- mem( Rate, [under_60, b_60_100, b_100_250] ).
reduced_3( b_60_100,  under_60 ).
reduced_3( b_100_250, b_60_100 ).

non_zero_3( Rate ) :-
   mem( Rate, [under_60, b_60_100, b_100_250, b_250_350, over_350] ).

%%
%% File: possible3.pl
%%
%  Constraints which eliminate physiologically impossible
%  and medically uninteresting arrhythmias at level 3.
%  Non-redundant (175 arrhythmias), nonground (BB) specification.

possible(3, arr(SA,   quiet,normal,quiet,BB,    quiet)) :- mem(SA, [sad,sb,sr,st]).
possible(3, arr(quiet,Atr,  normal,quiet,BB,    quiet)) :- mem(Atr,[af,afl,at,mat,wp]).
possible(3, arr(quiet,quiet,normal,Jun,  BB,    quiet)) :- mem(Jun,[jb,jr,jt]).
possible(3, arr(quiet,quiet,normal,quiet,normal,Ven)  ) :- mem(Ven,[avr,vf,vfl,vr,vt]).
possible(3, arr(SA,   quiet,wpw,   quiet,normal,quiet)) :- mem(SA, [sad,sb,sr,st]).
possible(3, arr(quiet,Atr,  wpw,   quiet,normal,quiet)) :- mem(Atr,[af,afl,at,mat,wp]).
possible(3, arr(SA,   quiet,avb3,  Jun,  BB,    quiet)) :- mem(SA, [sad,sb,sr,st]),
                                                           mem(Jun,[jb,jr,jt]).
possible(3, arr(SA,   quiet,avb3,  quiet,normal,Ven)  ) :- mem(SA, [sad,sb,sr,st]),
                                                           mem(Ven,[avr,vr,vt]).
possible(3, arr(quiet,Atr,  avb3,  Jun,  BB,    quiet)) :- mem(Atr,[af,afl,at,mat,wp]),
                                                           mem(Jun,[jb,jr,jt]).
possible(3, arr(quiet,Atr,  avb3,  quiet,normal,Ven)  ) :- mem(Atr,[af,afl,at,mat,wp]),
                                                           mem(Ven,[avr,vr,vt]).
possible(3, arr(SA,   quiet,AV,    quiet,BB,    quiet)) :- mem(SA, [sad,sb,sr,st]),
                                                           mem(AV, [avb1,lgl,mob2,wen]).
possible(3, arr(quiet,Atr,  AV,    quiet,BB,    quiet)) :- mem(Atr,[at,mat,wp]),
                                                           mem(AV, [avb1,lgl,mob2,wen]).

%%
%% File: heart4.pl
%%
%  Deep model of the heart, 4th level of detail.

heart( arr(SA, AF, AV, JUN, BB, VF, VEF),
       ecg(Rhythm, P, RateP, P_QRS, PR, QRS, Rate, EctP, EctPR, EctQRS) ) :-
   gen_sa_4( SA, ImpSA ),
   gen_atr_4( AF, ImpAF ),
   summ_4( ImpSA, ImpAF, ImpATR ),
   cond_ant_4( AV, ImpATR, ImpAV ),
   gen_jun_4( JUN, ImpJUN ),
   gen_reg_4( VF, ImpVF ),
   gen_ect_4( VEF, ImpVF ),
   summ_4( ImpJUN, ImpVF, ImpIV ),
   cond_ret_4( AV, ImpIV, ImpRET ),
   summ_4( ImpAV, ImpIV, ImpINV ),
   summ_4( ImpRET, ImpATR, ImpSV ),
   summ_4( ImpAV, ImpJUN, ImpHIS ),
   cond_bb_4( BB, ImpHIS, ImpBB ),
   summ_4( ImpBB, ImpVF, ImpVENT ),
   atr_ecg_4( ImpSV, P, RateP, EctP ),
   av_ecg_4( ImpSV, ImpINV, P_QRS, PR, EctPR ),
   vent_ecg_4( ImpVENT, Rhythm, QRS, Rate, EctQRS ).


%  Impulse generation.

gen_sa_4( quiet, form(reg(sa_node, none,      zero),
                      ect(sa_node, no)) ).
gen_sa_4( sr,    form(reg(sa_node, regular,   between_60_100),
                      ect(sa_node, no)) ).
gen_sa_4( sb,    form(reg(sa_node, regular,   under_60),
                      ect(sa_node, no)) ).
gen_sa_4( st,    form(reg(sa_node, regular,   between_100_250),
                      ect(sa_node, no)) ).
gen_sa_4( sad,   form(reg(sa_node, irregular, between_60_100),
                      ect(sa_node, no)) ).
gen_sa_4( sad,   form(reg(sa_node, irregular, under_60),
                      ect(sa_node, no)) ).
gen_sa_4( sa,    form(reg(sa_node, irregular, between_60_100),
                      ect(sa_node, no)) ).
gen_sa_4( sa,    form(reg(sa_node, irregular, under_60),
                      ect(sa_node, no)) ).

gen_atr_4( quiet, form(reg(atr_focus, none,      zero),
                       ect(atr_focus, no)) ).
gen_atr_4( wp,    form(reg(wandering, irregular, between_60_100),
                       ect(atr_focus, no)) ).
gen_atr_4( at,    form(reg(atr_focus, regular,   between_100_250),
                       ect(atr_focus, no)) ).
gen_atr_4( mat,   form(reg(wandering, irregular, between_100_250),
                       ect(atr_focus, no)) ).
gen_atr_4( afl,   form(reg(flutter,   regular,   between_250_350),
                       ect(atr_focus, no)) ).
gen_atr_4( af,    form(reg(fibflut,   irregular, over_350),
                       ect(atr_focus, no)) ).
gen_atr_4( af,    form(reg(fibrill,   irregular, over_350),
                       ect(atr_focus, no)) ).
gen_atr_4( aeb,   form(reg(atr_focus, none,      zero),
                       ect(atr_focus, uni)) ).

gen_jun_4( quiet, form(reg(junction, none,    zero),
                       ect(junction, no)) ).
gen_jun_4( jb,    form(reg(junction, regular, under_60),
                       ect(junction, no)) ).
gen_jun_4( jr,    form(reg(junction, regular, between_60_100),
                       ect(junction, no)) ).
gen_jun_4( jt,    form(reg(junction, regular, between_100_250),
                       ect(junction, no)) ).
gen_jun_4( jeb,   form(reg(junction, none,    zero),
                       ect(junction, uni)) ).


gen_reg_4( quiet, form( reg(ventric, none,      zero), _) ).
gen_reg_4( vr,    form( reg(ventric, regular,   under_60), _) ).
gen_reg_4( avr,   form( reg(ventric, regular,   between_60_100), _) ).
gen_reg_4( vt,    form( reg(ventric, regular,   between_100_250), _) ).
gen_reg_4( vfl,   form( reg(ventric, regular,   between_250_350), _) ).
gen_reg_4( vf,    form( reg(ventric, irregular, over_350), _) ).

gen_ect_4( quiet, form( _, ect(ventric, no)) ).
gen_ect_4( veb,   form( _, ect(ventric, uni)) ).


%  AV impulse conduction.

cond_ant_4( AV, form(RegATR, EctATR), form(RegAV, EctAV) ) :-
   ant_reg_4( AV, RegATR, RegAV ),
   ant_ect_4( AV, EctATR, EctAV ).

ant_ect_4( AV,   ect(Loc, no),  ect(Loc,  no) ).
ant_ect_4( avb3, ect(Loc, uni), ect(Loc,  no) ).
ant_ect_4( AV,   ect(_,   uni), ect(Cond, uni) ) :-
   ante_4( AV, Cond ).

   ante_4( normal,normal ).
   ante_4( avb1,  delayed ).
   ante_4( wen,   prog_delay ).
   ante_4( mob2,  part_block ).
   ante_4( wpw,   kent ).
   ante_4( lgl,   james ).

ant_reg_4( AV, reg(_,    Rhythm1, Rate1), reg(Cond, Rhythm2, Rate2) ) :-
   anterograd_4( AV, Cond,       Rhythm1,   Rhythm2,   Rate1,     Rate2 ).

anterograd_4( normal,normal,     Rhythm,    Rhythm,    Rate,      Rate ) :-
   mem( Rate, [zero, under_60, between_60_100, between_100_250] ).
anterograd_4( avb1,  delayed,    Rhythm,    Rhythm,    Rate,      Rate ) :-
   mem( Rate, [under_60, between_60_100, between_100_250] ).
anterograd_4( wen,   prog_delay, regular,   irregular, Rate0,     Rate ) :-
   reduced_4( Rate0, Rate ).
anterograd_4( wen,   prog_delay, irregular, irregular, Rate0,     Rate ) :-
   reduced_4( Rate0, Rate ).
anterograd_4( mob2,  part_block, Rhythm,    Rhythm,    Rate0,     Rate ) :-
   reduced_4( Rate0, Rate ).
anterograd_4( avb3,  _,          _,         none,      Rate,      zero ) :-
   non_zero_4( Rate ).
anterograd_4( wpw,   kent,       Rhythm,    Rhythm,    Rate,      Rate ) :-
   non_zero_4( Rate ).
anterograd_4( lgl,   james,      Rhythm,    Rhythm,    Rate,      Rate ) :-
   mem( Rate, [under_60, between_60_100, between_100_250] ).
anterograd_4( normal,normal,     irregular, irregular, over_350,  Rate ) :-
   mem( Rate, [under_60, between_60_100, between_100_250] ).
anterograd_4( normal,normal,     regular,   irregular, between_250_350, Rate ) :-
   mem( Rate, [under_60, between_60_100, between_100_250] ).
anterograd_4( normal,normal,     regular,   regular,   between_250_350, Rate ) :-
   mem( Rate, [under_60, between_60_100, between_100_250] ).


%  Retrograd AV impulse conduction.

cond_ret_4( AV, form(RegIV, EctIV), form(RegRET, EctRET) ) :-
   ret_reg_4( AV, RegIV, RegRET ),
   ret_ect_4( AV, EctIV, EctRET ).

ret_reg_4( avb3, reg(Loc, _, _   ),        reg(Loc,   none, zero)  ).
ret_reg_4( _   , reg(Loc, _, Rate),        reg(Loc,   none, zero)  ) :-
   mem( Rate, [zero, between_250_350, over_350] ).
ret_reg_4( AV,   reg(Loc,   Rhythm, Rate), reg(Retro, Rhythm, Rate) ) :-
   mem( AV, [normal, avb1, wen, mob2, wpw, lgl] ),
   mem( Rate, [under_60, between_60_100, between_100_250] ),
   retrograd_4( Loc,      Retro ).

ret_ect_4( AV,   ect(Loc, no),  ect(Loc,   no) ).
ret_ect_4( avb3, ect(Loc, uni), ect(Loc,   no) ).
ret_ect_4( AV,   ect(Loc, uni), ect(Retro, uni) ) :-
   mem( AV, [normal, avb1, wen, mob2, wpw, lgl] ),
   retrograd_4( Loc,      Retro ).

   retrograd_4( junction, faster ).
   retrograd_4( junction, slower ).
   retrograd_4( junction, equal  ).
   retrograd_4( ventric,  equal  ).
   retrograd_4( ventric,  slower ).


%  Impulse conduction through bundle branches.

cond_bb_4( BB, form(RegHIS, EctHIS), form(RegBB, EctBB) ) :-
   bb_reg_4( BB, RegHIS, RegBB ),
   bb_ect_4( BB, EctHIS, EctBB ).

bb_reg_4( BB, reg(Loc,      none,   zero), reg(Loc,      none,   zero) ).
bb_reg_4( BB, reg(His_cond, Rhythm, Rate), reg(BB_cond,  Rhythm, Rate) ) :-
   non_zero_4( Rate ),
   bb_conduct_4( BB,     His_cond, BB_cond ).

bb_ect_4( BB, ect(Loc,      no),  ect(Loc,     no) ).
bb_ect_4( BB, ect(His_cond, uni), ect(BB_cond, uni) ) :-
   bb_conduct_4( BB,     His_cond, BB_cond ).

   bb_conduct_4( _,      kent,     kent       ).
   bb_conduct_4( normal, His_cond, normal     ) :-
      mem( His_cond, [normal, james, delayed, prog_delay, part_block, junction] ).
   bb_conduct_4( lbbb,   His_cond, lbb_block  ) :-
      mem( His_cond, [normal, james, delayed, prog_delay, part_block, junction] ).
   bb_conduct_4( rbbb,   His_cond, rbb_block  ) :-
      mem( His_cond, [normal, james, delayed, prog_delay, part_block, junction] ).


%  SV impulse projection on ECG (P).

atr_ecg_4( form(Reg, Ect), P, Prate, EctP ) :-
   atr_reg_4( Reg, P, Prate ),
   atr_ect_4( Ect, EctP ).

atr_reg_4( reg(_,  none, zero), absent, zero ).
atr_reg_4( reg(SV, _,    Rate), P,      Prate ) :-
   non_zero_4( Rate ),
   supra_vent_4( SV,        P,        Rate, Prate ).

atr_ect_4( ect(_,  no),  absent ).
atr_ect_4( ect(SV, uni), P ) :-
   supra_vent_4( SV,        P,        _,    _ ).

   supra_vent_4( sa_node,   normal,   Rate, Rate   ).
   supra_vent_4( atr_focus, abnormal, Rate, Rate   ).
   supra_vent_4( wandering, changing, Rate, Rate   ).
   supra_vent_4( flutter,   abnormal, Rate, Rate   ).
   supra_vent_4( fibflut,   abnormal, Rate, Rate   ).
   supra_vent_4( fibrill,   absent,   _,    zero   ).
   supra_vent_4( faster,    abnormal, Rate, Rate   ).
   supra_vent_4( equal,     absent,   _,    zero   ).
   supra_vent_4( slower,    abnormal, Rate, Rate   ).


% SV and AV impulse projection on ECG (P_QRS, PR).

av_ecg_4( form(RegSV, EctSV), form(RegAV, EctAV), P_QRS, PR, EctPR ) :-
   av_reg_4( RegSV, RegAV, P_QRS, PR ),
   av_ect_4( EctSV, EctAV, EctPR ).

av_reg_4( reg(_, _,  zero), _,             meaningless, meaningless ).
av_reg_4( reg(SV, _, Rate), reg(AV, _, _), P_QRS,       PR ) :-
   non_zero_4( Rate ),
   his_bundle_4( AV,      SV,        PR,          P_QRS      ).

av_ect_4( ect(_,  no),  _,            meaningless ).
av_ect_4( ect(_,  uni), ect(_,  no),  meaningless ).
av_ect_4( ect(SV, uni), ect(AV, uni), PR ) :-
   his_bundle_4( AV,      SV,        PR,          _ ).

his_bundle_4( delayed,         _,    prolonged,   after_P_always_QRS ).
his_bundle_4( prog_delay,      _,    prolonged,   after_P_some_QRS_miss   ).
his_bundle_4( part_block,      _,    normal,      after_P_some_QRS_miss   ).
his_bundle_4( james,           _,    shortened,   after_P_always_QRS ).
his_bundle_4( kent,       sa_node,   shortened,   after_P_always_QRS ).
his_bundle_4( kent,       atr_focus, shortened,   after_P_always_QRS ).
his_bundle_4( kent,       wandering, shortened,   after_P_always_QRS ).
his_bundle_4( kent,       flutter,   meaningless, meaningless    ).
his_bundle_4( kent,       fibflut,   meaningless, meaningless    ).
his_bundle_4( kent,       fibrill,   meaningless, meaningless    ).
his_bundle_4( normal,     sa_node,   normal,      after_P_always_QRS ).
his_bundle_4( normal,     fibrill,   meaningless, meaningless    ).
his_bundle_4( normal,     fibflut,   meaningless, meaningless    ).
his_bundle_4( normal,     fibflut,   meaningless, independent_P_QRS   ).
his_bundle_4( normal,     fibflut,   meaningless, after_P_some_QRS_miss   ).
his_bundle_4( normal,     flutter,   meaningless, after_P_some_QRS_miss   ).
his_bundle_4( normal,     wandering, normal,      after_P_always_QRS ).
his_bundle_4( normal,     wandering, changing,    after_P_always_QRS ).
his_bundle_4( normal,     atr_focus, normal,      after_P_always_QRS ).
his_bundle_4( normal,     atr_focus, shortened,   after_P_always_QRS ).
his_bundle_4( normal,     atr_focus, prolonged,   after_P_always_QRS ).
his_bundle_4( junction,   faster,    shortened,   after_P_always_QRS ).
his_bundle_4( AV,         equal,     meaningless, meaningless ) :-
   mem( AV, [junction, ventric] ).
his_bundle_4( AV,         slower,    after_QRS_is_P, after_P_always_QRS ) :-
   mem( AV, [junction, ventric] ).
his_bundle_4( AV,         SV,        meaningless, independent_P_QRS ) :-
   mem( AV, [junction, ventric] ),
   mem( SV, [sa_node, atr_focus, wandering, flutter, fibflut, fibrill] ).


%  VENT impulse projection on ECG (Rhythm, Rate, QRS).

vent_ecg_4( form(Reg, Ect), Rhythm, QRS, Rate, EctQRS ) :-
   vent_reg_4( Reg, Rhythm, QRS, Rate ),
   vent_ect_4( Ect, EctQRS ).

vent_reg_4( reg(_,    Rhythm, over_350), Rhythm, absent, over_350 ).
vent_reg_4( reg(VENT, Rhythm, Rate),     Rhythm, QRS,    Rate ) :-
   mem( Rate, [under_60, between_60_100, between_100_250, between_250_350] ),
   ventricles_4( VENT,       QRS ).

vent_ect_4( ect(_,    no),   absent ).
vent_ect_4( ect(VENT, uni),  QRS ) :-
   ventricles_4( VENT,       QRS ).

   ventricles_4( normal,     normal  ).
   ventricles_4( lbb_block,  wide_LBBB  ).
   ventricles_4( rbb_block,  wide_RBBB  ).
   ventricles_4( ventric,    wide_RBBB  ).
   ventricles_4( ventric,    wide_LBBB  ).
   ventricles_4( ventric,    wide_LBBB_RBBB ).
   ventricles_4( kent,       delta_LBBB ).
   ventricles_4( kent,       delta_RBBB ).


%  Impulse summation.

summ_4( form(Reg1,Ect1), form(Reg2,Ect2), form(Reg,Ect) ) :-
   summ_reg_4( Reg1, Reg2, Reg ),
   summ_ect_4( Ect1, Ect2, Ect ).

summ_reg_4( reg(_,   none,   zero), Impulse, Impulse ).
summ_reg_4( reg(Loc, Rhythm, Rate), reg(_, none, zero), 
            reg(Loc, Rhythm, Rate) ) :-
   non_zero_4( Rate ).

summ_ect_4( ect(Loc, no),  ect(_,   no),  ect(Loc, no) ).
summ_ect_4( ect(Loc, uni), ect(_,   no),  ect(Loc, uni) ).
summ_ect_4( ect(_,   no),  ect(Loc, uni), ect(Loc, uni) ).


%  Utilities

reduced_4( Rate0,           Rate0 ) :- 
   mem( Rate0, [under_60, between_60_100, between_100_250] ).
reduced_4( between_60_100,  under_60 ).
reduced_4( between_100_250, between_60_100 ).

non_zero_4( Rate ) :-
   mem( Rate, [under_60,between_60_100,between_100_250,between_250_350,over_350]).

%%
%% File: possible4.pl
%%
%  Constraints which eliminate physiologically impossible
%  and medically uninteresting arrhythmias at level 4.
%  Non-redundant (943 Arr), nonground (BB,Ect) specification.

possible(arr(SA,   aeb,  AV,    quiet,BB,    quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(AV, [avb1,lgl,mob2,wen]).
possible(arr(SA,   aeb,  avb3,  Jun,  BB,    quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(Jun,[jb,jr,jt]).
possible(arr(SA,   aeb,  avb3,  quiet,normal,Reg,  quiet)) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(Reg,[avr,vr,vt]).
possible(arr(SA,   aeb,  normal,quiet,BB,    quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]).
possible(arr(SA,   aeb,  wpw,   quiet,normal,quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]).
possible(arr(SA,   quiet,AV,    jeb,  BB,    quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(AV, [avb1,lgl,mob2,wen]).
possible(arr(SA,   quiet,AV,    quiet,BB,    quiet,Ect)  ) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(AV, [avb1,lgl,mob2,wen]).
possible(arr(SA,   quiet,avb3,  Jun,  BB,    quiet,Ect)  ) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(Jun,[jb,jr,jt]).
possible(arr(SA,   quiet,avb3,  jeb,  BB,    Reg,  quiet)) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(Reg,[avr,vr,vt]).
possible(arr(SA,   quiet,avb3,  quiet,normal,Reg,  Ect)  ) :- mem(SA, [sa,sad,sb,sr,st]),
                                                              mem(Reg,[avr,vr,vt]).
possible(arr(SA,   quiet,normal,jeb,  BB,    quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]).
possible(arr(SA,   quiet,normal,quiet,BB,    quiet,Ect)  ) :- mem(SA, [sa,sad,sb,sr,st]).
possible(arr(SA,   quiet,wpw,   jeb,  BB,    quiet,quiet)) :- mem(SA, [sa,sad,sb,sr,st]).
possible(arr(SA,   quiet,wpw,   quiet,normal,quiet,Ect)  ) :- mem(SA, [sa,sad,sb,sr,st]).
possible(arr(quiet,Atr,  avb3,  Jun,  BB,    quiet,Ect)  ) :- mem(Atr,[af,afl,at,mat,wp]),
                                                              mem(Jun,[jb,jr,jt]).
possible(arr(quiet,Atr,  avb3,  jeb,  BB,    Reg,  quiet)) :- mem(Atr,[af,afl,at,mat,wp]),
                                                              mem(Reg,[avr,vr,vt]).
possible(arr(quiet,Atr,  avb3,  quiet,normal,Reg,  Ect)  ) :- mem(Atr,[af,afl,at,mat,wp]),
                                                              mem(Reg,[avr,vr,vt]).
possible(arr(quiet,Atr,  normal,jeb,  BB,    quiet,quiet)) :- mem(Atr,[af,afl,at,mat,wp]).
possible(arr(quiet,Atr,  normal,quiet,BB,    quiet,Ect)  ) :- mem(Atr,[af,afl,at,mat,wp]).
possible(arr(quiet,Atr,  wpw,   quiet,normal,quiet,quiet)) :- mem(Atr,[af,afl]).
possible(arr(quiet,Atr,  AV,    jeb,  BB,    quiet,quiet)) :- mem(Atr,[at,mat,wp]),
                                                              mem(AV, [avb1,lgl,mob2,wen]).
possible(arr(quiet,Atr,  AV,    quiet,BB,    quiet,Ect)  ) :- mem(Atr,[at,mat,wp]),
                                                              mem(AV, [avb1,lgl,mob2,wen]).
possible(arr(quiet,Atr,  wpw,   jeb,  BB,    quiet,quiet)) :- mem(Atr,[at,mat,wp]).
possible(arr(quiet,Atr,  wpw,   quiet,normal,quiet,Ect)  ) :- mem(Atr,[at,mat,wp]).
possible(arr(quiet,aeb,  normal,Jun,  BB,    quiet,quiet)) :- mem(Jun,[jb,jr,jt]).
possible(arr(quiet,aeb,  normal,quiet,BB,    Reg,  quiet)) :- mem(Reg,[avr,vr,vt]).
possible(arr(quiet,quiet,normal,Jun,  BB,    quiet,Ect)  ) :- mem(Jun,[jb,jr,jt]).
possible(arr(quiet,quiet,normal,jeb,  BB,    Reg,  quiet)) :- mem(Reg,[avr,vr,vt]).
possible(arr(quiet,quiet,normal,quiet,normal,Reg,  Ect)  ) :- mem(Reg,[avr,vr,vt]).
possible(arr(quiet,quiet,normal,quiet,normal,Reg,  quiet)) :- mem(Reg,[vf,vfl]). 

%%
%% File: hierar.pl
%%
%  Hierarchies over terms: h(L, Type, X, X').

%  Abstraction levels 2 -> 1.

h(2, arr, arr(SV2,   no_block,   quiet), Gen1 ) :- h(2, sv, SV2, Gen1 ).
h(2, arr, arr(quiet, no_block,   IV2),   Gen1 ) :- h(2, iv, IV2, Gen1 ).
h(2, arr, arr(_,     av_block_3, IV2),   Gen1 ) :- h(2, iv, IV2, Gen1 ).   % av_block_2

h(2, sv, sv_brady,  brady  ).
h(2, sv, sv_rhythm, rhythm ).
h(2, sv, sv_tachy,  tachy  ).

h(2, iv, iv_brady,  brady  ).
h(2, iv, iv_rhythm, rhythm ).
h(2, iv, iv_tachy,  tachy  ).

h(2, ecg, ecg(_, _, _, Rate2), Rate1 ) :- h(2, rate, Rate2, Rate1 ).

h(2, impulse, form(_, Rate2), form(Rate1) ) :- h(2, rate, Rate2, Rate1 ).

h(2, rate, under_60, less_than_60   ).
h(2, rate, b_60_100, between_60_100 ).
h(2, rate, over_100, more_than_100  ).   % zero


%  Abstraction levels 3 -> 2.

h(3, arr, arr(SA3, Atr3, AV3, Jun3, _BB, Ven3), arr(SV2, AV2, IV2) ) :-
   h(3, sv, SA3,  Atr3, SV2 ),
   h(3, av, AV3,        AV2 ),
   h(3, iv, Jun3, Ven3, IV2 ).

h(3, sv, quiet, quiet, quiet ).
h(3, sv, SA3,   quiet, SV2   ) :- h(3, sa, SA3, SV2 ).
h(3, sv, quiet, Atr3,  SV2   ) :- h(3, atr, Atr3, SV2 ).

h(3, iv, quiet, quiet, quiet ).
h(3, iv, Jun3,  quiet, IV2   ) :- h(3, jun, Jun3, IV2 ).
h(3, iv, quiet, Ven3,  IV2   ) :- h(3, ven, Ven3, IV2 ).

h(3, sa, sb,  sv_brady ).
h(3, sa, sad, sv_brady ).    % non-tree hierarchy!
h(3, sa, sad, sv_rhythm ).   % non-tree hierarchy!
h(3, sa, sr,  sv_rhythm ).
h(3, sa, st,  sv_tachy ).

h(3, atr, wp,  sv_rhythm ).
h(3, atr, at,  sv_tachy ).
h(3, atr, mat, sv_tachy ).   % af, afl

h(3, av, normal, no_block ).
h(3, av, lgl,    no_block ).
h(3, av, wpw,    no_block ).
h(3, av, avb1,   no_block ).
h(3, av, wen,    av_block_2 ).
h(3, av, mob2,   av_block_2 ).
h(3, av, avb3,   av_block_3 ).

h(3, jun, jb, iv_brady ).
h(3, jun, jr, iv_rhythm ).
h(3, jun, jt, iv_tachy ).

h(3, ven, vr,  iv_brady ).
h(3, ven, avr, iv_rhythm ).
h(3, ven, vt,  iv_tachy ).   % vf, vfl


h(3, ecg, ecg(P3, _Prate, P_QRS, _PR, _Rhythm, QRS3, Rate3),
          ecg(P2, P_QRS, QRS2, Rate2) ) :-
   h(3, p, P3, P2 ),
   h(3, qrs, QRS3, QRS2 ),
   h(3, rate, Rate3, Rate2 ).

h(3, p, absent,   absent  ).
h(3, p, normal,   present ).
h(3, p, abnormal, present ).
h(3, p, changing, present ).

h(3, p_qrs, P_QRS, P_QRS ).

h(3, qrs, normal,  present ).
h(3, qrs, delta_L, present ).
h(3, qrs, delta_R, present ).
h(3, qrs, wide_LR, present ).
h(3, qrs, wide,    present ).   % absent

h(3, rate, zero,      zero     ).
h(3, rate, under_60,  under_60 ).
h(3, rate, b_60_100,  b_60_100 ).
h(3, rate, b_100_250, over_100 ).   % b_250_350, over_350

h(3, impulse, form(Loc3, _, Rate3), form(Loc2, Rate2) ) :-
   h(3, loc, Loc3, Loc2 ),
   h(3, rate, Rate3, Rate2 ).

h(3, loc, sa_node,    supra_vent ).
h(3, loc, atr_focus,  supra_vent ).
h(3, loc, wandering,  supra_vent ).   % flutter, fibflut, fibrill
h(3, loc, junction,   intra_vent ).
h(3, loc, ventric,    intra_vent ).
h(3, loc, normal,     sv_normal  ).
h(3, loc, delayed,    sv_normal  ).
h(3, loc, james,      sv_normal  ).
h(3, loc, kent,       sv_normal  ).
h(3, loc, prog_delay, sv_blocked ).
h(3, loc, part_block, sv_blocked ).
h(3, loc, equal,      iv_simult  ).
h(3, loc, faster,     iv_differ  ).
h(3, loc, slower,     iv_differ  ).


%  Abstraction levels 4 -> 3.

h(4, arr, arr(SA4, Atr4, AV, Jun4, BB4, Reg, _Ect),
          arr(SA3, Atr3, AV, Jun3, BB3, Reg) ) :- 
   h(4, sa,  SA4,  SA3 ),
   h(4, atr, Atr4, Atr3 ),
   h(4, jun, Jun4, Jun3 ),
   h(4, bb,  BB4,  BB3 ).
h(4, arr, arr(quiet, Aeb4,  normal, quiet, Bbb4,   Reg, quiet),
          arr(quiet, quiet, normal, quiet, normal, Reg) ) :-
   aeb_bbb(4, Aeb4, Bbb4 ).
h(4, arr, arr(quiet, quiet, normal, Jeb4,  Bbb4,   Reg, quiet),
          arr(quiet, quiet, normal, quiet, normal, Reg) ) :-
   jeb_bbb(4, Jeb4, Bbb4 ).
h(4, arr, arr(SA4, Atr4, avb3, Jeb4,  Bbb4,   Reg, quiet),
          arr(SA3, Atr3, avb3, quiet, normal, Reg) ) :-
   h(4, sa,  SA4,  SA3 ),
   h(4, atr, Atr4, Atr3 ),
   jeb_bbb(4, Jeb4, Bbb4 ).
h(4, arr, arr(SA4, Atr4, wpw, Jeb4,  Bbb4,   quiet, quiet),
          arr(SA3, Atr3, wpw, quiet, normal, quiet) ) :-
   h(4, sa,  SA4,  SA3 ),
   h(4, atr, Atr4, Atr3 ),
   jeb_bbb(4, Jeb4, Bbb4 ).

   aeb_bbb(4, aeb, lbbb ).
   aeb_bbb(4, aeb, rbbb ).

   jeb_bbb(4, jeb, lbbb ).
   jeb_bbb(4, jeb, rbbb ).

h(4, sa, SA, SA ).
h(4, sa, sa, sad ).

h(4, atr, Atr, Atr ).
h(4, atr, aeb, quiet ).

h(4, jun, Jun, Jun ).
h(4, jun, jeb, quiet ).

h(4, bb, normal, normal ).
h(4, bb, lbbb,   bbb ).
h(4, bb, rbbb,   bbb ).


h(4, ecg, ecg(Rhythm, P, Prate4, P_QRS4, PR4, QRS4, Rate4, _EctP, _EctPR, _EctQRS),
          ecg(P, Prate3, P_QRS3, PR3, Rhythm, QRS3, Rate3) ) :-
   h(4, rate,  Prate4, Prate3 ),
   h(4, p_qrs, P_QRS4, P_QRS3 ),
   h(4, pr,    PR4,    PR3 ),
   h(4, qrs,   QRS4,   QRS3 ),
   h(4, rate,  Rate4,  Rate3 ).

h(4, p_qrs, meaningless,           not_app ).
h(4, p_qrs, after_P_always_QRS,    always_QRS ).
h(4, p_qrs, after_P_some_QRS_miss, miss_QRS ).
h(4, p_qrs, independent_P_QRS,     independ ).

h(4, pr, meaningless,    not_app ).
h(4, pr, normal,         normal ).
h(4, pr, changing,       changing ).
h(4, pr, prolonged,      prolonged ).
h(4, pr, shortened,      shortened ).
h(4, pr, after_QRS_is_P, after_QRS_P ).

h(4, qrs, normal,         normal ).
h(4, qrs, delta_LBBB,     delta_L ).
h(4, qrs, delta_RBBB,     delta_R ).
h(4, qrs, wide_LBBB_RBBB, wide_LR ).
h(4, qrs, wide_LBBB,      wide ).
h(4, qrs, wide_RBBB,      wide ).
h(4, qrs, absent,         absent ).

h(4, rate, zero,            zero ).
h(4, rate, under_60,        under_60 ).
h(4, rate, between_60_100,  b_60_100 ).
h(4, rate, between_100_250, b_100_250 ).
h(4, rate, between_250_350, b_250_350 ).
h(4, rate, over_350,        over_350 ).

%%
%% File: noth.pl
%%
%  Arrhythmias without abstraction: 
%  level 1 - 3 Arr, level 2 - 3 Arr, level 3 - 26 Arr, level 4 - 0 Arr
%
%  not_h(L,Arr) :- ~(E Arr') h(L,arr,Arr,Arr').

not_h(1, brady ).
not_h(1, rhythm ).
not_h(1, tachy ).

not_h(2, arr(sv_brady,av_block_2,quiet)).
not_h(2, arr(sv_rhythm,av_block_2,quiet)).
not_h(2, arr(sv_tachy,av_block_2,quiet)).

not_h(3, arr(quiet,af,avb3,jb,bbb,quiet)).
not_h(3, arr(quiet,af,avb3,jb,normal,quiet)).
not_h(3, arr(quiet,af,avb3,jr,bbb,quiet)).
not_h(3, arr(quiet,af,avb3,jr,normal,quiet)).
not_h(3, arr(quiet,af,avb3,jt,bbb,quiet)).
not_h(3, arr(quiet,af,avb3,jt,normal,quiet)).
not_h(3, arr(quiet,af,avb3,quiet,normal,avr)).
not_h(3, arr(quiet,af,avb3,quiet,normal,vr)).
not_h(3, arr(quiet,af,avb3,quiet,normal,vt)).
not_h(3, arr(quiet,af,normal,quiet,bbb,quiet)).
not_h(3, arr(quiet,af,normal,quiet,normal,quiet)).
not_h(3, arr(quiet,af,wpw,quiet,normal,quiet)).
not_h(3, arr(quiet,afl,avb3,jb,bbb,quiet)).
not_h(3, arr(quiet,afl,avb3,jb,normal,quiet)).
not_h(3, arr(quiet,afl,avb3,jr,bbb,quiet)).
not_h(3, arr(quiet,afl,avb3,jr,normal,quiet)).
not_h(3, arr(quiet,afl,avb3,jt,bbb,quiet)).
not_h(3, arr(quiet,afl,avb3,jt,normal,quiet)).
not_h(3, arr(quiet,afl,avb3,quiet,normal,avr)).
not_h(3, arr(quiet,afl,avb3,quiet,normal,vr)).
not_h(3, arr(quiet,afl,avb3,quiet,normal,vt)).
not_h(3, arr(quiet,afl,normal,quiet,bbb,quiet)).
not_h(3, arr(quiet,afl,normal,quiet,normal,quiet)).
not_h(3, arr(quiet,afl,wpw,quiet,normal,quiet)).
not_h(3, arr(quiet,quiet,normal,quiet,normal,vf)).
not_h(3, arr(quiet,quiet,normal,quiet,normal,vfl)).

%%
%% File: nothm.pl
%%
%  Precomputed pairs Arr-ECG for arrhythmias without abstraction.
%
%  not_h_m(L,Arr,ECG) :- ~(E Arr') h(L,arr,Arr,Arr'), model(L,Arr,ECG).

not_h_m(1, brady, less_than_60 ).
not_h_m(1, rhythm, between_60_100 ).
not_h_m(1, tachy, more_than_100 ).

not_h_m(2, arr(sv_brady,av_block_2,quiet), ecg(present,miss_QRS,present,under_60)).
not_h_m(2, arr(sv_rhythm,av_block_2,quiet), ecg(present,miss_QRS,present,b_60_100)).
not_h_m(2, arr(sv_rhythm,av_block_2,quiet), ecg(present,miss_QRS,present,under_60)).
not_h_m(2, arr(sv_tachy,av_block_2,quiet), ecg(present,miss_QRS,present,over_100)).
not_h_m(2, arr(sv_tachy,av_block_2,quiet), ecg(present,miss_QRS,present,b_60_100)).

not_h_m(3, arr(quiet,afl,normal,quiet,normal,quiet), 
           ecg(abnormal,b_250_350,miss_QRS,not_app,irregular,normal,under_60)).
not_h_m(3, arr(quiet,afl,normal,quiet,normal,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,irregular,normal,b_60_100)).
not_h_m(3, arr(quiet,afl,normal,quiet,normal,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,irregular,normal,b_100_250)).
not_h_m(3, arr(quiet,afl,normal,quiet,normal,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,regular,normal,under_60)).
not_h_m(3, arr(quiet,afl,normal,quiet,normal,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,regular,normal,b_60_100)).
not_h_m(3, arr(quiet,afl,normal,quiet,normal,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,regular,normal,b_100_250)).
not_h_m(3, arr(quiet,afl,normal,quiet,bbb,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,irregular,wide,under_60)).
not_h_m(3, arr(quiet,afl,normal,quiet,bbb,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,irregular,wide,b_60_100)).
not_h_m(3, arr(quiet,afl,normal,quiet,bbb,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,irregular,wide,b_100_250)).
not_h_m(3, arr(quiet,afl,normal,quiet,bbb,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,afl,normal,quiet,bbb,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,afl,normal,quiet,bbb,quiet),
           ecg(abnormal,b_250_350,miss_QRS,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,normal,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,independ,not_app,irregular,normal,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,miss_QRS,not_app,irregular,normal,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,normal,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,independ,not_app,irregular,normal,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,miss_QRS,not_app,irregular,normal,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,normal,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,independ,not_app,irregular,normal,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(abnormal,over_350,miss_QRS,not_app,irregular,normal,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(absent,zero,not_app,not_app,irregular,normal,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(absent,zero,not_app,not_app,irregular,normal,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,normal,quiet),
           ecg(absent,zero,not_app,not_app,irregular,normal,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,wide,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,independ,not_app,irregular,wide,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,miss_QRS,not_app,irregular,wide,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,independ,not_app,irregular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,miss_QRS,not_app,irregular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,independ,not_app,irregular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(abnormal,over_350,miss_QRS,not_app,irregular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(absent,zero,not_app,not_app,irregular,wide,under_60)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(absent,zero,not_app,not_app,irregular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,normal,quiet,bbb,quiet),
           ecg(absent,zero,not_app,not_app,irregular,wide,b_100_250)).
not_h_m(3, arr(quiet,quiet,normal,quiet,normal,vfl),
           ecg(absent,zero,not_app,not_app,regular,wide,b_250_350)).
not_h_m(3, arr(quiet,quiet,normal,quiet,normal,vfl),
           ecg(absent,zero,not_app,not_app,regular,wide_LR,b_250_350)).
not_h_m(3, arr(quiet,quiet,normal,quiet,normal,vf),
           ecg(absent,zero,not_app,not_app,irregular,absent,over_350)).
not_h_m(3, arr(quiet,afl,wpw,quiet,normal,quiet),
           ecg(abnormal,b_250_350,not_app,not_app,regular,delta_L,b_250_350)).
not_h_m(3, arr(quiet,afl,wpw,quiet,normal,quiet),
           ecg(abnormal,b_250_350,not_app,not_app,regular,delta_R,b_250_350)).
not_h_m(3, arr(quiet,af,wpw,quiet,normal,quiet),
           ecg(abnormal,over_350,not_app,not_app,irregular,absent,over_350)).
not_h_m(3, arr(quiet,af,wpw,quiet,normal,quiet),
           ecg(absent,zero,not_app,not_app,irregular,absent,over_350)).
not_h_m(3, arr(quiet,afl,avb3,jb,normal,quiet),
           ecg(abnormal,b_250_350,independ,not_app,regular,normal,under_60)).
not_h_m(3, arr(quiet,afl,avb3,jb,bbb,quiet),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,afl,avb3,jr,normal,quiet),
           ecg(abnormal,b_250_350,independ,not_app,regular,normal,b_60_100)).
not_h_m(3, arr(quiet,afl,avb3,jr,bbb,quiet),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,afl,avb3,jt,normal,quiet),
           ecg(abnormal,b_250_350,independ,not_app,regular,normal,b_100_250)).
not_h_m(3, arr(quiet,afl,avb3,jt,bbb,quiet),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,afl,avb3,quiet,normal,vr),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,afl,avb3,quiet,normal,vr),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide_LR,under_60)).
not_h_m(3, arr(quiet,afl,avb3,quiet,normal,avr),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,afl,avb3,quiet,normal,avr),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide_LR,b_60_100)).
not_h_m(3, arr(quiet,afl,avb3,quiet,normal,vt),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,afl,avb3,quiet,normal,vt),
           ecg(abnormal,b_250_350,independ,not_app,regular,wide_LR,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,jb,normal,quiet),
           ecg(abnormal,over_350,independ,not_app,regular,normal,under_60)).
not_h_m(3, arr(quiet,af,avb3,jb,normal,quiet),
           ecg(absent,zero,independ,not_app,regular,normal,under_60)).
not_h_m(3, arr(quiet,af,avb3,jb,bbb,quiet),
           ecg(abnormal,over_350,independ,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,af,avb3,jb,bbb,quiet),
           ecg(absent,zero,independ,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,af,avb3,jr,normal,quiet),
           ecg(abnormal,over_350,independ,not_app,regular,normal,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,jr,normal,quiet),
           ecg(absent,zero,independ,not_app,regular,normal,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,jr,bbb,quiet),
           ecg(abnormal,over_350,independ,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,jr,bbb,quiet),
           ecg(absent,zero,independ,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,jt,normal,quiet),
           ecg(abnormal,over_350,independ,not_app,regular,normal,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,jt,normal,quiet),
           ecg(absent,zero,independ,not_app,regular,normal,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,jt,bbb,quiet),
           ecg(abnormal,over_350,independ,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,jt,bbb,quiet),
           ecg(absent,zero,independ,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vr),
           ecg(abnormal,over_350,independ,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vr),
           ecg(abnormal,over_350,independ,not_app,regular,wide_LR,under_60)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vr),
           ecg(absent,zero,independ,not_app,regular,wide,under_60)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vr),
           ecg(absent,zero,independ,not_app,regular,wide_LR,under_60)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,avr),
           ecg(abnormal,over_350,independ,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,avr),
           ecg(abnormal,over_350,independ,not_app,regular,wide_LR,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,avr),
           ecg(absent,zero,independ,not_app,regular,wide,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,avr),
           ecg(absent,zero,independ,not_app,regular,wide_LR,b_60_100)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vt),
           ecg(abnormal,over_350,independ,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vt),
           ecg(abnormal,over_350,independ,not_app,regular,wide_LR,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vt),
           ecg(absent,zero,independ,not_app,regular,wide,b_100_250)).
not_h_m(3, arr(quiet,af,avb3,quiet,normal,vt),
           ecg(absent,zero,independ,not_app,regular,wide_LR,b_100_250)).

