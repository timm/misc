/* Files: surface/surface4.pl, model/possible4.pl

   kardio_sd.pl   surface diagnostic rules, 4th level of detail
                  standard Prolog, depth-first search, no domains,
                  diagnostic algorithm does not eliminate redundant diagnoses!

   Copyright (c) 1989, Igor Mozetic. All rights reserved.

   Author : Dr. Igor Mozetic
            Austrian Research Institute for Artificial Intelligence
            Schottengasse 3
            A-1010 Vienna, Austria
            Phone: (+43 1) 533-6112
            Email: igor@ai.univie.ac.at

   Updated: 6/20/1990

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

The original description of a knowledge base compression by means of machine
learning appeared in:

   Mozetic, I. Knowledge extraction through learning from examples. In Machine 
   Learning: A Guide to Current Research (T.M. Mitchell, J.G. Carbonell, R.S. 
   Michalski, Eds.), pp. 227-231, Kluwer Academic Publishers, Boston, 1986.


The following are domain definitions (domains are sorted):

domains_arr(arr(SA, Atr, AV, Jun, BB, Reg, Ect)) :-
   domain( SA,  [quiet,sa,sad,sb,sr,st] ),
   domain( Atr, [aeb,af,afl,at,mat,quiet,wp] ),
   domain( AV,  [avb1,avb3,lgl,mob2,normal,wen,wpw] ),
   domain( Jun, [jb,jeb,jr,jt,quiet] ),
   domain( BB,  [lbbb,normal,rbbb] ),
   domain( Reg, [avr,quiet,vf,vfl,vr,vt] ),
   domain( Ect, [quiet,veb] ).

domains_ecg(ecg(Rhythm, P, RateP, P_QRS, PR, QRS, Rate, EctP, EctPR, EctQRS)) :-
   domain( Rhythm,[irregular,regular] ),
   domain( P,     [abnormal,absent,changing,normal] ),
   domain( RateP, [between_100_250,between_250_350,between_60_100,
                   over_350,under_60,zero] ),
   domain( P_QRS, [after_P_always_QRS,after_P_some_QRS_miss,
                   independent_P_QRS,meaningless] ),
   domain( PR,    [after_QRS_is_P,changing,meaningless,normal,
                   prolonged,shortened] ),
   domain( QRS,   [absent,delta_LBBB,delta_RBBB,normal,wide_LBBB,
                   wide_LBBB_RBBB,wide_RBBB] ),
   domain( Rate,  [between_100_250,between_250_350,between_60_100,
                   over_350,under_60] ),
   domain( EctP,  [abnormal,absent] ),
   domain( EctPR, [after_QRS_is_P,meaningless,normal,prolonged,shortened] ),
   domain( EctQRS,[absent,delta_LBBB,delta_RBBB,normal,wide_LBBB,
                   wide_LBBB_RBBB,wide_RBBB] ).

********************************************************************************
*/

%%
%% File : surface4.pl
%%
%  Compressed diagnostic rules relate individual ECG features 
%  to multiple arrhythmias which can cause them (no domains).
%  Fast for diagnosis if ECG is given.
%  Redundant, 9777 Arr-ECG pairs (there are 5240 unique pairs).
/*
?- sdiag( Arr ).
Arr = arr(quiet,at,lgl,jeb,normal,quiet,quiet) ;
Arr = arr(quiet,at,normal,jeb,normal,quiet,quiet) ;
no
*/

sample_ecg(4, ecg(regular, abnormal, between_100_250, after_P_always_QRS,
   shortened, normal, between_100_250, abnormal, after_QRS_is_P, normal)).

mem( X, [X|_] ).
mem( X, [_|L] ) :- mem( X, L ).


sdiag( Arr ) :-
   sample_ecg(4, ECG ),
   sdiag( ECG, Arr ).

sdiag( ECG, Arr ) :-
   surface( ECG, Arr ),
   possible( Arr ).         % possible4.pl


surface( ecg(Rhythm, P, RateP, P_QRS, PR, QRS, Rate, EctP, EctPR, EctQRS), Arr ) :-
   if_then( rhythm,         Rhythm, Arr ),
   if_then( regular_P,      P,      Arr ),
   if_then( rate_of_P,      RateP,  Arr ),
   if_then( relation_P_QRS, P_QRS,  Arr ),
   if_then( regular_PR,     PR,     Arr ),
   if_then( regular_QRS,    QRS,    Arr ),
   if_then( rate_of_QRS,    Rate,   Arr ),
   if_then( ectopic_P,      EctP,   Arr ),
   if_then( ectopic_PR,     EctPR,  Arr ),
   if_then( ectopic_QRS,    EctQRS, Arr ).


if_then( rhythm, regular, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sb,sr,st] ),
   mem( Atr, [aeb,afl,at,quiet] ),
   mem( AV, [avb1,avb3,lgl,mob2,normal,wpw] ),
   mem( Reg, [avr,quiet,vfl,vr,vt] ).
if_then( rhythm, regular, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = avb3.

if_then( rhythm, irregular, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sa,sad] ),
   mem( Atr, [aeb,af,mat,quiet,wp] ),
   mem( Jun, [jeb,quiet] ),
   mem( Reg, [quiet,vf] ).
if_then( rhythm, irregular, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = afl,
   AV = normal.
if_then( rhythm, irregular, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = wen.

if_then( regular_P, normal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [sa,sad,sb,sr,st] ).

if_then( regular_P, abnormal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,af,afl,at,quiet] ),
   mem( Reg, [avr,quiet,vr,vt] ).

if_then( regular_P, changing, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [mat,wp] ).

if_then( regular_P, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,af,quiet] ).


if_then( rate_of_P, zero, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,af,quiet] ).

if_then( rate_of_P, under_60, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [sa,sad,sb] ).
if_then( rate_of_P, under_60, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,quiet] ),
   mem( Jun, [jb,jeb,quiet] ),
   mem( Reg, [quiet,vr] ).

if_then( rate_of_P, between_60_100, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sa,sad,sr] ),
   mem( Atr, [aeb,quiet,wp] ),
   mem( Jun, [jeb,jr,quiet] ),
   mem( Reg, [avr,quiet] ).
if_then( rate_of_P, between_60_100, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sa,sad,sr] ),
   mem( Atr, [aeb,quiet,wp] ),
   AV = avb3.

if_then( rate_of_P, between_100_250, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,st] ),
   mem( Atr, [aeb,at,mat,quiet] ),
   mem( Jun, [jeb,jt,quiet] ),
   mem( Reg, [quiet,vt] ).
if_then( rate_of_P, between_100_250, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,st] ),
   mem( Atr, [aeb,at,mat,quiet] ),
   AV = avb3.

if_then( rate_of_P, between_250_350, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = afl.

if_then( rate_of_P, over_350, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = af.


if_then( relation_P_QRS, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,af,quiet] ),
   mem( AV, [normal,wpw] ).
if_then( relation_P_QRS, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = afl,
   AV = wpw.

if_then( relation_P_QRS, after_P_always_QRS, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [aeb,at,mat,quiet,wp] ),
   mem( AV, [avb1,lgl,normal,wpw] ),
   mem( Reg, [avr,quiet,vr,vt] ).

if_then( relation_P_QRS, after_P_some_QRS_miss, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [mob2,wen] ).
if_then( relation_P_QRS, after_P_some_QRS_miss, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [af,afl] ),
   AV = normal.

if_then( relation_P_QRS, independent_P_QRS, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = avb3.
if_then( relation_P_QRS, independent_P_QRS, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = af,
   AV = normal.


if_then( regular_PR, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = avb3.
if_then( regular_PR, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,af,afl,quiet] ).

if_then( regular_PR, normal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [aeb,at,mat,quiet,wp] ),
   mem( AV, [mob2,normal] ),
   mem( Jun, [jeb,quiet] ),
   Reg = quiet.

if_then( regular_PR, prolonged, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,wen] ).
if_then( regular_PR, prolonged, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = at,
   AV = normal.

if_then( regular_PR, shortened, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [aeb,at,mat,quiet,wp] ),
   mem( AV, [lgl,wpw] ).
if_then( regular_PR, shortened, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,at,quiet] ),
   AV = normal,
   Reg = quiet.

if_then( regular_PR, changing, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [mat,wp] ),
   AV = normal.

if_then( regular_PR, after_QRS_is_P, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   SA = quiet,
   mem( Atr, [aeb,quiet] ),
   mem( Reg, [avr,quiet,vr,vt] ).


if_then( regular_QRS, normal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,avb3,lgl,mob2,normal,wen] ),
   BB = normal,
   Reg = quiet.

if_then( regular_QRS, wide_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,avb3,lgl,mob2,normal,wen] ),
   BB = lbbb.
if_then( regular_QRS, wide_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Reg, [avr,vfl,vr,vt] ).

if_then( regular_QRS, wide_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,avb3,lgl,mob2,normal,wen] ),
   BB = rbbb.
if_then( regular_QRS, wide_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Reg, [avr,vfl,vr,vt] ).

if_then( regular_QRS, wide_LBBB_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Reg, [avr,vfl,vr,vt] ).

if_then( regular_QRS, delta_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [aeb,afl,at,mat,quiet,wp] ),
   AV = wpw.

if_then( regular_QRS, delta_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( Atr, [aeb,afl,at,mat,quiet,wp] ),
   AV = wpw.

if_then( regular_QRS, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = af,
   AV = wpw.
if_then( regular_QRS, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Reg = vf.


% if_then( rate_of_QRS, zero, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :- fail.

if_then( rate_of_QRS, under_60, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sa,sad,sb] ),
   mem( Atr, [aeb,af,afl,quiet] ),
   mem( AV, [avb1,avb3,lgl,mob2,normal,wen] ),
   mem( Jun, [jb,jeb,quiet] ),
   mem( Reg, [quiet,vr] ).
if_then( rate_of_QRS, under_60, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sr] ),
   mem( Atr, [aeb,quiet,wp] ),
   mem( AV, [avb3,mob2,wen] ),
   mem( Jun, [jb,jeb,quiet] ),
   mem( Reg, [quiet,vr] ).
if_then( rate_of_QRS, under_60, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [sa,sad,sb] ),
   AV = wpw.
if_then( rate_of_QRS, under_60, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = avb3,
   mem( Jun, [jb,jeb,quiet] ),
   mem( Reg, [quiet,vr] ).

if_then( rate_of_QRS, between_60_100, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sa,sad,sr] ),
   mem( Atr, [aeb,af,afl,quiet,wp] ),
   mem( AV, [avb1,avb3,lgl,mob2,normal,wen] ),
   mem( Jun, [jeb,jr,quiet] ),
   mem( Reg, [avr,quiet] ).
if_then( rate_of_QRS, between_60_100, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,st] ),
   mem( AV, [avb3,mob2,wen] ),
   mem( Jun, [jeb,jr,quiet] ),
   mem( Reg, [avr,quiet] ).
if_then( rate_of_QRS, between_60_100, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,sa,sad,sr] ),
   mem( Atr, [aeb,quiet,wp] ),
   AV = wpw.
if_then( rate_of_QRS, between_60_100, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = avb3,
   mem( Jun, [jeb,jr,quiet] ),
   mem( Reg, [avr,quiet] ).

if_then( rate_of_QRS, between_100_250, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,st] ),
   mem( Atr, [aeb,af,afl,at,mat,quiet] ),
   mem( AV, [avb1,avb3,lgl,mob2,normal,wen] ),
   mem( Jun, [jeb,jt,quiet] ),
   mem( Reg, [quiet,vt] ).
if_then( rate_of_QRS, between_100_250, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( SA, [quiet,st] ),
   mem( Atr, [aeb,at,mat,quiet] ),
   AV = wpw.
if_then( rate_of_QRS, between_100_250, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   AV = avb3,
   mem( Jun, [jeb,jt,quiet] ),
   mem( Reg, [quiet,vt] ).

if_then( rate_of_QRS, between_250_350, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Reg = vfl.
if_then( rate_of_QRS, between_250_350, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = afl,
   AV = wpw.

if_then( rate_of_QRS, over_350, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = af,
   AV = wpw.
if_then( rate_of_QRS, over_350, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Reg = vf.


if_then( ectopic_P, abnormal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,lgl,mob2,normal,wen,wpw] ),
   Ect = veb.
if_then( ectopic_P, abnormal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,lgl,mob2,normal,wen,wpw] ),
   Jun = jeb.
if_then( ectopic_P, abnormal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb.

if_then( ectopic_P, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Ect = veb.
if_then( ectopic_P, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Jun = jeb.
if_then( ectopic_P, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :- % none
   mem( Atr, [af,afl,at,mat,quiet,wp] ),
   mem( Jun, [jb,jr,jt,quiet] ),
   Ect = quiet.


if_then( ectopic_PR, normal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   mem( AV, [mob2,normal] ).

if_then( ectopic_PR, prolonged, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   mem( AV, [avb1,normal,wen] ).

if_then( ectopic_PR, shortened, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,lgl,mob2,normal,wen,wpw] ),
   Jun = jeb.
if_then( ectopic_PR, shortened, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   mem( AV, [lgl,normal,wpw] ).

if_then( ectopic_PR, after_QRS_is_P, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,lgl,mob2,normal,wen,wpw] ),
   Ect = veb.
if_then( ectopic_PR, after_QRS_is_P, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   mem( AV, [avb1,lgl,mob2,normal,wen,wpw] ),
   Jun = jeb.

if_then( ectopic_PR, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Ect = veb.
if_then( ectopic_PR, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Jun = jeb.
if_then( ectopic_PR, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   AV = avb3.
if_then( ectopic_PR, meaningless, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :- % none
   mem( Atr, [af,afl,at,mat,quiet,wp] ),
   mem( Jun, [jb,jr,jt,quiet] ),
   Ect = quiet.


if_then( ectopic_QRS, normal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Jun = jeb,
   BB = normal.
if_then( ectopic_QRS, normal, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   mem( AV, [avb1,lgl,mob2,normal,wen] ),
   BB = normal.

if_then( ectopic_QRS, wide_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Ect = veb.
if_then( ectopic_QRS, wide_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Jun = jeb,
   BB = lbbb.
if_then( ectopic_QRS, wide_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   mem( AV, [avb1,lgl,mob2,normal,wen] ),
   BB = lbbb.

if_then( ectopic_QRS, wide_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Ect = veb.
if_then( ectopic_QRS, wide_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Jun = jeb,
   BB = rbbb.
if_then( ectopic_QRS, wide_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   mem( AV, [avb1,lgl,mob2,normal,wen] ),
   BB = rbbb.

if_then( ectopic_QRS, wide_LBBB_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Ect = veb.

if_then( ectopic_QRS, delta_LBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   AV = wpw.

if_then( ectopic_QRS, delta_RBBB, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   AV = wpw.

if_then( ectopic_QRS, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :-
   Atr = aeb,
   AV = avb3.
if_then( ectopic_QRS, absent, arr(SA, Atr, AV, Jun, BB, Reg, Ect) ) :- % none 
   mem( Atr, [af,afl,at,mat,quiet,wp] ),
   mem( Jun, [jb,jr,jt,quiet] ),
   Ect = quiet.

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

