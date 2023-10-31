KARDIO

The KARDIO system was designed to diagnose cardiac arrhythmias from symbolic descriptions of electrocardiograms. Major parts of the system were later reimplemented into three small, compatible subsystems which can be used independently from each other (gzipped Prolog):

- kardio_dm.pl - a deep model of the heart which simulates its electrical activity and derives ECG descriptions for any cardiac arrhythmia,
- kardio_h4.pl - a model of the heart represented at four levels of abstraction for hierarchical diagnosis,
- kardio_sd.pl - a surface knowledge base for diagnosis, derived automatically from the deep model.

These programs are written in standard Prolog (e.g., C-Prolog, Quintus Prolog, SICStus Prolog). As experimental, research software, these programs are provided free of charge on an "as is" basis without warranty of any kind. There is no unanimous agreement between expert cardiologists regarding the correctness of the KARDIO model. The author cannot accept any liability regarding applications derived from this model. All title, ownership and rights to KARDIO and any copies remain with the author. When publishing any results using KARDIO, this will be properly acknowledged and the following publications (whichever is relevant) will be referenced:

Bratko, I., Mozetic, I., Lavrac, N. KARDIO: A Study in Deep and Qualitative Knowledge for Expert Systems. The MIT Press, Cambridge, MA, 1989.
Mozetic, I. Diagnostic efficiency of deep and surface knowledge in KARDIO. Artificial Intelligence in Medicine 2 (2), pp. 67-83, 1990.

See KARDIO citations.

Last modified: 21 May 1992 by Igor Mozetic
