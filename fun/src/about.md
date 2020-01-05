 # About

Modern AI systems are large and complex.  Understanding and maintaining
and improving those complex AI tools is a daunting task.  Teaching
those tools is another daunting task.

But large things are built from smaller parts. To understand complex
things, one tactic is to first look at its smaller elements. Such
smaller elements are easier to understand, maintain, and improve.

To help software engineers and technical manager understand AI
systems (so they can maintain and improve those systems), this book
shows the AI coding elements seen in 20 years of Ph.D. students
from the RAISE research lab (RAISE is short for real-world AI for
SE).  The original version of this code has been applied to many
real-world applications including managing NASA spacecraft and
software, automatic configuration, managing technical debt, bad
smell defection, defect prediction, software effort estimation,
managing cloud computers, reducing the cost of running test suites,
etc.  Yet as shown here, all those systems comprise a small number
of elements (e.g. Recursively cluster, build contrast sets, mutate
towards better sets, etc) which can be used for many tasks including
classification, regression, planning, explanation, monitoring,
privacy, security, optimization, etc.

For industry workers, the various elements of this code can be
readily mixed and matched as required (or replaced, in part, by
code from other sources).  Designed using pipes and filters, this
code avoids   esoteric programming  constructs (no pointers, no
higher-order functions, no monads, nearly no polymorphism or
inheritance).  Hence,

For teachers, all the scripts are very short so they can be quickly
studied as separate units in separate lectures.  This code has been
used for teaching graduate AI classes at North Carolina State
University.  Using this code, it is easy to structure semester-long
"SE for AI" or "AI programming" subjects.  Assignments can be as
simple as "port some of these elements to your favorite programming
language" or as complex as "experimentally compare the methods of
this element with alternate methods taken from the research
literature".
