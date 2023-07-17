:- dynamic docstring/2.
:- dynamic docstring/1.
:- dynamic comment/1.

/**
 * pycco_generate/1 generates documentation for Prolog code.
 *
 * @param Files A list of Prolog files to generate documentation for.
 */
pycco_generate(Files) :-
    pycco_setup,
    pycco_parse_files(Files),
    pycco_generate_output.

/**
 * pycco_setup/0 sets up the necessary environment for documentation generation.
 */
pycco_setup :-
    dynamic(docstring/2),
    dynamic(comment/1),
    op(900, fx, comment),
    op(900, fx, section),
    op(900, fx, code).

/**
 * pycco_parse_files/1 parses the Prolog files and extracts documentation information.
 *
 * @param Files A list of Prolog files to parse.
 */
pycco_parse_files([]).
pycco_parse_files([File|Rest]) :-
    pycco_parse_file(File),
    pycco_parse_files(Rest).

/**
 * pycco_parse_file/1 parses a single Prolog file and extracts documentation information.
 *
 * @param File The Prolog file to parse.
 */
pycco_parse_file(File) :-
    open(File, read, Stream),
    repeat,
    read(Stream, Term),
    (   Term = end_of_file -> true
    ;   pycco_process_term(Term),
        fail
    ),
    close(Stream).

/**
 * pycco_process_term/1 processes a single term and extracts documentation information.
 *
 * @param Term The Prolog term to process.
 */
pycco_process_term(comment(Text)) :-
    assertz(comment(Text)),
    fail.
pycco_process_term(docstring(Text)) :-
    assertz(docstring(Text)),
    fail.
pycco_process_term(section(Section)) :-
    assertz(comment('% ')),
    assertz(comment(Section)),
    assertz(comment(':-')),
    assertz(comment(' ')),
    assertz(comment('docstring(_).')),
    fail.
pycco_process_term(code(_)).

/**
 * pycco_generate_output/0 generates the documentation output based on the extracted information.
 */
pycco_generate_output :-
    findall(Comment, comment(Comment), Comments),
    findall(Docstring, docstring(Docstring), Docstrings),
    pycco_render_output(Comments, Docstrings).

/**
 * pycco_render_output/2 renders the documentation output based on the extracted information.
 *
 * @param Comments   The list of comments.
 * @param Docstrings The list of docstrings.
 */
pycco_render_output(Comments, Docstrings) :-
    pycco_render_comments(Comments),
    pycco_render_docstrings(Docstrings).

/**
 * pycco_render_comments/1 renders the comments section of the documentation.
 *
 * @param Comments The list of comments to render.
 */
pycco_render_comments([]).
pycco_render_comments([Comment|Rest]) :-
    format('~w~n', [Comment]),
    pycco_render_comments(Rest).

/**
 * pycco_render_docstrings/1 renders the docstrings section of the documentation.
 *
 * @param Docstrings The list of docstrings to render.
 */
pycco_render_docstrings([]).
pycco_render_docstrings([Docstring|Rest]) :-
    format('~n~w~n', [Docstring]),
    pycco_render_docstrings(Rest).

