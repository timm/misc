# jocco.jl - Literate documentation generator in Julia

const code_sep       = "# CUT HERE\n"
const code_sep_html  = "<span class=\"c\"># CUT HERE</span>\n"
const docs_sep       = "\n##### CUT HERE\n\n"
const docs_sep_html  = r"<h5 id=\"cut-here.*\">CUT HERE</h5>\n"

const header = """<!DOCTYPE html>
<html>
<head>
  <title>%title%</title>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <link rel="stylesheet" href="jocco.css" />
  <script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
</head>
<body>
  <div id="container">
    <div id="background"></div>
    <table>
      <thead>
        <tr>
          <th class="docs"><h1>%title%</h1></th>
          <th class="code"></th>
        </tr>
      </thead>
      <tbody>
"""

const table_entry = """
<tr id="section-%index%">
<td class="docs">
  <div class="pilwrap"><a class="pilcrow" href="#section-%index%">&#182;</a></div>
  %docs_html%
</td>
<td class="code"><div class="highlight"><pre>%code_html%
</pre></div></td>
</tr>
"""

const footer = """
      </tbody>
    </table>
  </div>
</body>
</html>"""

function parse_source(source)
    code, docs = String[], String[]
    has_code = false
    code_text, docs_text = "", ""

    for line in eachline(source)
        line = chomp(line)
        m = match(r"^\s*(?:#\s(.*?)\s*$|$)", line)
        if m === nothing
            m = match(r"^\s*#()$", line)
        end
        if m === nothing || m.captures == (nothing,)
            has_code = true
            code_text *= "$line\n"
        else
            if has_code
                push!(code, code_text)
                push!(docs, docs_text)
                has_code = false
                code_text, docs_text = "", ""
            end
            doc_line = m.captures[1]
            if doc_line !== nothing
                docs_text *= "$doc_line\n"
            end
        end
    end
    push!(code, code_text)
    push!(docs, docs_text)
    return code, docs
end

function highlight(text_array, sep_in, sep_out, cmd)
    input  = IOBuffer(join(text_array, sep_in))
    output = IOBuffer()
    run(pipeline(cmd, stdin=input, stdout=output))
    split(String(take!(output)), sep_out)
end

function highlight_code(code)
    cmd = `pygmentize -l julia -f html -O encoding=utf8`
    code = highlight(code, code_sep, code_sep_html, cmd)
    if !isempty(code)
        code[1] = replace(code[1], "<div class=\"highlight\"><pre>", "")
        code[end] = replace(code[end], "</pre></div>", "")
    end
    unshift!(code, "")
    return code
end

function get_files_with_extension(dir, ext)
    files = readdir(dir)
    return [joinpath(dir, f) for f in files if endswith(f, ext)]
end

function join_arg_vals(arg, vals)
    args = String[]
    for v in vals
        push!(args, arg, v)
    end
    return args
end

function highlight_docs(docs, path)
    bibs = get_files_with_extension(path, ".bib")
    csls = get_files_with_extension(path, ".csl")
    filters = get_files_with_extension(path, ".hs")

    args = ["-S"; join_arg_vals("--bibliography", bibs);
                 join_arg_vals("--csl", csls);
                 "-f"; "markdown"; "-t"; "json"]
    cmd = `pandoc $(args...)`
    for f in filters
        cmd = cmd | `runhaskell $f`
    end
    cmd = cmd | `pandoc -S --mathjax -f json -t html`
    return highlight(docs, docs_sep, docs_sep_html, cmd)
end

function generate_html(source, path, file, code, docs, jump_to)
    outfile = joinpath(path, replace(file, r"\.jl$", ".html"))
    open(outfile, "w") do f
        write(f, replace(header, "%title%", String(source)))
        lines = max(length(code), length(docs))
        while length(code) < lines; push!(code, ""); end
        while length(docs) < lines; push!(docs, ""); end

        for i in 1:lines
            t = replace(table_entry, "%index%", string(i))
            t = replace(t, "%docs_html%", String(docs[i]))
            t = replace(t, "%code_html%", String(code[i]))
            write(f, t)
        end
        write(f, footer)
    end
    println("$file --> $outfile")
end

function generate_documentation(source, path, file, jump_to)
    code, docs = parse_source(source)
    code = highlight_code(code)
    docs = highlight_docs(docs, path)
    generate_html(source, path, file, code, docs, jump_to)
end

function main()
    jump_to = ""
    for source in ARGS
        file = chomp(read(`basename $source`, String))
        path = joinpath(chomp(read(`dirname $source`, String)), "docs")
        run(`mkdir -p $path`)
        generate_documentation(source, path, file, jump_to)
    end
end

main()
