
# MyPrecompiledModule.jl

in file 'MyPrecompiledModule.jl' (e.g. in the same directory as myscript.jl)

```julia
__precompile__()
module MyPrecompiledModule
  export exportedfun;
  using ArgParse;
  function innerfun()
    println("Hello from MyPrecompiledModule.innerfun");
  end

  function main()
    for x in ARGS print(x) end
    s = ArgParseSettings()
    @add_arg_table s begin
        "-k"
            help = "low freeuncy kudge"
            arg_type = Int
            default = 2
        "-m"
            help = "low freeuncy kudge"
            arg_type = Int
            default = 1
        "-v"
            help = "set veborse"
            action = :store_true
    end
    
    args = parse_args(ARGS, s)
    
    for (arg,val) in args
            println("  $arg  =>  $val")
    end
  end

  function exportedfun()
    innerfun()
    print("Hello from MyPrecompiledModule.exportedfun");
  end
end
````

