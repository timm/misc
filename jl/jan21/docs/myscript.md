
# myscript.jl

in file 'myscript.jl'

```julia
push!( LOAD_PATH, "./" )
import MyPrecompiledModule
println( "Hello from the script. The arguments passed into it were $ARGS" )
MyPrecompiledModule.exportedfun()
````

