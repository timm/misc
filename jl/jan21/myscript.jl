# in file 'myscript.jl'
push!( LOAD_PATH, "./" )
import MyPrecompiledModule
println( "Hello from the script. The arguments passed into it were $ARGS" )
MyPrecompiledModule.exportedfun()
