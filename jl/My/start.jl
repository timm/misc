import Pkg
let
    pkgs = ["Revise", "Parameters", "OhMyREPL"]
    for pkg in pkgs
    if Base.find_package(pkg) === nothing
        Pkg.add(pkg)
    end
    end
end

println("go!")

#Pkg.activate(".")
#using Revise
#using OhMyREPL
