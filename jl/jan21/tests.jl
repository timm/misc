# vim: set et ts=2 sw=2:

function _tests()
  r0() = begin Random.seed!(it.seed); true end
  r0()
  @testset "ALL      " begin
     _lib()
   end
end

function _lib()
  @testset "lib       " begin
    @test r0() && few("abcdefgh",2) == ['b','c'] 
    @test thing("string") == "string"
    @test thing(11.5) == 11.5
    all = [row for row in csv("data/weather.csv")] 
    @test 5       == length(all[1])
    @test 15      == length(all)
    @test Float64 == typeof(all[2][2])
  end 
end

1
