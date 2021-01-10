# vim: set et ts=2 sw=2:
r0() = begin Random.seed!(it.seed); true end

function _tests()
  @testset "ALL" begin 
    _lib(); _some(); _sym() end end

function _lib()
  @testset "lib" begin
    @test r0() && few("abcdefgh",2) == ['b','c'] 
    @test thing("string") == "string"
    @test thing(11.5) == 11.5
    all = [row for row in csv("data/weather.csv")] 
    @test 5       == length(all[1])
    @test 15      == length(all)
    @test Float64 == typeof(all[2][2])
  end 
end 

function _sym()
  @testset "some" begin
    s=  col(txt="a") 
    @test typeof(s)==Sym
    for i in "aaaabbc" inc!(s,i) end
    @test s.mode == 'a'
    @test s.most == 4
  end 
end 

function _some()
  @testset "some" begin
    s=  col(txt="<a") 
    @test typeof(s)==Some
    s.max = 32
    for i in 1:1000 inc!(s,int(100*rand())) end
    lst = all(s)
    @test lst[1] < lst[end]
    @test mid(s,lo=16) == 69
    @test 32.6 < sd(s) < 32.7
    @test s.w == -1
  end 
end 
