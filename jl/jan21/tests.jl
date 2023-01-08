# vim: set et ts=2 sw=2:
r0() = Random.seed!(it.seed)

ok() = begin
  @testset "ALL" begin 
    _lib(); _some(); _sym() end end

_lib() = begin
  r0()
  @testset "lib" begin
    @test few("abcdefgh",2) == ['a','c'] 
    @test thing("string") == "string"
    @test thing(11.5) == 11.5
    all = [row for row in csv("data/weather.csv")] 
    @test 5       == length(all[1])
    @test 15      == length(all)
    @test Float64 == typeof(all[2][2]) end end 

_sym() = begin
  @testset "some" begin
    s=  col(txt="a") 
    @test typeof(s)==Sym
    for i in "aaaabbc" inc!(s,i) end
    @test s.mode == 'a'
    @test s.most == 4 end end 

_some() = begin
  r0()
  @testset "some" begin
    s=  col(txt="<a") 
    @test typeof(s)==Some
    s.max = 32
    for i in 1:1000 inc!(s,int(100*rand())) end
    lst = all(s)
    @test lst[1] < lst[end]
    @test mid(s,lo=16) == 79
    @test 31.2 < sd(s) < 31.3
    @test s.w == -1
    @test .75 < norm!(s,75) < .76 end end 
