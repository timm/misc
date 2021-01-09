# vim: set et ts=2 sw=2:

function ok()
    @testset "ALL      " begin
      @testset "trig      " begin
        θ = 2/3*π
        @test_skip sin(-θ) ≈ -sin(θ)
        @test cos(-θ) ≈ cos(θ)
        @test 1 == 2
        @test sin(2θ) != 2*sin(θ)*cos(θ)
        @test cos(2θ) ≈ cos(θ)^2 - sin(θ)^2
      end

      @testset "other     " begin
        θ = 2/3*π
        @test sin(-θ) ≈ -sin(θ)
        @test cos(-θ) ≈ cos(θ)
        @test sin(2θ) != 2*sin(θ)*cos(θ)
        @test cos(2θ) ≈ cos(θ)^2 - sin(θ)^2 end end end 
