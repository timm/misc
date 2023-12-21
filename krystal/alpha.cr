class Col 
  @n=0
  @at=0
  @txt="" end

class Num < Col
  @mu=0.0
  @m2=0.0
  @sd=0.0
  def add(x : _)
    if x != "?"
      @n  += 1
      d    = x - @mu
      @mu += d/@n 
      @m2 += d*(x - @mu) 
      @sd  = @n==1 ? 0 : (@m2/(@n - 1)) ** 0.5 end end end


n1= Num.new
n2= Num.new
n1.add(23)
