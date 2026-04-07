def f1(); f2() end
def f2(); f3() end
def f3(); f4() end
def f4(); f5() end
def f5(); f6() end
def f6(); f7() end
def f7(); f8() end
def f8(); f9() end 
def f9(); f10() end
def f10(); return a[10][2]  end   # boom

puts f1()
