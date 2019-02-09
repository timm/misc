class String  
  def xxx  
    self.size  
  end  
end  

def hello1 a: 'aa', b:'bb', c: 'world'
  'Hello ' + a + b + c  
end  

puts hello1 b: 'timm'

puts  "Tell me my size!".xxx  
