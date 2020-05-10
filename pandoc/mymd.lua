
function update(b4) 
  if b4 then print(b4) end
  line = io.read()
  if not line then os.exit() end
end

function eats(b4)
  update(b4) 
  _,_,rest = line:find("^--%[%[(.*)")
  if rest then
    repeat
      update(rest)
    until line:find("^--%]%]")
    return eats("")
  end

  _,_,rest = line:find("^-- ")
  if rest then return eats(rest) end
   
  _,_,rest = line:find("-- (    )")
  if rest then
    repeat
      update(rest)
    until not line:find("^-- (    )")
    return eats("")
  end

end

eats()
