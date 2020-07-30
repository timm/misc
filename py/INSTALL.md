# Install

usage="sh INSTALL.md"  
mac=1  
unix=0  
tricks=0  

if [ "$mac" -eq 1 -o "$unix" -eq 1 ]; then  
  sudo pip3 install docopt  #required  
  sudo pip3 install rerun pdoc3 # optional  
fi   

[ "$tricks" -eq 0 ] && exit 0
 
echo tricks 
