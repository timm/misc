x="as"
var a=1,b = 2
say=console.log
say(a +b +"Hello " + ["world", "!"])

var myObj = {key1: "Hello", key2: "World"};
say(myObj)


for (i = 0; i < 10; i++) {
    for (j = 0; j < 10; j++) {
            if (i == 5 && j ==5) { say(i,j)
                        break ;
                                            }
                                                }
                                                }

description = "";
person = {fname:"Paul", lname:"Ken", age:18};
for (x in person){
    description += person[x] + " ";
    } // description = 'Paul Ken 18 '

say(description)
