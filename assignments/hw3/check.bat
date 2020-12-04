scala -cp classes pp202002.hw3test.Test > my.txt 
cd C:\Users\wnsdud\Desktop\dfa
scala -cp classes pp202002.hw3test.Test > your.txt 
cd C:\Users\wnsdud\Documents\GitHub\pp202002-private\assignments\hw3
diff (cat .\my.txt) (cat C:\Users\wnsdud\Desktop\dfa\your.txt)              
