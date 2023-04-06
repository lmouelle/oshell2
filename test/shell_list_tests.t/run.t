This file is for testing shell list implentation. A shell list is a list of commands chainged together
with the && and || symbols, or the & and ; symbols. These implement conditionals and backgrounding respectively

Test that foregrounding chains properly
  $ oshell2 -c "grep illo < text.txt ; ls | grep notexists ;"
  Occaecati ex ut dolorum et illo. 
  [1]

Test that backgrounding and foregrounding mix properly
  $ oshell2 -c "grep illo < text.txt ; ls &"
  Occaecati ex ut dolorum et illo. 
  text.txt

Test that simple commands can implement backgrounding
  $ oshell2 -c "ls &"
  text.txt

  $ oshell2 -c "ls ; ls ;"
  text.txt
  text.txt

