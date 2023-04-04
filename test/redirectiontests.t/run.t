Read in the text.txt file, grep for a specific word and write to output
  $ oshell2 -c "grep illo < text.txt"
  Occaecati ex ut dolorum et illo. 

Try again but this time writing to a file
  $ oshell2 -c "grep illo < text.txt > outfile.txt"; cat outfile.txt
  Occaecati ex ut dolorum et illo. 

Try again but writing errors to a file
  $ oshell2 -c "grep illo < doesnotexist.txt"
  Fatal error: exception Unix.Unix_error(Unix.ENOENT, "open", "doesnotexist.txt")
  [2]
