#!/usr/bin/ruby

# Offline Wikipedia title Splitter for English
# by KAWABATA, Taichi

while gets
  if $_ =~ /<title/
    begin
      $_ =~ /<title>([^<]+)<\/title>/
      print "!",$1,"\n"
      array = $1.split(" ")
      if (array.length > 1)
        array.each {|str|
          if (str =~ /^[^a-z]/) || (str.length > 3)
            print str,"\n"
          end
        }
      end
    rescue
      $stderr.print("error rescued!\n")
    end
  end
end
