#!/usr/bin/ruby

# Offline Wikipedia title Splitter for Japanese (May be used with English).
# by KAWABATA, Taichi 

require 'MeCab'
c = MeCab::Tagger.new(ARGV.join(" "))

while gets
  if $_ =~ /<title/
    $_ =~ /<title>([^<]+)<\/title>/
    print "!",$1,"\n"
    begin
      n = c.parseToNode($1)
      if (n.next != nil) ||
         (n.next.next != nil) ||
         (n.next.next.next != nil) 
        while n do
          if n.surface.length > 3
            print n.surface,"\n"
          end
          n = n.next
        end
      end
    rescue
      $stderr.print "error rescued!\n"
    end
  end
end
