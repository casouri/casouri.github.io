#!/usr/bin/env fish

for file in src/*.html.pm
    set day (basename $file .html.pm)
    if test ! -e $day
        mkdir $day
    end
    if test ! -e $day"/index.html.pm"
        ln $file $day"/index.html.pm"
    end
end
            
