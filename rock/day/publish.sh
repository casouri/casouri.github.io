#!/usr/bin/env fish

for file in src/*.html.pm
    set source (basename $file .html.pm)
    if test ! -e $source
        mkdir $source
    end
    set dest (basename $file .html.pm)
    if test ! -e $dest
        ln $file $dest"/index.html.pm"
    end
end
            
