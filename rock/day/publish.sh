#!/usr/bin/env fish

for file in src/*.html.pm
    mkdir (basename $file .html.pm)
    ln $file (basename $file .html.pm)/index.html.pm
end
            
