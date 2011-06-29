#!/usr/bin/ruby -pi

$_ = $_.gsub(/^(\s+)/) do |m|
  m.gsub(/\t/, ' '*4)
end.gsub(/(\s+)$/,"\n")
