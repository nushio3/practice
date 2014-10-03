# -*- coding: utf-8 -*-
require 'jubatus/regression/client'
require 'yaml'
require 'pp'

def score(xs)
  sum = 0.0
  (0..9).each{|i|
    sum += (xs[i] - i) ** 2
  }
  return sum
end


cli = Jubatus::Regression::Client::Regression.new "127.0.0.1", 9199, "Kumagi-Muranushi"

loop{
  xs = []
  10.times{
    xs << 10*rand()
  }
  
  kvp={}

  sum = 0.0
  xs.each{|x| sum += x*x}
  kvp['a']=sum
  10.times{|i|
    kvp["b#{i}"]=xs[i]
  }
  kvp['c'] = 1.0

  datum = Jubatus::Common::Datum.new(kvp)

  p xs
  result = cli.estimate [datum]
  puts "estimate = #{result[0]}; actual = #{score(xs)}"


  cli.train [[score(xs),datum]]


  
}
