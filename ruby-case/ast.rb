#!/usr/bin/env ruby
# coding: utf-8

# マッチするかどうかを判定することと、値を返すことが必要

def binop(&block)
  ret = Object.new()
  ret.instance_variable_set('@k', block)
  def ret.===(other)
    @k.call('*', 6, 7)
    return 114514
  end
  return ret
end

p case []
  when binop {|op, a, b|
         p "called #{op} #{a} #{b}"
         case op
         when '+'
           a+b
         when '*'
           a*b
         end
       }
    "hoge"
  end


binop do |x|
  p x
end
