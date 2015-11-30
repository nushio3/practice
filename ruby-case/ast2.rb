#!/usr/bin/env ruby
# coding: utf-8

# マッチするかどうかを判定することと、値を返すことが必要


def supermatch(x)

end

def binop(&block)
  ret = Object.new()
  ret.instance_variable_set('@k', block)
  def ret.===(other)
    @k.call('*', 6, 7)
    return 114514
  end
  def ret.|(other)
    p "invoke or!"
  end
  return ret
end

def uniop(&block)
  ret = Object.new()
  ret.instance_variable_set('@k', block)
  def ret.===(other)
    @k.call('*', 6, 7)
    return 114514
  end
  return ret
end


supermatch binop {|op, a, b|
  p "called #{op} #{a} #{b}"
  case op
  when '+'
    a+b
  when '*'
    a*b
  end
} | uniop {|op, a|
  p "called #{op} a"
}


binop do |x|
  p x
end

def gimme_block(&blk)
  if blk
  else
    p "noblk!"
  end
end

gimme_block()
