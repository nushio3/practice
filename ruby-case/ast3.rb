#!/usr/bin/env ruby

class ADT
  attr_accessor :constructor, :argv, :metadata
  def =~(pattern)
    pattern.match(self)
  end
end

class Pattern
  attr_accessor :accept_continuation, :alternative
  def match(x)
    if self === x
      return @accept_continuation.call(*x.argv)
    elsif @alternative
      return @alternative.match(x)
    else
      return nil
    end
  end
  def | (other)
    @alternative=other
    return self
  end
end

def Binop(*argv, &block)
  if(block) # used as pattern
    ret = Pattern.new()
    ret.accept_continuation = block
    def ret.===(x)
      return x.constructor == :binop && String === x.argv[0]
    end
    return ret
  else # used as constructor
    ret = ADT.new()
    ret.constructor = :binop
    ret.argv = argv
    return ret
  end
end

def Imm(*argv, &block)
  if(block) # used as pattern
    ret = Pattern.new()
    ret.accept_continuation = block
    def ret.===(x)
      return x.constructor == :imm
    end
    return ret
  else # used as constructor
    ret = ADT.new()
    ret.constructor = :imm
    ret.argv = argv
    return ret
  end
end

def evArith(expr)
  expr =~ Imm {|x|
    x
  } | Binop {|op, x, y|
    case op
    when '+'
      evArith(x) + evArith(y)
    when '*'
      evArith(x) * evArith(y)
    else
      p "error: unknown operator " + op + " at position " + expr.metadata
    end
  }
end


expr = Binop('*', Imm(6), Binop('+', Imm(3), Imm(4)))

badExpr = Binop('>>', Imm(1), Imm(2))
badExpr.metadata = '77:17'


p evArith(expr)

p evArith(Binop('+', expr, badExpr))
