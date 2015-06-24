class Env < Hash
  def initialize(keys=[], vals=[], outer=nil)
    @outer = outer
    keys.zip(vals).each{|p| store(*p)}
  end
  def [](name)  super(name) || @outer[name]  end
  def set(name, value) key?(name) ? store(name, value) : @outer.set(name, value)  end
end

def add_globals(env)
  ops = [:+, :-, :*, :/, :>, :<, :>=, :<=, :==]
  ops.each{|op|  env[op] = lambda{|a, b| a.send(op, b)}}
  env.update({ :length => lambda{|x| x.length}, :cons => lambda{|x, y| [x]+y},
  :car => lambda{|x| x[0]}, :cdr => lambda{|x| x[1..-1]}, :append => lambda{|x,y| x+y},
  :list => lambda{|*xs| xs}, :list? => lambda{|x| x.is_a? Array}, :null? => lambda{|x| x==nil},
  :symbol? => lambda{|x| x.is_a? Symbol}, :not => lambda{|x| !x}, :display => lambda{|x| p x}})
end

def eval(x, env)
  return env[x] if x.is_a? Symbol
  return x if !x.is_a? Array
  case x[0]
    when :quote then x[1..-1]
    when :if
      _, test, conseq, alt = x
      eval(eval(test, env) ? conseq : alt, env)
    when :set! then env.set(x[1], eval(x[2], env))
    when :define then env[x[1]] = eval(x[2], env)
    when :lambda
      _, vars, exp = x
      Proc.new{|*args| eval(exp, Env.new(vars, args, env))}
    when :begin
      x[1..-1].inject([nil, env]){|val_env, exp| [eval(exp, val_env[1]), val_env[1]]}[0]
    when :callcc
      f = eval(x[1], env)
      callcc {|cont| f.call( lambda{|x| cont.call(x)} ) }
    else
      exps = x.map{|exp| eval(exp, env)}
      exps[0].call(*exps[1..-1])
  end
end

def atom(s)
  return "[" if s=='('
  return "]" if s==')'
  return s if s =~ /^-?\d+$/ || s =~ /^-?\d*\.\d+$/
  ':'+s
end

def parse(src)
  tokens = src.gsub('(', ' ( ').gsub(')', ' ) ').split
  Kernel.eval(tokens.map{|s| atom(s)}.join(' ').gsub(' ]',']').gsub(/([^\[]) /,'\1, '))
end

if ARGV.size > 0
  src = open(ARGV[0], 'r'){|f| f.read }
  p(eval(parse(src), add_globals(Env.new)))
else
  print "usage: rispy.rb file.scm\n"
end