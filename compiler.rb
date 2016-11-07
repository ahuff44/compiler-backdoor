#!/usr/bin/env ruby


# Token = Struct.new(:type, :val) do
#   def initialize
#     raise "iae: bad token: #{self}" unless token_types
#     super
#   end
# end

RESERVED = %w(true false set def if while print return)
SYMBOLS = %w(+ - * = < > ; ( ) , { })

def tokenize(text)
  # tokenize :: String -> [ Token ]
  # Token
  #   = (:integer, Integer)
  #   | (:symbol, String)
  #   | (:identifier, String)
  #   | (:string, String)
  #   | (:reserved, String)

  state = :none
  buffer = []
  text.chars.each_with_object([]) do |char, tokens|
    # p [char, state, buffer]

    case state
    when :comment
      if char == "\n"
        state = :none
      end
    when :string
      if char == '\\'
        state = :escape_char
      elsif char != '"'
        buffer << char
      else
        tokens << [state, buffer.join]
        state = :none
      end
    when :escape_char
      buffer << case char
        when "n"
          "\n"
        when "\\"
          "\\"
        when "\""
          "\""
        else
          char
        end
      state = :string
    when :integer
      if char =~ /\d/
        buffer << char
      else
        tokens << [state, Integer(buffer.join)]
        state = :none
        redo
      end
    when :symbol
      tokens << [state, char]
      state = :none
    when :identifier
      if char =~ /[_a-zA-Z0-9]/
        buffer << char
      else
        val = buffer.join
        if RESERVED.include?(val)
          tokens << [:reserved, val]
        else
          tokens << [state, val]
        end
        state = :none
        redo # re-process this character; e.g. "+" in "3+4"
      end
    when :none
      case char
      when /\s/
        # no-op
      when '#'
        state = :comment
      when '"'
        state = :string
        buffer = []
      when /\d/
        state = :integer
        buffer.clear
        redo
      when /[_a-zA-Z]/
        state = :identifier
        buffer.clear
        redo
      when ->(c) { SYMBOLS.include?(c) }
        state = :symbol
        buffer.clear
        redo
      else
        raise "unrecognized character: #{char}"
      end
    else
      raise "iae: illegal state: #{state}"
    end
  end
end

def parse(tokens)
  tokens = [[:symbol, '{']] + tokens + [[:symbol, '}']] # sigil for parsing a scope
  tokens = tokens.reverse # makes next_token! etc easier
  parse_!(:scope, tokens)
end

def next_token!(tokens)
  tok = tokens.pop
  if block_given?
    yield tok
  end
  tok
end

def peek(tokens)
  tokens.last
end

def parse_!(state, tokens)
  # p ['parse_!', state, tokens.reverse]

  case state
  when :scope
    next_token!(tokens) { |tok| raise "scope missing '{': #{tok[1]}" unless tok == [:symbol, '{'] }
    ast = []
    loop do
      if peek(tokens) == [:symbol, "}"]
        next_token!(tokens)
        return [:scope, ast]
      end
      ast << parse_!(:statement, tokens)
    end
  when :set
    target = next_token!(tokens) { |(type, val)| raise "bad set target #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing '=': #{tok[1]}" unless tok == [:symbol, '='] }
    rhs = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:set, target, rhs]
  when :expression
    type, val = tok = next_token!(tokens)
    case type
    when :string
      lhs = tok
    when :integer
      lhs = tok
    when :reserved
      raise "reserved word in expression: #{val}" unless val == 'true' or val == 'false'
      lhs = tok
    when :identifier
      type2, val2 = tok2 = peek(tokens)
      if tok2 == [:symbol, '(']
        tokens << tok # put func name back
        lhs = parse_!(:call, tokens)
      else
        lhs = tok
      end
    when :symbol
      if tok == [:symbol, '(']
        lhs = parse_!(:expression, tokens)
        next_token!(tokens) { |tok| raise "missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
      end
    else
      raise "iae"
    end

    # p ['lhs', lhs, tokens.reverse]

    type, val = tok = next_token!(tokens)
    if type == :symbol
      case val
      when '+'
        [:binop, tok, lhs, parse_!(:expression, tokens)]
      when '-'
        [:binop, tok, lhs, parse_!(:expression, tokens)]
      when '*'
        [:binop, tok, lhs, parse_!(:expression, tokens)]
      when '<'
        [:binop, tok, lhs, parse_!(:expression, tokens)]
      when '>'
        [:binop, tok, lhs, parse_!(:expression, tokens)]
      when '='
        next_token!(tokens) { |tok| raise "unexpected '=': #{tok[1]}" unless tok == [:symbol, '='] }
        [:binop, [:symbol, '=='], lhs, parse_!(:expression, tokens)]
      else # e.g. ')', ';', or ','
        tokens << tok # put it back
        lhs
      end
    else
      raise "bad expression: #{val}"
    end
  when :call
    name_ = next_token!(tokens) { |type, val| raise "bad call name: #{name_}: #{tok[1]}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing '(': #{tok[1]}" unless tok == [:symbol, '('] }
    args = parse_!(:explist, tokens)
    next_token!(tokens) { |tok| raise "missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
    [:call, name_, args]
  when :print
    args = parse_!(:explist, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:print, args]
  when :explist
    if peek(tokens) == [:symbol, ")"]
      []
    else
      buffer = []
      loop do
        buffer << parse_!(:expression, tokens)
        popped = next_token!(tokens)
        if popped != [:symbol, ","]
          tokens << popped # return the token
          return buffer
        end
      end
    end
  when :arglist
    if peek(tokens) == [:symbol, ")"]
      []
    else
      buffer = []
      loop do
        buffer << next_token!(tokens) { |type, val| raise "bad arg: #{val}" unless type == :identifier }
        popped = next_token!(tokens)
        if popped != [:symbol, ","]
          tokens << popped # return the token
          return buffer
        end
      end
    end
  when :def
    name_ = next_token!(tokens) { |(type, val)| raise "bad function name: #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "def '#{name_[1]}' missing '(': #{tok[1]}" unless tok == [:symbol, '('] }
    params = parse_!(:arglist, tokens)
    next_token!(tokens) { |tok| raise "def '#{name_[1]}' missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
    body = parse_!(:scope, tokens)
    [:def, name_, params, body]
  when :if
    next_token!(tokens) { |tok| raise "'if' missing '(': #{tok[1]}" unless tok == [:symbol, '('] }
    cond = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "'if' missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
    then_ = parse_!(:scope, tokens)
    if peek(tokens) == [:identifier, 'else']
      next_token!(tokens) # consume 'else'
      else_ = parse_!(:scope, tokens)
    end
    [:if, cond, then_, else_]
  when :while
    cond = parse_!(:expression, tokens)
    body = parse_!(:scope, tokens)
    [:while, cond, body]
  when :return
    val = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:return, val]
  when :statement
    type, val = tok = next_token!(tokens)
    case type
    when :reserved
      case val
      when "set"
        parse_!(:set, tokens)
      when "print"
        parse_!(:print, tokens)
      when "def"
        parse_!(:def, tokens)
      when "if"
        parse_!(:if, tokens)
      when "while"
        parse_!(:while, tokens)
      when "return"
        parse_!(:return, tokens)
      else
        raise "unknown reserved word: #{val}"
      end
    when nil
      raise "unexpected end of input"
    else
      raise "unexpected statement starting at: #{val}; tokens=#{tokens.reverse}"
    end
  else
    raise "unimplemented state: #{state}"
  end
end

def codegen_list(list, sep)
  "#{list.map{|elem| codegen(elem)}.join(sep)}"
end

def generate_code(node)
  codegen(node)
end

def codegen(node)
  # p ["codegen", node]

  node_type = node.shift
  case node_type
  when :scope
    body, = node
    "#{body.map{|elem| codegen(elem)}.join("\n")}"
  when :set
    target, rhs = node
    "#{codegen(target)} = #{codegen(rhs)};"
  when :binop
    type, lhs, rhs = node
    "(#{codegen(lhs)} #{codegen(type)} #{codegen(rhs)})"
  when :call
    name_, args = node
    name_str = codegen(name_)
    case name_str # shim in javascript methods
    when '_array'
      "[#{codegen_list(args, ', ')}]"
    when '_include'
      arr, elem = args
      "#{codegen(arr)}.includes(#{codegen(elem)})"
    when '_push'
      arr, elem = args
      "#{codegen(arr)}.push(#{codegen(elem)})"
    when '_pop'
      arr, elem = args
      "#{codegen(arr)}.pop(#{codegen(elem)})"
    when '_map'
      arr, fxn = args
      "#{codegen(arr)}.map(#{codegen(fxn)})"
    when '_get'
      arr, ix = args
      "#{codegen(arr)}[#{codegen(ix)}]"
    else
      "#{name_str}(#{codegen_list(args, ', ')})"
    end
  when :print
    args, = node
    "console.log(#{codegen_list(args, ', ')});"
  when :def
    name_, params, body = node
    "function #{codegen(name_)}(#{codegen_list(params, ', ')}) {\n#{codegen(body)}\n}"
  when :if
    cond, then_, else_ = node
    fst = "if #{codegen(cond)} {\n#{codegen(then_)}\n}"
    snd = if else_ then " else {\n#{codegen(else_)}\n}" else "" end
    fst + snd
  when :while
    cond, body = node
    "while (#{codegen(cond)}) {\n#{codegen(body)}\n}"
  when :return
    val, = node
    "return #{codegen(val)};"
  when :integer
    val, = node
    "#{val}" # @todo
  when :string
    val, = node
    "#{val}" # @todo
  when :reserved
    val, = node
    "#{val}" # @todo
  when :symbol
    val, = node
    if val == '=='
      val = '==='
    end
    "#{val}"
  when :identifier
    varname, = node
    "#{varname}" # @todo
  else
    raise "iae: unrecognized node type: #{node_type}"
  end
end

infile, outfile = ARGV
text = File.read(infile)
# puts "\ntext loaded:\n#{text}"
tokens = tokenize(text)
# puts "\ntokens:\n#{tokens}"
ast = parse(tokens)
# puts "\nast:\n#{ast}"
code = generate_code(ast)
# puts "\ncode:\n#{code}"
File.write(outfile, code)
