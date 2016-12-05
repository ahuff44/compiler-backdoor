#!/usr/bin/env ruby


RESERVED = %w(true false let def if while return break do raise alloc)
SYNTAX = %w(= ; ( ) , { })
BINOPS = %w(    . *  +  -  == != <  >  <= >= && ||)
BINOP_PREC = %w(0 10 20 20 30 30 30 30 30 30 40 50).map(&:to_i)
ALL_SYMBOLS = SYNTAX + BINOPS

def is_symbol_character(char)
  ALL_SYMBOLS.any? { |sym| sym.include?(char) }
end

def tokenize(text)
  # tokenize :: String -> [ Token ]
  # Token
  #   = (:integer, Integer)
  #   | (:symbol, String)
  #   | (:identifier, String)
  #   | (:string, String)
  #   | (:reserved, String)

  chars = text.chars
  chars << "\n" # extra char to allow tokens to complete. Necessary esp for comments - infinite loop otherwise

  buffer = []
  tokens = []

  ix = 0
  char = chars[ix]
  while ix < chars.length
    # p ["tokenize", char, peek(tokens)]

    case char
    when /\s/
      # no-op
    when '#'
      ix += 1 # consume
      char = chars[ix]

      while char != "\n"
        ix += 1
        char = chars[ix]
      end
    when '"'
      ix += 1 # consume
      char = chars[ix]

      buffer.clear
      while char != '"'
        if char == "\\"
          # escape character
          ix += 1
          char = chars[ix]
          buffer << generate_escape_code(char)
        else
          buffer << char
        end

        ix += 1
        char = chars[ix]
      end
      tokens << [:string, buffer.join]
    when /\d/
      buffer.clear
      while char =~ /\d/
        buffer << char

        ix += 1
        char = chars[ix]
      end
      ix -= 1 # put non \d char back
      tokens << [:integer, buffer.join]
    when /[_a-zA-Z]/
      buffer.clear
      while char =~ /[_a-zA-Z0-9]/
        buffer << char

        ix += 1
        char = chars[ix]
      end
      ix -= 1 # put non-ident char back

      val = buffer.join
      if RESERVED.include?(val)
        tokens << [:reserved, val]
      else
        tokens << [:identifier, val]
      end
    when ->(c){is_symbol_character(c)}
      buffer.clear
      while is_symbol_character(char)
        buffer << char

        ix += 1
        char = chars[ix]
      end
      ix -= 1 # put non-sym char back

      while !buffer.empty?
        # p ['buffer', buffer]
        valid_prefixes = ALL_SYMBOLS.select { |sym| buffer.join.start_with?(sym) }.sort_by(&:length)
        if valid_prefixes.empty?
          raise "unrecognized symbols: #{buffer.join}"
        end

        to_push = valid_prefixes.last # longest valid prefix
        tokens << [:symbol, to_push]
        buffer.shift(to_push.length) # remove consumed chars from the buffer
      end
    else
      raise "unrecognized character: #{char}"
    end

    ix += 1
    char = chars[ix]
  end
  tokens
end

def parse(tokens)
  tokens = [[:symbol, '{']] + tokens + [[:symbol, '}']] # sigil for parsing a scope
  tokens = tokens.reverse # makes next_token! etc easier
  ast = parse_!(:scope, tokens)
  raise "trailing tokens starting at: #{peek(tokens)[1]}" unless tokens.empty?
  ast
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
  # p ['parse_!', state, peek(tokens)]
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
  when :let
    target = next_token!(tokens) { |(type, val)| raise "bad let target: #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing '=': #{tok[1]}" unless tok == [:symbol, '='] }
    rhs = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:let, target, rhs]
  when :expression
    _parse_expression!(tokens)
  when :call # expression
    name_ = next_token!(tokens) { |type, val| raise "bad call name: #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing '(': #{tok[1]}" unless tok == [:symbol, '('] }
    args = parse_!(:explist, tokens)
    next_token!(tokens) { |tok| raise "missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
    [:call, name_, args]
  when :do # statement
    expr = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:do, expr]
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
      if peek(tokens) == [:symbol, '{']
        else_ = parse_!(:scope, tokens)
      elsif peek(tokens) == [:reserved, 'if']
        next_token!(tokens) # consume 'if'
        else_ = parse_!(:if, tokens)
      else
        raise "bad 'else' branch: #{peek(tokens)[1]}"
      end
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
  when :break
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:break]
  when :raise
    val = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:raise, val]
  when :alloc
    target = next_token!(tokens){ |type, val| raise "bad alloc target: #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:alloc, target]
  when :statement
    type, val = tok = next_token!(tokens)
    case type
    when :reserved
      if %w(let def if while return break do raise alloc).include?(val)
        parse_!(val.to_sym, tokens)
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

def _parse_expression!(tokens)
  # p ['_parse_expression!', peek(tokens)]

  lhs = _parse_operand!(tokens)
  op = next_token!(tokens)
  prec = _precedence(op[1])
  if prec.nil?
    tokens << op # put it back
    return lhs
  end

  loop do
    rhs = _parse_operand!(tokens)
    op2 = next_token!(tokens)
    prec2 = _precedence(op2[1])
    if prec2.nil?
      tokens << op2 # put it back
      return [:binop, op, lhs, rhs]
    end
    # p ["comparing", op[1], op2[1], prec, prec2]
    if prec <= prec2
      lhs = [:binop, op, lhs, rhs]
      op = op2
      prec = prec2
    else
      tokens << op2
      tokens << rhs
      return [:binop, op, lhs, _parse_expression!(tokens)]
    end
  end
end

def _parse_operand!(tokens)
  # p ['_parse_operand!', peek(tokens)]

  type, val = tok = next_token!(tokens)
  case type
  when :string
    tok
  when :integer
    tok
  when :reserved
    raise "reserved word in expression: #{val}" unless val == 'true' or val == 'false'
    tok
  when :identifier
    if peek(tokens) == [:symbol, '(']
      tokens << tok # put fxn name back
      parse_!(:call, tokens)
    else
      tok
    end
  when :symbol
    if tok == [:symbol, '-']
      [:binop, tok, [:integer, "0"], _parse_operand!(tokens)]
    elsif tok == [:symbol, '(']
      lhs = parse_!(:expression, tokens)
      next_token!(tokens) { |tok| raise "missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
      lhs
    else
      raise "bad lhs: #{tok[1]}"
    end
  else
    raise "iae"
  end
end

def _precedence(op_val)
  ix = BINOPS.find_index(op_val)
  if ix
    return BINOP_PREC[ix]
  end
end

def generate_code(node)
  codegen(node)
end

def codegen_list(list, sep)
  "#{list.map{|elem| codegen(elem)}.join(sep)}"
end

def codegen(node)
  # p ["codegen", node]

  node_type = node.shift
  case node_type
  when :scope
    body, = node
    "#{body.map{|elem| codegen(elem)}.join("\n")}"
  when :let
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
    when '_leftpop'
      arr, = args
      "#{codegen(arr)}.splice(0,1)[0]"
    when '_get'
      arr, ix = args
      "#{codegen(arr)}[#{codegen(ix)}]"
    when '_readFile'
      fname, = args
      "require('fs').readFileSync(#{codegen(fname)}, 'utf-8')"
    when '_writeFile'
      fname, data = args
      "require('fs').writeFileSync(#{codegen(fname)}, #{codegen(data)}, 'utf-8')"
    when '_inspect'
      data, = args
      "require('util').inspect(#{codegen(data)}, false, null)"
    when '_print'
      val, = args
      "console.log(#{codegen(val)})"
    when '_ARGV'
      "process.argv.slice(2)"
    else
      "#{name_str}(#{codegen_list(args, ', ')})"
    end
  when :do
    call, = node
    "#{codegen(call)};"
  when :def
    name_, params, body = node
    codegen_def(name_, params, body)
  when :if
    cond, then_, else_ = node
    fst = "if (#{codegen(cond)}) {\n#{codegen(then_)}\n}"
    snd = if else_ then " else {\n#{codegen(else_)}\n}" else "" end
    fst + snd
  when :while
    cond, body = node
    "while (#{codegen(cond)}) {\n#{codegen(body)}\n}"
  when :return
    val, = node
    "return #{codegen(val)};"
  when :break
    "break;"
  when :raise
    val, = node
    "throw new Error(#{codegen(val)});"
  when :alloc
    target, = node
    "let #{codegen(target)};"
  when :integer
    val, = node
    "#{val}"
  when :string
    val, = node
    "\"#{string_escape(val)}\""
  when :reserved
    word, = node
    "#{word}"
  when :symbol
    val, = node
    if val == '=='
      val = '==='
    end
    "#{val}"
  when :identifier
    varname, = node
    "#{varname}"
  else
    raise "iae: unrecognized node type: #{node_type}"
  end
end

def codegen_def(name_, params, body)
  "function #{codegen(name_)}(#{codegen_list(params, ', ')}) {\n#{codegen(body)}\n}"
end

def generate_escape_code(char)
  case char
  when "n"
    "\n"
  when "t"
    "\t"
  else
    char
  end
end

def string_escape(str)
  str.chars.map do |char|
    case char
    when "\n"
      "\\n"
    when "\t"
      "\\t"
    when "\\"
      "\\\\"
    when "\""
      "\\\""
    else
      char
    end
  end.join
end

def _tok_to_string(tok)
  type, val = tok
  val_to_print = string_escape(val)

  "[ :#{type.to_s.upcase}, \"#{val_to_print}\" ],"
end

# for diff purposes
def _tokens_to_string(tokens)
  ( ["["] \
  + tokens.map{|tok| "#{_tok_to_string(tok)}"} \
  + ["]"] \
  ).join("\n")
end

infile, outfile = ARGV
text = File.read(infile)
# puts "\ntext loaded:\n#{text}"
tokens = tokenize(text)
File.write("ahuff/main_tokens.txt", _tokens_to_string(tokens)) # @debug
# puts "\ntokens:\n#{tokens}"
ast = parse(tokens)
File.write("ahuff/main_ast.txt", ast) # @debug
# puts "\nast:\n#{ast}"
code = generate_code(ast)
# puts "\ncode:\n#{code}"
File.write(outfile, code)
