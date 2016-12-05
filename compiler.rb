#!/usr/bin/env ruby


RESERVED = %w(true false let def if while return break do raise alloc)
STATEMENT_STARTERS = %w(let def if while return break do raise alloc)
SYNTAX = %w(= ; ( ) , { } <<)
BINOPS = %w(    . *  +  -  == != <  >  <= >= && ||)
BINOP_PREC = %w(0 10 20 20 30 30 30 30 30 30 40 50).map(&:to_i)
ALL_SYMBOLS = SYNTAX + BINOPS

def is_symbol_character(char)
  ALL_SYMBOLS.any? { |sym| sym.include?(char) }
end

def tokenize_error(msg, pos)
  line, char = pos
  puts "Tokenization error at #{line}:#{char}:\n\t#{msg}"
  exit 1
end

def tokenize(text, logging)
  # tokenize :: String -> [ Token ]
  # Token
  #   = (:integer, Integer)
  #   | (:symbol, String)
  #   | (:identifier, String)
  #   | (:string, String)
  #   | (:reserved, String)

  $char_ix = 0 # 1-indexed
  $line_ix = 1 # 1-indexed
  $char_in_line = 0 # 1-indexed
  $chars = (text+"\n").chars # extra char to allow tokens to complete. Necessary esp for comments - infinite loop otherwise
  $char = ""
  def next_char!
    $char_ix += 1
    $char_in_line += 1
    $char = $chars[$char_ix-1]
    if $char == "\n"
      $line_ix += 1
      $char_in_line = 0 # 1-indexed
    end
  end
  def prev_char!
    $char_in_line -= 1
    if $char == "\n"
      $line_ix -= 1
      $char_in_line = -42 # @hack hard to know what it actually should be
    end
    $char_ix -= 1
    if $char_ix < 0
      raise "iae: sub-0 char index"
    end
    $char = $chars[$char_ix-1]
  end

  def tokenize_heredoc(pos)
    target = []
    while $char =~ /[_a-zA-Z0-9]/
      target << $char

      next_char!
    end
    tokenize_error("bad heredoc header; expected \\n, got \"#{$char}\"", pos) unless $char == "\n"
    next_char! # skip \n

    target = ["\n"] + target
    target << "\n"
    buffer = []
    while buffer.last(target.length) != target
      if $char
        buffer << $char
      else
        #@hack
        tokenize_error("unexpected EOF", [-1, -1])
      end

      next_char!
    end
    prev_char! # put next char back

    str = buffer.slice(0, buffer.length-target.length).join
    return [:string, str, pos]
  end

  buffer = []
  tokens = []

  next_char!
  while $char_ix < $chars.length
    if logging
      p ["tokenize", $char_ix, $line_ix, $char_in_line, $char, peek(tokens)]
    end

    case $char
    when /\s/
      # no-op
    when '#'
      next_char! # consume

      while $char != "\n"
        next_char!
      end
    when '"'
      pos = [$line_ix, $char_in_line]
      next_char! # consume

      buffer.clear
      while $char != '"'
        if $char == "\\"
          # escape character
          next_char!
          buffer << generate_escape_code($char)
        else
          buffer << $char
        end

        next_char!
      end
      tokens << [:string, buffer.join, pos]
    when /\d/
      pos = [$line_ix, $char_in_line]

      buffer.clear
      while $char =~ /\d/
        buffer << $char

        next_char!
      end
      prev_char! # put non \d char back
      tokens << [:integer, buffer.join, pos]
    when /[_a-zA-Z]/
      pos = [$line_ix, $char_in_line]

      buffer.clear
      while $char =~ /[_a-zA-Z0-9]/
        buffer << $char

        next_char!
      end
      prev_char! # put non-ident char back

      val = buffer.join
      if RESERVED.include?(val)
        tokens << [:reserved, val, pos]
      else
        tokens << [:identifier, val, pos]
      end
    when ->(c){is_symbol_character(c)}
      pos = [$line_ix, $char_in_line]

      buffer.clear
      while is_symbol_character($char)
        buffer << $char

        if buffer == '<<'.chars # @hack the placement of the code is... awkward. the exit mechanism back into normal execution state is p weird too
          next_char!
          tokens << tokenize_heredoc(pos)
          buffer.clear
        end

        next_char!
      end
      prev_char! # put non-sym char back

      while !buffer.empty?
        # p ['buffer', buffer]
        valid_prefixes = ALL_SYMBOLS.select { |sym| buffer.join.start_with?(sym) }.sort_by(&:length)
        if valid_prefixes.empty?
          tokenize_error("cannot parse \"#{buffer.join}\" into symbols", pos)
        end

        to_push = valid_prefixes.last # longest valid prefix
        tokens << [:symbol, to_push, pos] # @todo: fix pos; won't always be correct
        buffer.shift(to_push.length) # remove consumed chars from the buffer
      end
    else
      tokenize_error("unrecognized character: #{$char}", pos)
    end

    next_char!
  end
  if logging
    p ["tokens", tokens]
  end
  tokens
end

def parse_entrypoint(tokens)
  tokens = tokens.reverse # makes next_token! etc easier
  ast = []
  while !peek(tokens).nil?
    ast << parse_!(:statement, tokens)
  end
  parse_error("expected EOF", peek(tokens)) unless tokens.empty?
  ast
end

def next_token!(tokens)
  tok = tokens.pop
  if tok.nil?
    parse_error("unexpected EOF", [:eof, "EOF", [-1, -1]])
  end
  if block_given?
    yield tok
  end
  tok
end

def peek(tokens)
  tokens.last
end

def tok_match(tok, target)
  return (!tok.nil? and tok[0] == target[0] and tok[1] == target[1])
end

def parse_error(msg, tok)
  type, val, (line, char) = tok
  puts "Parse error at #{line}:#{char} (\"#{val}\"):\n\t#{msg}"
  exit 1
end

def parse_!(state, tokens)
  # p ['parse_!', state, peek(tokens)]

  case state
  when :scope
    next_token!(tokens) { |tok| parse_error("scope missing '{'", tok) unless tok_match(tok, [:symbol, '{']) }
    ast = []
    loop do
      if tok_match(peek(tokens), [:symbol, "}"])
        next_token!(tokens)
        return [:scope, ast]
      end
      ast << parse_!(:statement, tokens)
    end
  when :let
    target = next_token!(tokens) { |tok| parse_error("bad let target", tok) unless tok[0] == :identifier }
    next_token!(tokens) { |tok| parse_error("missing '='", tok) unless tok_match(tok, [:symbol, '=']) }
    rhs = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| parse_error("missing ';'", tok) unless tok_match(tok, [:symbol, ';']) }
    [:let, target, rhs]
  when :expression
    _parse_expression!(tokens)
  when :call # expression
    name_ = next_token!(tokens) { |tok| parse_error("bad call name: #{val}", tok) unless tok[0] == :identifier }
    next_token!(tokens) { |tok| parse_error("missing '('", tok) unless tok_match(tok, [:symbol, '(']) }
    args = parse_!(:explist, tokens)
    next_token!(tokens) { |tok| parse_error("missing ')'", tok) unless tok_match(tok, [:symbol, ')']) }
    [:call, name_, args]
  when :do # statement
    expr = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| parse_error("missing ';'", tok) unless tok_match(tok, [:symbol, ';']) }
    [:do, expr]
  when :explist
    if tok_match(peek(tokens), [:symbol, ")"])
      []
    else
      buffer = []
      loop do
        buffer << parse_!(:expression, tokens)
        popped = next_token!(tokens)
        if !tok_match(popped, [:symbol, ","])
          tokens << popped # return the token
          return buffer
        end
      end
    end
  when :arglist
    if tok_match(peek(tokens), [:symbol, ")"])
      []
    else
      buffer = []
      loop do
        buffer << next_token!(tokens) { |tok| parse_error("bad arg", tok) unless tok[0] == :identifier }
        popped = next_token!(tokens)
        if !tok_match(popped, [:symbol, ","])
          tokens << popped # return the token
          return buffer
        end
      end
    end
  when :def
    name_ = next_token!(tokens) { |tok| parse_error("bad function name", tok) unless tok[0] == :identifier }
    next_token!(tokens) { |tok| parse_error("def '#{name_[1]}' missing '('", tok) unless tok_match(tok, [:symbol, '(']) }
    params = parse_!(:arglist, tokens)
    next_token!(tokens) { |tok| parse_error("def '#{name_[1]}' missing ')'", tok) unless tok_match(tok, [:symbol, ')']) }
    body = parse_!(:scope, tokens)
    [:def, name_, params, body]
  when :if
    next_token!(tokens) { |tok| parse_error("'if' missing '('", tok) unless tok_match(tok, [:symbol, '(']) }
    cond = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| parse_error("'if' missing ')'", tok) unless tok_match(tok, [:symbol, ')']) }
    then_ = parse_!(:scope, tokens)
    if tok_match(peek(tokens), [:identifier, 'else'])
      next_token!(tokens) # consume 'else'
      if tok_match(peek(tokens), [:symbol, '{'])
        else_ = parse_!(:scope, tokens)
      elsif tok_match(peek(tokens), [:reserved, 'if'])
        next_token!(tokens) # consume 'if'
        else_ = parse_!(:if, tokens)
      else
        parse_error("bad 'else' branch", peek(tokens))
      end
    end
    [:if, cond, then_, else_]
  when :while
    cond = parse_!(:expression, tokens)
    body = parse_!(:scope, tokens)
    [:while, cond, body]
  when :return
    val = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| parse_error("missing ';'", tok) unless tok_match(tok, [:symbol, ';']) }
    [:return, val]
  when :break
    next_token!(tokens) { |tok| parse_error("missing ';'", tok) unless tok_match(tok, [:symbol, ';']) }
    [:break]
  when :raise
    val = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| parse_error("missing ';'", tok) unless tok_match(tok, [:symbol, ';']) }
    [:raise, val]
  when :alloc
    target = next_token!(tokens){ |tok| parse_error("bad alloc target", tok) unless tok[0] == :identifier }
    next_token!(tokens) { |tok| parse_error("missing ';'", tok) unless tok_match(tok, [:symbol, ';']) }
    [:alloc, target]
  when :statement
    type, val, pos = tok = next_token!(tokens)
    case type
    when :reserved
      if STATEMENT_STARTERS.include?(val)
        parse_!(val.to_sym, tokens)
      else
        raise "iae: unknown reserved word: #{tok}"
      end
    else
      parse_error("unexpected statement", tok)
    end
  else
    raise "iae: unimplemented state: #{state}"
  end
end

def _parse_expression!(tokens)
  # p ['_parse_expression!', peek(tokens)]

  lhs = _parse_operand!(tokens)
  op = next_token!(tokens)
  prec = _precedence(op[1])
  if prec == -1
    tokens << op # put it back
    return lhs
  end

  loop do
    rhs = _parse_operand!(tokens)
    op2 = next_token!(tokens)
    prec2 = _precedence(op2[1])
    if prec2 == -1
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

  type, val, pos = tok = next_token!(tokens)
  case type
  when :string
    tok
  when :integer
    tok
  when :reserved
    parse_error("reserved word in expression", tok) unless val == 'true' or val == 'false'
    tok
  when :identifier
    if tok_match(peek(tokens), [:symbol, '('])
      tokens << tok # put fxn name back
      parse_!(:call, tokens)
    else
      tok
    end
  when :symbol
    if tok_match(tok, [:symbol, '-'])
      # @hack
      tokens << tok # put '-' back
      [:integer, "0", pos]
    elsif tok_match(tok, [:symbol, '('])
      lhs = parse_!(:expression, tokens)
      next_token!(tokens) { |tok| parse_error("missing ')'", tok) unless tok_match(tok, [:symbol, ')']) }
      lhs
    else
      parse_error("bad lhs", tok)
    end
  when :call
    # @hack this happens when a :call "token" is pushed back onto the tokens stack b/c of operator precedence
    tok
  else
    raise "iae: _parse_operand! type=#{type}"
  end
end

def _precedence(op_val)
  ix = BINOPS.find_index(op_val)
  if ix
    BINOP_PREC[ix]
  else
    -1 # @hack ? to be parallel with the other compiler
  end
end

def generate_code(ast)
  "#{ast.map{|stmt| codegen(stmt)}.join("\n")}"
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
    when '_leftpush'
      arr, val = args
      "#{codegen(arr)}.unshift(#{codegen(val)})"
    when '_get'
      arr, ix = args
      "#{codegen(arr)}[#{codegen(ix)}]"
    when '_set'
      arr, ix, data = args
      "#{codegen(arr)}[#{codegen(ix)}] = #{codegen(data)}"
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
    when '_EXIT'
      val, = args;
      "process.exit(#{codegen(val)})"
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
    val, pos = node
    "#{val}"
  when :string
    val, pos = node
    "\"#{string_escape(val)}\""
  when :reserved
    word, pos = node
    "#{word}"
  when :symbol
    val, pos = node
    if val == '=='
      val = '==='
    end
    "#{val}"
  when :identifier
    varname, pos = node
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
  type, val, pos = tok
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
tokens = tokenize(text, false)
File.write("ahuff/main_tokens.txt", _tokens_to_string(tokens)) # @debug
# puts "\ntokens:\n#{tokens}"
ast = parse_entrypoint(tokens)
File.write("ahuff/main_ast.txt", ast) # @debug
# puts "\nast:\n#{ast}"
code = generate_code(ast)
# puts "\ncode:\n#{code}"
File.write(outfile, code)
