#!/usr/bin/env ruby


RESERVED = %w(true false let def if while return break call do raise)
SYMBOLS = %w(+ - * ! = < > ; ( ) , { } | &)

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
      if char == "\\"
        state = :escape_char
      elsif char != "\""
        buffer << char
      else
        tokens << [state, buffer.join]
        state = :none
      end
    when :escape_char
      buffer << case char
        when "n"
          "\n"
        when "t"
          "\t"
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
        tokens << [state, buffer.join] # @todo convert to int here? nah
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
        buffer.clear
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
        buffer.clear # not currently necessary; will be later for multi-character symbols
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
    target = next_token!(tokens) { |(type, val)| raise "bad let target #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing '=': #{tok[1]}" unless tok == [:symbol, '='] }
    rhs = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:let, target, rhs]
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
        next_token!(tokens) { |tok| raise "expected '='; got #{tok[1]}" unless tok == [:symbol, '='] }
        [:binop, [:symbol, '=='], lhs, parse_!(:expression, tokens)]
      when '!'
        next_token!(tokens) { |tok| raise "expected '='; got #{tok[1]}" unless tok == [:symbol, '='] }
        [:binop, [:symbol, '!='], lhs, parse_!(:expression, tokens)]
      when '&'
        next_token!(tokens) { |tok| raise "expected '&'; got #{tok[1]}" unless tok == [:symbol, '&'] }
        [:binop, [:symbol, '&&'], lhs, parse_!(:expression, tokens)]
      when '|'
        next_token!(tokens) { |tok| raise "expected '|'; got #{tok[1]}" unless tok == [:symbol, '|'] }
        [:binop, [:symbol, '||'], lhs, parse_!(:expression, tokens)]
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
  when :do
    call = parse_!(:call, tokens)
    next_token!(tokens) { |tok| raise "missing ';': #{tok[1]}" unless tok == [:symbol, ';'] }
    [:do, call]
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
  when :statement
    type, val = tok = next_token!(tokens)
    case type
    when :reserved
      case val
      when "let"
        parse_!(:let, tokens)
      when "def"
        parse_!(:def, tokens)
      when "if"
        parse_!(:if, tokens)
      when "while"
        parse_!(:while, tokens)
      when "return"
        parse_!(:return, tokens)
      when "break"
        parse_!(:break, tokens)
      when "do"
        parse_!(:do, tokens)
      when "raise"
        parse_!(:raise, tokens)
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
    when '_include'
      arr, elem = args
      "#{codegen(arr)}.includes(#{codegen(elem)})"
    when '_push'
      arr, elem = args
      "#{codegen(arr)}.push(#{codegen(elem)})"
    when '_pop'
      arr, elem = args
      "#{codegen(arr)}.pop(#{codegen(elem)})"
    when '_shift'
      arr, elem = args
      "#{codegen(arr)}.shift(#{codegen(elem)})"
    when '_map'
      arr, fxn = args
      "#{codegen(arr)}.map(#{codegen(fxn)})"
    when '_join'
      arr, sep = args
      if sep.nil?
        raise "_join takes 2 args; got #{args.length}: #{args}";
      end
      "#{codegen(arr)}.join(#{codegen(sep)})"
    when '_length'
      arr, = args
      "#{codegen(arr)}.length"
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
    "function #{codegen(name_)}(#{codegen_list(params, ', ')}) {\n#{codegen(body)}\n}"
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

# for diff purposes
def _tokens_to_string(tokens)
  def helper(tok)
    type, val = tok
    val_to_print = string_escape(val).gsub(/\\\"/, "\"")
    "[ '#{type.to_s.upcase}', '#{val_to_print}' ]"
  end

  a, *rest, b = tokens
  ( ["[ #{helper(a)},"] \
  + rest.map{|tok| "  #{helper(tok)},"} \
  + ["  #{helper(b)} ]"] \
  ).join("\n")
end

infile, outfile = ARGV
text = File.read(infile)
# puts "\ntext loaded:\n#{text}"
tokens = tokenize(text)
File.write("ahuff/main_tokens.txt", _tokens_to_string(tokens))
# puts "\ntokens:\n#{tokens}"
ast = parse(tokens)
# puts "\nast:\n#{ast}"
code = generate_code(ast)
# puts "\ncode:\n#{code}"
File.write(outfile, code)
