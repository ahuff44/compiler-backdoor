#!/usr/bin/env ruby


def preprocess(text)
  text
  #   .gsub(/#.*$/, '')
  #   .gsub(/\s+/, ' ')
end

def tokenize(text)
  # tokenize :: String -> [ Token ]
  # Token
  #   = (:integer, Integer)
  #   | (:symbol, String)
  #   | (:identifier, String)
  #   | (:string, String)

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
        tokens << [state, buffer.join]
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
      when /[\+\-\*\=\;\(\)\,\{\}]/
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
  tokens = tokens.reverse # makes token.pop easier
  parse_!(:scope, tokens)
end

def next_token!(tokens)
  tok = tokens.pop
  if block_given?
    yield tok
  end
  tok
end

def parse_!(state, tokens)
  case state
  when :scope
    next_token!(tokens) { |tok| raise "scope missing '{': #{tok[1]}" unless tok == [:symbol, '{'] }
    ast = []
    loop do
      if tokens.last == [:symbol, "}"] # peek
        next_token!(tokens)
        return ast
      end
      ast << parse_!(:statement, tokens)
    end
  when :let
    target = next_token!(tokens) { |(type, val)| raise "bad let target #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "missing assignment operator: #{tok[1]}" unless tok == [:symbol, '='] }
    rhs = parse_!(:expression, tokens)
    next_token!(tokens) { |tok| raise "missing semicolon: #{tok[1]}" unless tok == [:symbol, ';'] }
    [:let, target, rhs]
  when :expression
    p ['exp', tokens.reverse]
    type1, val1 = tok1 = next_token!(tokens)
    if tok1 == [:symbol, '(']
      exp = parse_!(:expression, tokens)
      next_token!(tokens) { |tok| raise "missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }

      lhs = exp
    else
      lhs = tok1
    end
    p lhs
    type2, val2 = tok2 = next_token!(tokens)

    if type2 == :symbol
      case val2
      when '('
        params = parse_!(:list, tokens)
        next_token!(tokens) { |tok| raise "missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
        [:call, lhs, params]
      when '+'
        p 'plus'
        [:binop, tok2, lhs, parse_!(:expression, tokens)]
      when '-'
        [:binop, tok2, lhs, parse_!(:expression, tokens)]
      when '*'
        [:binop, tok2, lhs, parse_!(:expression, tokens)]
      else
        tokens << tok2 # put it back
        lhs
      end
    else
      raise "bad expression: #{lhs}"
    end



  when :list
    if tokens.last == [:symbol, ")"] # peek
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
    if tokens.last == [:symbol, ")"] # peek
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
  when :print
    params = parse_!(:list, tokens)
    next_token!(tokens) { |tok| raise "missing semicolon: #{tok[1]}" unless tok == [:symbol, ';'] }
    [:print, params]
  when :def
    name_ = next_token!(tokens) { |(type, val)| raise "bad function name: #{val}" unless type == :identifier }
    next_token!(tokens) { |tok| raise "def '#{name_[1]}' missing '(': #{tok[1]}" unless tok == [:symbol, '('] }
    args = parse_!(:arglist, tokens)
    next_token!(tokens) { |tok| raise "def '#{name_[1]}' missing ')': #{tok[1]}" unless tok == [:symbol, ')'] }
    body = parse_!(:scope, tokens)
    [:def, name_, args, body]
  when :if
    cond = parse_!(:expression, tokens)
    then_ =
    else_ =
    [:if, cond, then_, else_]
  when :statement
    type, val = tok = next_token!(tokens)
    case type
    when :string
      raise "unexpected statement starting at: #{val}"
    when :integer
      raise "unexpected statement starting at: #{val}"
    when :symbol
      raise "unexpected statement starting at: #{val}"
    when :identifier
      case val
      when "let"
        parse_!(:let, tokens)
      when "print"
        parse_!(:print, tokens)
      when "def"
        parse_!(:def, tokens)
      when "if"
        parse_!(:if, tokens)
      when "for"
        parse_!(:for, tokens)
      else
        tok
      end
    when nil
      raise "unexpected end of input"
    end
  else
    puts "unimplemented state: #{state}" # @TODO raise
    next_token!(tokens)
  end
end

def generate_code(ast)
  ast
end

infile, outfile = ARGV
text = File.read(infile)
# puts "\ntext loaded:\n#{text}"
text = preprocess(text)
# puts "\ntext preprocessed:\n#{text}"
tokens = tokenize(text)
# puts "\ntokens:\n#{tokens}"
ast = parse(tokens)
puts "\nast:\n#{ast}"
code = generate_code(ast)
# puts "\ncode:\n#{code}"
