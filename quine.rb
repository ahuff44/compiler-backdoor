def escape(str)
  str.chars.map do |chr|
    case chr
    when "\\"
      "\\\\"
    when "\""
      "\\\""
    when "\n"
      "\\n"
    else
      chr
    end
  end.join
end

foo = ->(x){ x+"\nputs foo.call(\""+escape(x)+"\")" }
puts foo.call("def escape(str)\n  str.chars.map do |chr|\n    case chr\n    when \"\\\\\"\n      \"\\\\\\\\\"\n    when \"\\\"\"\n      \"\\\\\\\"\"\n    when \"\\n\"\n      \"\\\\n\"\n    else\n      chr\n    end\n  end.join\nend\n\nfoo = ->(x){ x+\"\\nputs foo.call(\\\"\"+escape(x)+\"\\\")\" }")

# basic idea:
# foo = ->(x){ x+"\nputs foo.call("+x.inspect+")" }
# puts foo.call("foo = ->(x){ x+\"\\nputs foo.call(\"+x.inspect+\")\" }")
