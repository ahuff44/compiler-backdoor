foo = ->(pre, post){ pre+"foo.call("+pre.inspect+", "+post.inspect+")"+post }

x = 2
if x == 1
  puts "x is 1"
elsif x == 2
  payload = foo.call("foo = ->(pre, post){ pre+\"foo.call(\"+pre.inspect+\", \"+post.inspect+\")\"+post }\n\nx = 2\nif x == 1\n  puts \"x is 1\"\nelsif x == 2\n  payload = ", "\n  puts payload\nend")
  puts payload
end