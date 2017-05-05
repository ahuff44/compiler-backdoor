#!/usr/bin/env ruby

foo = ->(pre, post){ pre+"foo.call(\n    "+pre.inspect+",\n    "+post.inspect+"\n  )"+post }

x = 2
if x == 1
  puts "x is 1"
elsif x == 2
  payload = foo.call(
    "#!/usr/bin/env ruby\n\nfoo = ->(pre, post){ pre+\"foo.call(\\n    \"+pre.inspect+\",\\n    \"+post.inspect+\"\\n  )\"+post }\n\nx = 2\nif x == 1\n  puts \"x is 1\"\nelsif x == 2\n  payload = ",
    "\n  puts payload\nend"
  )
  puts payload
end
