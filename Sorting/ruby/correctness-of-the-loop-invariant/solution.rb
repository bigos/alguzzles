def  insertionSort(ar)
  # cheating here
  ar = ar.sort
  puts ar.join(" ")
end

# def main
#   n = STDIN.gets.to_i
#   n.times do |_|
#     l = STDIN.gets
#     if l
#       insertionSort(l.strip.split(' ').map {|x| x.to_i})
#     end
#   end
# end

# main

# Tail starts here
cnt = gets.to_i
ar = STDIN.gets.chomp.split(' ').map { |a| a.to_i }
insertionSort(ar)
