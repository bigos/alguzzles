# for some reason this works on the shell terminal but doesn't on hackerrank
def  insertionSort(ar)
  # puts 'before '+ar.to_s
  i = 1
  while (i < ar.length)
    if (i > 0 && ar[i] < ar[i - 1])
      value = ar[i]
      j = i-1
      while (j > 0 && value < ar[j])
        ar[j+1] = ar[j]
        j -= 1
        # puts i
        # puts j
        # puts ar.inspect
      end
      ar[j+1] = value
    end
    i += 1
  end
  STDOUT.puts ar.join(" ")
end

def main
  n = STDIN.gets.to_i
  n.times do |_|
    l = STDIN.gets
    if l
      insertionSort(l.strip.split(' ').map {|x| x.to_i})
    end
  end
end

main
