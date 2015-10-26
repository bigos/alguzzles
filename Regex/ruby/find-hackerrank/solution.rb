# running in the shell: $ ruby ./solution.rb < ./input0.txt

def main
  n = STDIN.gets.to_i
  ws = []
  n.times do |_|
    l = STDIN.gets.strip
    # p l
    if  l =~ /^hackerrank/ && l =~ /hackerrank$/
      puts 0
    elsif l =~ /^hackerrank/
      puts 1
    elsif  l =~ /hackerrank$/
      puts 2
    else
      puts -1
    end
  end
end

main
