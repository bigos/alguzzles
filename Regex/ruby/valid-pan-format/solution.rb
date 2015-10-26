# running in the shell: $ ruby ./solution.rb < ./input0.txt

def main
  n = STDIN.gets.to_i
  ws = []
  n.times do |_|
    l = STDIN.gets.strip
    # p l
    if  l =~ /[A-Z]{5}\d{4}[A-Z]/
      puts "YES"
    else
      puts 'NO'
    end
  end
end

main
