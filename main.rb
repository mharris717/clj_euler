require 'mharris_ext'

class Solution
  class << self
    fattr(:all_coins) { [1 ,2 ,5 ,10, 20, 50, 100, 200] }
    def solve(c)
      new(:remaining => c).ultimate_solutions
    end
  end
  attr_accessor :remaining
  include FromHash
  fattr(:coins) { [] }
  fattr(:next_coins) do
    smallest_current = coins.min || 999
    self.class.all_coins.select { |x| x <= remaining && x <= smallest_current }
  end
  def next_solution(c)
    if c == 1
      ones = [1]*remaining
      Solution.new(:coins => coins + ones, :remaining => 0)
    else
      Solution.new(:coins => coins + [c], :remaining => remaining-c)
    end
  end
  fattr(:next_solutions) do
    next_coins.map { |c| next_solution(c) }
  end
  fattr(:ultimate_solutions) do
    if remaining < 0
      raise "bad remaining"
    elsif remaining == 0
      [self]
    elsif next_solutions.empty?
      raise "next empty"
    else
      next_solutions.map { |x| x.ultimate_solutions }.flatten
    end
  end
  def coin_groups
    ultimate_solutions.map { |x| x.coins }
  end
  def to_s
    res = []
    self.class.all_coins.each do |c|
      sz = coins.select { |x| x == c }.size
      res << "#{c}:#{sz}" if sz > 0
    end
    res.join(" ")
  end
end

ss = Solution.solve(200).map { |x| x.to_s }
#puts ss.inspect
puts ss.size

#s = Solution.new(:remaining => 2)
#s.next_solutions.each { |x| puts x.coins.inspect }
      
      
  