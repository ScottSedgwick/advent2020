task :build do
  sh('cabal build')
end

task :run do
  sh('cabal exec advent-exe < data/Day11Test.txt')
end

task :test do
  puts `cabal test`
end

task :default => [:build, :run]