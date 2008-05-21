unless TimesClass
  TimesClass = ((RUBY_VERSION < "1.7") ? Time : Process) 
end

module Profiler
  @@start = TimesClass.times.utime

  # Method invocation stack with one entry for each invocation:
  #   Time at entry, Total times in subfunction also being logged, MethodId
  @@invocation_stack = [[0, 0, "#toplevel".intern]]

  # One entry for each method: NumCalls, TotalTime, OnlyMyTime, Callers
  @@map = {"#toplevel".intern => [1, 0, 0]}

  # One entry for each method: hash mapping args.inspect to count
  @@arguments = Hash.new

  @@time_limit = 5 * 60

  def start(timeLimitInMinutes = nil)
    if timeLimitInMinutes
      @@time_limit = timeLimitInMinutes * 60
    else
      @@time_limit = nil
    end
    @@start = Float(TimesClass.times.utime)
  end
  module_function :start

  def __enter__(method, *args)
    now = TimesClass.times.utime
    if @@time_limit
      if now - @@start > @@time_limit
	STDERR.puts "Profiling time limit violated. Run terminated."
	STDERR.puts profile_summary(true, true)
	exit(-1)
      end
    end
    @@invocation_stack.push [now, 0.0, method]
    begin
      @@arguments[method][args.inspect] += 1
    rescue Exception
      @@arguments[method] = Hash.new(0)
      retry
    end
  end
  module_function :__enter__
    
  def __leave__(method, *retargs)
    now = TimesClass.times.utime
    tick, data = @@invocation_stack.pop, @@map[method]
    unless data
      data = [0.0, 0.0, 0.0, Hash.new(0)] 
      @@map[method] = data
    end
    data[0] += 1
    total_time_this_invocation = now - tick[0]
    data[1] += total_time_this_invocation
    data[2] += total_time_this_invocation - tick[1]
    data[3][caller[1]] += 1
    @@invocation_stack[-1][1] += total_time_this_invocation
    return *retargs
  end
  module_function :__leave__

  # Go through the invocation stack and leave all methods.
  def unwind_invocation_stack
    while @@invocation_stack.length > 1
      __leave__(@@invocation_stack.pop[2])
    end
  end
  module_function :unwind_invocation_stack

  def Profiler.profile_summary(writeCallers = false, writeArguments = false)
    total_elapsed = TimesClass.times.utime - @@start
    str =  "Profiling summary\n"
    str += "*****************\n"
    str += "Total elapsed time: #{total_elapsed} seconds\n"
    unwind_invocation_stack if @@invocation_stack.length > 1
    total = @@invocation_stack.last[1]
    time_in_nonprofiled = total_elapsed - total
    str += "Time spent in non-profiled methods: #{time_in_nonprofiled} sec\n"
    str += "Time in profiled methods:\n"
    if total == 0 then total = 0.01 end
    @@map["#toplevel".intern][1] = total
    data = @@map.to_a.sort{|a,b| b[1][2] <=> a[1][2]}
    sum = 0
    str += "  %%   cumulative   self              self     total\n"
    str += " time   seconds   seconds    calls  ms/call  ms/call  name\n"
    str += " ---------------------------------------------------------\n"
    for d in data
      method = d[0]
      next if method == "#toplevel".intern 
      d = d[1]
      sum += d[2]
      str += "%6.2f %8.2f  %8.2f %8d " % [d[2]/total*100, sum, d[2], d[0]]
      str += "%8.2f %8.2f  %s\n" % [d[2]*1000/d[0], d[1]*1000/d[0], 
	method.id2name]
      if writeCallers
	str += "  Call sites:\n"
	d[3].to_a.sort {|a,b| b[1] <=> a[1]}.each do |callersite, count|
	  str += "   #{count}: " + callersite.split("/").last + "\n"
	end
      end
      if writeArguments and d[0] > 1
	str += "  Arguments:\n"
	counts, num_prev_seen = Hash.new(0), 0
	@@arguments[method].to_a.sort {|a,b| b[1] <=> a[1]}.each do |args, cnt|
	  # str += "   #{cnt}: " + args + "\n" if cnt > 1
	  counts[cnt] += cnt
	  num_prev_seen += cnt if cnt > 1
	end
	proportion_prev_seen = num_prev_seen*100.0/d[0]
	proportion_unique = 100.0 - proportion_prev_seen
	str += "   %3.2f%% (#{d[0].to_i - num_prev_seen}) of calls with unique args" % proportion_unique
	if proportion_unique != 100.0
	  str += ", and\n"
	  str += "   %3.2f%% (#{num_prev_seen}) of calls with args that were used several times\n" % proportion_prev_seen
	  str += "    distr: #{counts.inspect}"
	end
	str += "\n"
      end
      str += "\n" if writeCallers or writeArguments
    end
    return str
  end
end

#############################################################################
# Simple test
#############################################################################
if __FILE__ == $0
  class ComplexTest
    attr_reader :real, :imaginary
    def initialize(real, imaginary)
      Profiler.__enter__(:initialize, real, imaginary) 
      @real, @imaginary = real, imaginary
      Profiler.__leave__(:initialize, self)
    end
    def add(other)
      Profiler.__enter__(:add, other)
      real_add(other)
      Profiler.__leave__(:add, self)
    end
    def real_add(other)
      Profiler.__enter__(:real_add, other)
      @real += other.real
      @imaginary += other.imaginary
      Profiler.__leave__(:real_add, self)
    end
    def inspect
      "#{real} + i*#{imaginary}"
    end
  end

  Profiler.start

  10.times do
    c = ComplexTest.new(rand, rand)
    100.times do
      c.add(ComplexTest.new(rand, rand))
    end
    puts "It finished"
  end
  c = ComplexTest.new(1,1)
  c = ComplexTest.new(1,1)
  puts Profiler.profile_summary(true, true)
end
