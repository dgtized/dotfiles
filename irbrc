#!/usr/bin/ruby
# put this as ~/.irbrc and you shall have commandline completion

HISTFILE = "~/.irb.hist" unless(defined? :HISTFILE)
MAXHISTSIZE = 100 unless (defined? :MAXHISTSIZE)

begin
  if defined? Readline::HISTORY
    histfile = File::expand_path( HISTFILE )
    if File::exist?( histfile )
      lines = IO::readlines( histfile ).collect {|line| line.chomp}
      puts "Read %d saved history commands from %s." %
        [ lines.nitems, histfile ] if $DEBUG || $VERBOSE
      Readline::HISTORY.push( *lines )
    else
      puts "History file '%s' was empty or non-existant." %
        histfile if $DEBUG || $VERBOSE
    end
    
    Kernel::at_exit {
      lines = Readline::HISTORY.to_a.reverse.uniq.reverse
      lines = lines[ -MAXHISTSIZE, MAXHISTSIZE ] if lines.nitems > MAXHISTSIZE
      $stderr.puts "Saving %d history lines to %s." %
        [ lines.length, histfile ] if $VERBOSE || $DEBUG
      File::open( histfile, File::WRONLY|File::CREAT|File::TRUNC ) {|ofh|
        lines.each {|line| ofh.puts line }
      }
    }
  end
end

class Object
  # Clone fails on numbers, but they're immutable anyway
  def megaClone
    begin self.clone; rescue; self; end
  end
end

class MethodFinder

  # Find all methods on [anObject] which, when called with [args] return [expectedResult]
  def self.find( anObject, expectedResult, *args )
    anObject.methods.select { |name| anObject.method(name).arity == args.size }.
                     select { |name| begin anObject.megaClone.method( name ).call(*args) == expectedResult; 
                                     rescue; end }	
  end

  # Pretty-prints the results of the previous method
  def self.show( anObject, expectedResult, *args )
    find( anObject, expectedResult, *args ).each { |name|
      print "#{anObject.inspect}.#{name}" 
      print "(" + args.map { |o| o.inspect }.join(", ") + ")" unless args.empty?
      puts " == #{expectedResult.inspect}" 
    }
  end
end

require 'irb/completion'
ARGV.concat [ "--readline", "--prompt-mode", "simple" ]

module Kernel
  def m(o)
    ObjectMethods.new(o.public_methods(false).sort)
  end
  
  class ObjectMethods < Array
    def inspect
      puts self
    end
  end
end


class Object
  def my_methods
    methods - Object.new.methods
  end
end

require 'yaml'

