module Rush
##################################################################
# default init functions
# You can copy this file to ~/.rushrc for a per user setting
##################################################################

require 'yaml'


# Sets up colors
def self.rush_init_curses
    Curses.init_pair(1,Curses::COLOR_WHITE,Curses::COLOR_BLUE) # Main screen colors
    Curses.init_pair(2,Curses::COLOR_BLACK,Curses::COLOR_WHITE)# Box colors
    Curses.init_pair(3,Curses::COLOR_WHITE,Curses::COLOR_RED)  # Selected item colors
end

# Right before curses is started
def self.rush_startup
  rush_init_curses

  $env = {}
  $env[:prompt] = "Dir.getwd.to_s+\"> \""
  Alias.set("vi","^vi")
  Alias.set("cat","!cat")
end

# Right after curses is shutdown
def self.rush_shutdown
end

end # Module
