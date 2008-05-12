
require 'yaml'
 
module Rush

# Loading Extra Aliases from yaml config file.
my_yaml_file = ENV['HOME']+"/rush.yaml"

if File.exists? my_yaml_file
  my_yaml_handle = YAML.load( File.open( my_yaml_file ) )
  my_yaml_handle.each_pair {| key, value | Alias.set(key,value) } 
end

end
