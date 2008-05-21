# Cache as hash with bounded size. Will delete least recently used (LRU) entry
# if full when new key-value pair added.
# NOTE: Not thread safe...
class BoundedLruCache
  attr_accessor :max_size

  def initialize(max_size = 2**30-1, anObject = nil)
    @hash, @uses, @max_size = Hash.new(anObject), Array.new, max_size
  end

  def []=(key, val)
    delete_least_recently_used if @hash.length >= @max_size
    latest_used_key(key)
    @hash[key] = val
  end

  def [](key)
    res = @hash[key] 
    latest_used_key(key) if res
    res
  end

  # Delegate undefined methods to the hash
  def method_missing(methodId, *args)
    if @hash.respond_to?(methodId)
      @hash.send(methodId, *args)
    else
      super
    end
  end

  private

  def latest_used_key(key)
    @uses.delete(key)
    @uses.push(key)
  end

  def delete_least_recently_used
    lru = @uses.shift
    @hash.delete lru
  end
end
