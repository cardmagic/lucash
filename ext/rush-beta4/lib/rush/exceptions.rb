module Rush

class NoInformationException < StandardError
	attr :ok_to_retry

	def message
		@message
	end

	def initialize(ok_to_retry, mes)
		@ok_to_retry = ok_to_retry
		@message = mes
	end
end

class PipelineStopedException < StandardError

	def message
		@message
	end

	def initialize(mes)
		@message = mes
	end
end

# A commands throwing this error will cause
# IRB to startup
class DebugException < StandardError
	attr :ok_to_retry

	def message
		@message
	end

	def initialize(ok_to_retry, mes)
		@ok_to_retry = ok_to_retry
		@message = mes
	end
end

end
