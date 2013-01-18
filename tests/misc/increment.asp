<%
	class incr
		private x	

		public default property get item()
			item = x + 1
		end property

		public property let item(value)
			x = item
		end property
	end class

	set y = new incr

	while true
		y = y
		response.write y
	wend
%>
