<%
	class contents
		public y(10)

		public default property get item(index)
			item = y(index)
		end property

		public property let item(index, value)
			y(index) = value
		end property
	end class

	dim c
	set c = new contents

	function x
		set x = c
	end function

	c(3) = 5
	response.write c(3)
	
	x()(5) = 4  ' DOES work
	response.write x()(5) ' DOES work
	
	x()(5) = 8  ' DOES work
	response.write x(5) ' but this doesn't

	x(5) = 7  ' doesn't work!!
	response.write x(5)
%>
