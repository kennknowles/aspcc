<%
	response.buffer = false

	class test
		public y
	end class

	dim	z
	set z = new test
	z.y = 45

	function dilly(x)
		response.write typename(x)
		response.write x.y
	end function

	dilly z
%>
