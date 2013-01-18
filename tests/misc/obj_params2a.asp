<%
	response.buffer = false

	class test
		public y
	end class

	dim	z

	function shanooga
		if isEmpty(z) then
			z = new test
			z.y = 45
		end if
		set shanooga = z
	end function

	function dilly(x)
		response.write typename(x)
		response.write x.y
	end function

	dilly shanooga()
%>
