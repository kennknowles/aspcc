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

	response.write shanooga.y
%>
