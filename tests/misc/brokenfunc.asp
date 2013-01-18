<%
	function x()
		dim poo(3)
		poo(1) = "hello"
		x = poo
	end function
	response.write x(1)
%>
