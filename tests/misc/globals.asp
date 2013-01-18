<%
	dim x
	x = 45

	function barf
		x = 10
	end function

	response.write x & vbNewLine
	barf
	response.write x & vbNewLine
%>
