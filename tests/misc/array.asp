<%
	dim x(4)
	
	x(1) = 5
	x(3) = 8

	response.write x(1) & vbNewLine
	response.write x(3) & vbNewLine


	dim y()
	redim y(5)

	y(4) = 6
	response.write y(4) & vbNewLine
%>
