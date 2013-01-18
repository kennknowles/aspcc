<%
	dim a(3)
	a(1) = "x"
	a(3) = "y"
	for i = 1 to 3
		response.write a(i)
	next
	erase a
	for i = 1 to 3
		response.write a(i)
	next
%>
