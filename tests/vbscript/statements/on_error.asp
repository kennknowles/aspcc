<%
	dim a(4)
	a(1) = 3
	on error resume next
	a(10) = 5
	response.write "made it past the error"

	on error goto 0

	a(10) = 6
%>
