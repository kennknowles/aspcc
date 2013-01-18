<%
	for i = 1 to 10
		response.write i & " "
	next

	response.write vbNewLine

	for i = 1 to 20 step 2
		response.write i & " "
	next

	response.write vbNewLine

	for i = 20 to 1 step -1
		response.write i & " "
	next

	response.write vbNewLine
%>
