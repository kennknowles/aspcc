<%
	dim var
	for each var in Request.ServerVariables
		response.write var & " = " & Request.ServerVariables(var) & "<br />" & vbNewLine
	next
%>
