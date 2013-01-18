<%
	response.write cDbl(cDate("1/1/1970")) & "<br />"
	response.write cDbl(cDate("12/30/1899"))
%>

------------<p />
------------<p />

<%
	response.write now
	response.write "<br />" & vbNewLine

	response.write now() - 2
	response.write "<br />" & vbNewLine

	response.write cDbl(now() - 2)
	response.write "<br />" & vbNewLine

	response.write now() * 3
	response.write "<br />" & vbNewLine

	response.write now() + 5
	response.write "<br />" & vbNewLine


	response.write date()
	response.write "<br />" & vbNewLine
	response.write date() - 2
	response.write "<br />" & vbNewLine
%>
