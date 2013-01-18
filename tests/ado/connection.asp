<%
	'set conn = Server.CreateObject("ADODB.Connection")
	set conn = new ADODB.Connection
	conn.open "Provider=SQLOLEDB;Server=DBDEV;Database=Services2001;uid=sa;pwd=1.Carol"

	set rs = conn.execute("select top 10 firstname from personcore")
	
	response.write "Go!" & vbNewLine
	while not rs.eof
		response.write rs("firstname") & vbNewLine
		rs.movenext
	wend
	response.write "Done!" & vbNewLine
%>
