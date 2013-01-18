<%
	'set conn = Server.CreateObject("ADODB.Connection")
	set conn = new ADODB.Connection
	conn.open "Provider=SQLOLEDB;Server=DBDEV;Database=Services2001;uid=sa;pwd=1.Carol"

	set rs = conn.execute("select object_id('services2001..personcore') as hello)")
	
	response.write "Go!" & vbNewLine
	response.write rs("hello")
	response.write "Done!" & vbNewLine
%>
