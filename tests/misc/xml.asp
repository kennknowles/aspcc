<%
	file = "test.xml"

	set objXML = Server.CreateObject("Microsoft.XMLDOM")

	response.write "Loading XML from: " & Server.MapPath(File) & vbNewLine
	objXML.Load(Server.MapPath(File))

	set xmlNodes = objXML.GetElementsByTagName("Section")

	for each node in xmlNodes
		response.write node.NodeTypeString
		response.write node.GetAttribute("Label") & ":" & node.text & "<br />"
	next
%>
