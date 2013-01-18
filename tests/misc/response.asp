<%
	response.buffer = true
	response.write response.buffer
	response.flush
	response.write "biggity"
	response.flush
	response.write "bammity"
	response.clear

	response.write "hello"
	response.write "baby"
	response.write 3
	response.write true

	response.end
	response.write "tada!"
%>
