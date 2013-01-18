<%
	for i = 1 to 10
		select case i
		case 1
			response.write "one"
		case 2
			response.write "two"
		case 3
			response.write "three"
		case else
			response.write "greater than three"
		end select
		response.write vbNewLine
	next

%>
