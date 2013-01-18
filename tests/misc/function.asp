<%
	function square(num)
		square = num * num
	end function

	function gonuts( testvalue )
		gonuts = testvalue & "-" & testvalue & vbNewLine
	end function

	function nuttin
		nuttin = "blargly"
	end function

	response.write square(3) & vbNewLine
	response.write square(2) & vbNewLine

	response.write gonuts("babycakes")
	call gonuts( "testval" )

	response.write nuttin
%>
