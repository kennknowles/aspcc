<%
	dim zzz
	zzz = 5
	sub dance
		response.write "dance" & vbNewLine
	end sub

	sub gonuts( testvalue )
		response.write testvalue & "-" & testvalue & vbNewLine
	end sub

	dance
	dance

	gonuts "babycakes"
	call gonuts( "testval" )

	dim x
	sub print_global
		response.write "x = " & x & vbNewLine
	end sub

	x = 5
	print_global
	x = 3
	print_global
%>

<%
	dim y
	sub modify_global(value)
		y = value
	end sub

	y = 3
	response.write "y = " & y & vbNewLine
	modify_global 6
	response.write "y = " & y & vbNewLine

%>
