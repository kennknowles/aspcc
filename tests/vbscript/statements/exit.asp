<%
	i = 1
	while i < 100
		response.write i & vbNewLine
		if i > 27 then exit while
		i = i + 1
	wend

	for z = 1 to 10
		response.write z * z & vbNewLine
		if z = 7 then exit for
	next
	
	sub exittest
		response.write "yes"
		exit sub
		response.write "no"
	end sub

	function testfunc( value )
		testfunc = 4
		exit function
		testfunc = 1
	end function

	exittest
	
	response.write testfunc(9)
%>
