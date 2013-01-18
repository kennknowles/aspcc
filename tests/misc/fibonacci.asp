<%
	function fib(x)
		if x < 2 then
			fib = x
		else
			fib = fib(x - 2) + fib(x - 1)
		end if
	end function

	response.buffer = false
	'response.write timer() & vbNewLine
	for i = 1 to 15
		response.write fib(i) & vbNewLine
	next
	'response.write timer() & vbNewLine
%>
