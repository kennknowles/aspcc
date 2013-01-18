<%
	function recmult(num1, num2)
		if num1 = 1 then
			recmult = num2
		else
			recmult = recmult(num1-1, num2) + num2
		end if
	end function

	function print_up( target )
		if target > 0 then
			print_up target - 1
			response.write target & " "
		end if
	end function

	response.write recmult(1,4) & vbNewLine
	response.write recmult(3,5)
	response.write vbNewLine
	print_up 100
%>
